// LiquidLearn (c) 2016 Johannes Bausch
module LiquidLearn.Interactions

open Microsoft.Research
open Liquid.Operations // for >< and !! operator

open Utils
open System

module Matrices =
    // some standard matrices
    let PauliX = Liquid.CSMat(2, [0, 1, 1.0, 0.0; 1, 0, 1.0, 0.0])
    let PauliY = Liquid.CSMat(2, [0, 1, 0.0, -1.0; 1, 0, 0.0, 1.0])
    let PauliZ = Liquid.CSMat(2, [0, 0, 1.0, 0.0; 1, 1, -1.0, 0.0])
    let Identity = Liquid.CSMat(2, [0, 0, 1.0, 0.0; 1, 1, 1.0, 0.0])
    let IdentityN n = Liquid.CSMat(n, [ for i in 0..n-1 -> i, i, 1.0, 0.0 ])
        

type Liquid.CSMat with
    // direct sum
    member this.Plus (other : Liquid.CSMat) =
        let totalSize = this.Length + other.Length
        let thisEntries = [
            for i in 0..this.Length-1 do
                for j in 0..this.Length-1 do
                    let entry = this.Item (i, j)
                    yield (i, j, entry.r, entry.i)
        ]
        let otherEntries = [
            for i in 0..other.Length-1 do
                for j in 0..other.Length-1 do
                    let entry = other.Item (i, j)
                    yield (i + this.Length, j + this.Length, entry.r, entry.i)
        ]
        
        Liquid.CSMat(totalSize, List.concat [thisEntries; otherEntries])

    // basis permutation helpers
    // Sig2State (FromString "011") = 3
    static member Sig2State (signature : Data.DataT) = 
        let signature = signature |> List.rev
        [ for i in 0..signature.Length-1 -> if signature.[i] = Data.BitT.One then pown 2 i else 0 ] |> List.sum

    // |001><010|
    static member Rank1Projector dim (sigA, sigB) =
        let stateA, stateB = (sigA, sigB) ||> Liquid.CSMat.Sig2State
        Liquid.CSMat(dim, [(stateA, stateB, 1., 0.)])

    static member RankNProjector dim (sigs : (Data.StateT * Data.StateT) list) =
        sigs
            ||> Liquid.CSMat.Rank1Projector dim
            ||> fun m -> m.Dense()
            |> List.reduce (fun acc m -> acc.Add(m); acc)
            |> Liquid.CSMat

    static member BasisPermutationMatrix permutation systems =
        let basis = [Data.BitT.Zero; Data.BitT.One] |> tuples systems
        let permutedBasis = basis ||> List.permute permutation

        Liquid.CSMat.RankNProjector (pown 2 systems) (List.zip basis permutedBasis)

    // permutation of basis
    member this.PermuteBasis permutation =
        if (log2 this.Length) % 1.0 <> 0.0 then failwith "not a power of 2 matrix size"
        let systems = log2 this.Length |> round |> int
        let permutationMatrix = Liquid.CSMat.BasisPermutationMatrix permutation systems

        permutationMatrix.Adj().Mul(this).Mul(permutationMatrix)

    // take a matrix and act with it on qubits specified by 1s in a list, i.e.
    // M on [0; 1; 1; 0; 0] will be transformed to 1 otimes M otimes 1^3
    member this.SpreadTo (signature : Data.StateT) =
        if (log2 this.Length) % 1.0 <> 0.0 then failwith "not a power of 2 matrix size"
        let systems = log2 this.Length |> round |> int
        let other = signature.Length - systems
        match other with
        | n when n < 0 -> failwith "cannot spread matrix to fewer qubits"
        | 0 -> this
        | n ->
            let bigmatrix = this.Kron (pown 2 (signature.Length - systems))
            let orig = [for i in 1..systems -> Data.BitT.One ] @ [for i in 1..n -> Data.BitT.Zero ]
            bigmatrix.PermuteBasis (getPermutation orig signature)


// Training Gates

// projector gate
let Projector (states : (float * Data.DataT) list) (theta : float) (qs : Liquid.Qubits) =
    // matrix size
    let count = snd(states.[0]).Length
    let size = pown 2 count
    // create diagonal
    let entries = states ||> (fun arg -> (Liquid.CSMat.Sig2State (snd arg), arg)) |> Map.ofList
    let diagonal =
        [
            for i in 0..size-1 ->
                if entries.ContainsKey i then
                    let (strength, state) = entries.[i]
                    (i, i, Math.Cos(strength * theta), Math.Sin(strength * theta))
                else
                    (i, i, 1.0, 0.0)
        ]

    // create new liquid gate and run
    (new Liquid.Gate(
        Name = "Projector",
        Draw = (join "" [ for i in 0..qs.Length-1 -> sprintf "\\cwx[#%d]\\go[#%d]\\gate{Train}" i i ]),
        Help = sprintf "custom projector onto %A" states,
        Mat  = Liquid.CSMat(size, diagonal)
    )).Run qs

// gate from matrix
let MatrixInteraction name (matrix : float -> Liquid.CSMat) theta (qs : Liquid.Qubits) =
    (new Liquid.Gate(
        Name = name,
        Draw = sprintf "\\multigate{#%d}{%s}" (qs.Length-1) name,
        Mat  = matrix theta
    )).Run qs


// like CgateNC, but for a qudit control; we assume all matrices are the same size
let ControlledInteraction name (matrices : (float -> Liquid.CSMat) list) theta (qs : Liquid.Qubits) =
    // size of single matrix block
    let singleMatrixSize = (matrices.[0] 0.0).Length
    // size of block of matrices to control
    let usefulMatrixSize = matrices.Length * singleMatrixSize
    // next power of 2, need at least one identity matrix, hence the +1
    let totalSize = nextPowerOf2 (usefulMatrixSize + singleMatrixSize)

    let identity = Matrices.IdentityN (totalSize - usefulMatrixSize)
    let matrices = (fun matrix -> matrix theta) <|| matrices

    // direct sum all matrices to 1 + m1+m2+...+mn
    let matrixSum = matrices |> List.fold (fun (bigmatrix : Liquid.CSMat) matrix -> bigmatrix.Plus matrix) identity


    (new Liquid.Gate(
        Name = "QuditControl",
        Draw = (
            let numberOfControls = nextPowerOf2 (matrices.Length + 1) |> log2 |> round |> int
            let numberOfQubits = singleMatrixSize |> log2 |> round |> int
            let controlString = join "" [ for i in 0..numberOfControls-1 -> sprintf "\\qwx[#%d]\\go[#%d]\\control" i i ]
            controlString + (sprintf "\qwx[#%d]\\go[#%d]\\multigate{#%d}{%s}" (numberOfControls+numberOfQubits-1) numberOfControls (numberOfControls+numberOfQubits-1) name )
        ),
        Mat = matrixSum
    )).Run qs

// some interaction sets
module Sets =
    type InteractionT = (float -> Liquid.Qubits -> unit)

    // Interaction base class
    [<AbstractClass>]
    [<StructuredFormatDisplay("{ShortForm}")>]
    type IInteractionFactory() =
        abstract member ShortForm : string
        // give back gate name for a list of interactions
        abstract member GateName : Graph.InteractionT list -> string
        // give a complete list of possible interactions * number of qubits it acts on
        abstract member Names : int -> (string * int) list
        // for any id returned by Names, this function has to return a valid matrix
        // the size of the matrix has to be n qubits, where n is given to Names to yield the id.
        abstract member Matrix : string -> (float -> Liquid.CSMat)



        // for every edge, pass a list of interactions and the vertices within that edge
        // that the interaction is acting on nontrivially
        // the locality given for every name has to equal the size of the matrix returned by Matrix name
        member this.ListPossibleInteractions (edge : Graph.EdgeT) =
            let whole = edge.StripControl.NormalOrder
            let subsystems = edge.Subsets
            [ for name, locality in this.Names edge.Size ->
                [ for vertices in subsystems.[locality] ->
                  {
                    Graph.InteractionT.name = name
                    Graph.InteractionT.vertices = vertices
                } ]
            ] |> List.concat

        // simply gives back one matrix interaction for an interaction name
        member this.Interaction (interaction : Graph.InteractionT) =
            MatrixInteraction (sprintf "%s on %A" interaction.name interaction.vertices) (this.Matrix interaction.name)

        // gives back a big controlled interaction matrix for all the interactions in the edge.control.interactions list
        member this.ControlledInteraction (edge : Graph.EdgeT) =
            let whole = edge.StripControl.NormalOrder
            let interactions = edge.GetControl.Unwrap.interactions
            let matrices =
                [ for interaction in interactions ->
                    // if interaction.vertices = 1; 2 and whole = 0; 1; 2; 3; 4 then signature = 01100
                    let signature = [ for v in whole -> if contains v interaction.vertices then Data.BitT.One else Data.BitT.Zero ]
                    let matrix = this.Matrix interaction.name
                    fun theta -> (matrix theta).SpreadTo signature
                ]
            ControlledInteraction (this.GateName interactions) matrices

    // Pauli matrix products
    type Paulis() =
        inherit IInteractionFactory()
        override this.ShortForm = "Paulis"

        override this.GateName list = sprintf "%d Paulis" list.Length

        override this.Names n = [ for name in tuples n ['1'; '2'] -> string (new System.String(Seq.toArray name)), n ]
        override this.Matrix name = GeneratedCode.PauliProductMatrices.Get name

    // Projectors
    type Projectors() =
        inherit IInteractionFactory()

        override this.ShortForm = "Projectors"

        override this.GateName list = sprintf "%d Projectors" list.Length

        override this.Names n = [ for name in tuples n ['0'; '1'] -> string (new System.String(Seq.toArray name)), n ]
        override this.Matrix name = GeneratedCode.Rank1ProjectionMatrices.Get name

    // Compressed Random Matrices
    // set of 5 random sparse Hermitian matrices
    // These are 2-local 3-qubit interactions or 2-local 2-qubit
    type Random() =
        inherit IInteractionFactory()

        override this.ShortForm = "Random"

        override this.GateName list = sprintf "%d Random" list.Length

        override this.Names n =
            match n with
            | n when n >= 2 -> GeneratedCode.SparseRandomHermitian.List ||> fun name -> name, 2
            | _ -> failwith "History State matrices are 2-local"

        override this.Matrix name = GeneratedCode.SparseRandomHermitian.Get name

    // History State Matrices
    // set of 5 special matrices from History state constructions, all 2-local
    type History() =
        inherit IInteractionFactory()

        override this.ShortForm = "History"

        override this.GateName list = sprintf "%d History" list.Length

        override this.Names n =
            match n with
            | n when n >= 2 -> GeneratedCode.UniversalHistory.List ||> fun name -> name, 2
            | _ -> failwith "History State matrices are 2-local"

        override this.Matrix name = GeneratedCode.UniversalHistory.Get name
