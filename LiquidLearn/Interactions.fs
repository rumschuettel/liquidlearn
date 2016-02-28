// LiquidLearn (c) 2016 Johannes Bausch
module LiquidLearn.Interactions

open Microsoft.Research
open Liquid.Operations // for >< and !! operator

open Utils

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



// create sets of gates used in the training algorithms
(*
Remark: it seems like the gates used in the simulator are exponentiated Hamiltonian terms, i.e. for a coupling H,
we need to specify U(t) = exp(i H t), where t = theta.
That way our gates are actually unitary and everything works out.

The trotter number seems to be a parameter that controls how fine-grained the subdivision is, i.e. it expands
exp(i (A + B) t) = (exp(i A t/T)exp(i B t/T)^T for some trotter number T.
Hence the parameter T and theta has to make the coupling "smaller" in the sense that it applies the coupling multiple times.
*)
open System

module Matrices =
    // some standard matrices
    let PauliX = Liquid.CSMat(2, [0, 1, 1.0, 0.0; 1, 0, 1.0, 0.0])
    let PauliY = Liquid.CSMat(2, [0, 1, 0.0, -1.0; 1, 0, 0.0, 1.0])
    let PauliZ = Liquid.CSMat(2, [0, 0, 1.0, 0.0; 1, 1, -1.0, 0.0])
    let Identity = Liquid.CSMat(2, [0, 0, 1.0, 0.0; 1, 1, 1.0, 0.0])
    let IdentityN n = Liquid.CSMat(n, [ for i in 0..n-1 -> i, i, 1.0, 0.0 ])

open Matrices

let Projector (states : (float * Data.DataT) list) (theta : float) (qs : Liquid.Qubits) =
    // state2pos (Parse "011") = 6
    let state2pos (signature : Data.DataT) = 
        [ for i in 0..signature.Length-1 -> if signature.[i] = Data.BitT.One then pown 2 i else 0 ] |> List.sum

    // matrix size
    let count = snd(states.[0]).Length
    let size = pown 2 count
    // create diagonal
    let entries = states ||> (fun arg -> (state2pos (snd arg), arg)) |> Map.ofList
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
        Name = "projector",
        Draw = (join "" [ for i in 0..qs.Length-1 -> sprintf "\\cwx[#%d]\\go[#%d]\\gate{Train}" i i ]),
        Help = sprintf "custom projector onto %A" states,
        Mat  = Liquid.CSMat(size, diagonal)
    )).Run qs

let MatrixInteraction name (matrix : float -> Liquid.CSMat) theta (qs : Liquid.Qubits) =
    (new Liquid.Gate(
        Name = name,
        Draw = (join "" [ for i in 0..qs.Length-1 -> sprintf "\\cwx[#%d]\\go[#%d]\\gate{%s}" i i name ]),
        Mat  = matrix theta
    )).Run qs


// like CgateNC, but for a qudit control; we assume all matrices are the same size
let ControlledInteraction (matrices : (float -> Liquid.CSMat) list) theta (qs : Liquid.Qubits) =
    // size of single matrix block
    let singleMatrixSize = (matrices.[0] 0.0).Length
    // size of block of matrices to control
    let usefulMatrixSize = matrices.Length * singleMatrixSize
    // next power of 2, need at least one identity matrix, hence the +1
    let nextPowerOf2 = nextPowerOf2 usefulMatrixSize

    let identity = IdentityN (nextPowerOf2 - usefulMatrixSize)
    let matrices = (fun matrix -> matrix theta) <|| matrices

    // direct sum all matrices to 1 + m1+m2+...+mn
    let matrixSum = matrices |> List.fold (fun (bigmatrix : Liquid.CSMat) matrix -> bigmatrix.Plus matrix) identity

    (new Liquid.Gate(
        Name = "foo",
        Mat = matrixSum
    )).Run qs

// some interaction sets
module Sets =
    type InteractionT = (float -> Liquid.Qubits -> unit)

    // interpret measurement result on controlled interaction
    let InterpretControlMeasurement (results : float list) (control : Graph.VertexT) =
        let interactions = control.Unwrap.interactions
        let nextPowerOf2 = nextPowerOf2 interactions.Length
        let controlCount = (nextPowerOf2 - interactions.Length)
        // sum control identity probabilities
        let control =
            match results |> List.take controlCount |> List.sum with
            | 0.0 -> 1.0
            | v -> v

        dump results
        let results = results |> List.skip controlCount |> normalize ||> ( fun v -> v / control )        
        List.zip interactions results

    // Interaction base class
    [<AbstractClass>]
    type InteractionFactory() =
        abstract member Names : int -> string list
        abstract member Matrix : string -> (float -> Liquid.CSMat)

        member this.ListPossibleInteractions (edge : Graph.EdgeT) =
            this.Names edge.Size

        member this.Interaction name =
            MatrixInteraction name (this.Matrix name)

        member this.ControlledInteraction (edge : Graph.EdgeT) =
            let matrices = [ for name in edge.GetControl.Unwrap.interactions -> this.Matrix name ]
            ControlledInteraction matrices

    // Pauli matrix products
    type Paulis() =
        inherit InteractionFactory()

        override this.Names n = [ for id in tuples ['0'; '1'; '2'; '3'] n -> string (new System.String(Seq.toArray id)) ]
        override this.Matrix id = GeneratedCode.PauliProductMatrices.Get id

    // Projectors
    type Projectors() =
        inherit InteractionFactory()

        override this.Names n = [ for id in tuples ['0'; '1'] n -> string (new System.String(Seq.toArray id)) ]
        override this.Matrix id = GeneratedCode.Rank1ProjectionMatrices.Get id
