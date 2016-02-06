// LiquidLearn (c) 2016 Johannes Bausch
module LiquidLearn.Train

open Microsoft.Research
open Liquid.Operations // for >< and !! operator

open LiquidLearn.Utils


// create sets of gates used in the training algorithms
module Gates =
    module Matrices =
        // some standard matrices
        let PauliX = Liquid.CSMat(2, [0, 1, 1.0, 0.0; 1, 0, 1.0, 0.0])
        let PauliY = Liquid.CSMat(2, [0, 1, 0.0, -1.0; 1, 0, 0.0, 1.0])
        let PauliZ = Liquid.CSMat(2, [0, 0, 1.0, 0.0; 1, 1, -1.0, 0.0])
        let Identity = Liquid.CSMat(2, [0, 0, 1.0, 0.0; 1, 1, 1.0, 0.0])
        let IdentityN n = Liquid.CSMat(n, [ for i in 0..n-1 -> i, i, 1.0, 0.0 ])

        // kronecker product for a list of matrices
        let Outer (list : Liquid.CSMat list) =
            List.fold (fun (matl : Liquid.CSMat) (matr : Liquid.CSMat) -> matl.Kron matr) list.Head list.Tail

        // overload tensor power multiplication
        type OperatorExtension = OperatorExtension with
            static member (?*) (OperatorExtension, (a : Liquid.CSMat, n : int)) =
                match n with
                | 0 -> IdentityN a.Length
                | _ -> Outer [ for i in 1..n -> a ]
            static member inline (?*) (OperatorExtension, (a: 'a, b: 'b)) =
                a * b : 'a

        let inline (*) a b = (OperatorExtension ?* (a, b))

            

    open Matrices

    let Projector (states : Data.DataT list) (theta : float) (qs : Liquid.Qubits) =
        let state2pos (signature : Data.DataT) = 
            [ for i in 0..signature.Length-1 -> if signature.[i] = Data.BitT.One then pown 2 i else 0 ] |> List.sum

        let gate (theta : float) = 
            // matrix size
            let size = pown 2 states.[0].Length
            // mark diagonal entries
            let entries = state2pos /@ states
            // create diagonal
            let diagonal = [ for i in 0..size-1 -> if contains i entries then (i, i, 1.0 + theta, 0.0) else (i, i, 1.0, 0.0) ]
            // create new liquid gate
            new Liquid.Gate(
                Name        = "projector",
                Draw        = (join "" [ for i in 0..qs.Length-1 -> sprintf "\\cwx[#%d]\\go[#%d]\\gate{Train}" i i ]),
                Help        = sprintf "custom projector onto %A" states,
                Mat         = Liquid.CSMat(size, diagonal)
            )
                
        (gate theta).Run qs

    let X (theta : float) (qs : Liquid.Qubits) =
        let gate (theta : float) =
            new Liquid.Gate(
                Name        = "multi X",
                Draw        = (join "" [ for i in 0..qs.Length-1 -> sprintf "\\qwx[#%d]\\go[#%d]\\gate{X}" i i ]),
                Mat         = Matrices.PauliX * qs.Length
            )

        (gate theta).Run qs



    let CX (theta : float) (qs : Liquid.Qubits) =
        (PauliX * 0).DumpDense()
        Liquid.Operations.Cgate (X theta) qs


// controlled gate trainer
module ControlledTrainer =
    open Graph
    open Data

    let MeasurementStatistics (ket : Liquid.Ket) (qubits : int list) =
        ket.Probs !!(ket.Qubits, qubits)

    let Train (graph : Hypergraph) (data : Dataset) =
        let controlled_graph = graph.ControlGraph
        let projector = Gates.Projector (data.No @ (Flip /@ data.Yes))
        let terms =
            [
                // projector on training data
                Liquid.SpinTerm(1, projector, controlled_graph.Outputs, 1.0)
            ] @ [
                // training interactions
                for interaction in controlled_graph.Interactions do
                    if contains interaction.[0] controlled_graph.Controls then
                        yield Liquid.SpinTerm(1, Gates.CX, interaction, 1.0)
            ]
        let spin = Liquid.Spin(terms, controlled_graph.Size)

        Liquid.Spin.Test(
            tag = "training",
            repeats = 20,
            trotter = 4,
            schedule = [
                0,     [|1.0; 0.0; 0.0|];
                100,   [|0.0; 1.0; 1.0|]
            ],
            res = 10,
            spin = spin,
            runonce = true,
            decohereModel = []
        )

        // extract trained controls
        dump !!(spin.Ket.Qubits, controlled_graph.Controls)
        let controls = MeasurementStatistics spin.Ket controlled_graph.Controls
        dump controls
        
        ()