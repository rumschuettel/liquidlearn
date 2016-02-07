// LiquidLearn (c) 2016 Johannes Bausch
module LiquidLearn.Train

open Microsoft.Research
open Liquid.Operations // for >< and !! operator

open LiquidLearn.Utils


// create sets of gates used in the training algorithms
(*
Remark: it seems like the gates used in the simulator are exponentiated Hamiltonian terms, i.e. for a coupling H,
we need to specify U(t) = exp(i H t), where t = theta.
That way our gates are actually unitary and everything works out.

The trotter number seems to be a parameter that controls how fine-grained the subdivision is, i.e. it expands
exp(i (A + B) t) = (exp(i A t/T)exp(i B t/T)^T for some trotter number T.
Hence the parameter T and theta has to make the coupling "smaller" in the sense that it applies the coupling multiple times.
*)
module Interactions =
    module Matrices =
        // some standard matrices
        let PauliX = Liquid.CSMat(2, [0, 1, 1.0, 0.0; 1, 0, 1.0, 0.0])
        let PauliY = Liquid.CSMat(2, [0, 1, 0.0, -1.0; 1, 0, 0.0, 1.0])
        let PauliZ = Liquid.CSMat(2, [0, 0, 1.0, 0.0; 1, 1, -1.0, 0.0])
        let Identity = Liquid.CSMat(2, [0, 0, 1.0, 0.0; 1, 1, 1.0, 0.0])
        let IdentityN n = Liquid.CSMat(n, [ for i in 0..n-1 -> i, i, 1.0, 0.0 ])

        // kronecker product for a list of matrices, overloaded
        type OuterT = OuterT with
            static member ($) (OuterT, list : Liquid.CSMat list) =
                List.fold (fun (matl : Liquid.CSMat) (matr : Liquid.CSMat) -> matl.Kron matr) list.Head list.Tail
            static member ($) (OuterT, arg : Liquid.CSMat * int) =
                let matrix, exponent = arg
                OuterT $ [ for i in 1..exponent -> matrix ]
        
        let inline Outer arg : Liquid.CSMat = OuterT $ arg
            

    open Matrices
    open System

    let Projector (states : (float * Data.DataT) list) (theta : float) (qs : Liquid.Qubits) =
        // state2pos (Parse "011") = 6
        let state2pos (signature : Data.DataT) = 
            [ for i in 0..signature.Length-1 -> if signature.[i] = Data.BitT.One then pown 2 i else 0 ] |> List.sum

        let gate (theta : float) = 
            // matrix size
            let count = snd(states.[0]).Length
            let size = pown 2 count
            // create diagonal
            let entries = (fun arg -> (state2pos (snd arg), arg)) /@ states |> Map.ofList
            let diagonal =
                [
                    for i in 0..size-1 ->
                        if entries.ContainsKey i then
                            let (strength, state) = entries.[i]
                            (i, i, Math.Cos(strength * theta), Math.Sin(strength * theta))
                        else
                            (i, i, 1.0, 0.0)
                ]
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
                Mat         = Outer(PauliX, qs.Length)
            )

        (gate theta).Run qs


    let CX (theta : float) (qs : Liquid.Qubits) =
        Liquid.Operations.Cgate (X theta) qs


// controlled gate trainer
module ControlledTrainer =
    open Graph
    open Data

    let MeasurementStatistics (ket : Liquid.Ket) (qubits : int list) =
        ket.Probs !!(ket.Qubits, qubits)

    let Train (graph : Hypergraph) (data : Dataset) =
        let controlled_graph = graph.ControlGraph
        let projector = Interactions.Projector (data.ValuatedList 1.0)
        let terms =
            [
                // projector on training data
                Liquid.SpinTerm(1, projector, controlled_graph.Outputs, 1.0)
            ] @ [
                // training interactions
(*                for interaction in controlled_graph.Interactions do
                    if contains interaction.[0] controlled_graph.Controls then
                        yield Liquid.SpinTerm(1, Interactions.CX, interaction, 4.0)*)
            ]
        let spin = Liquid.Spin(terms, controlled_graph.Size)

        Liquid.Spin.Test(
            tag = "training",
            repeats = 20,
            trotter = 10,
            schedule = [
                0,     [|1.0; 0.0; 0.0|];
                100,   [|0.0; 1.0; 1.0|]
            ],
            res = 5,
            spin = spin,
            runonce = false,
            decohereModel = []
        )
        
        // extract trained controls
        //dump !!(spin.Ket.Qubits, controlled_graph.Controls)
        //let controls = MeasurementStatistics spin.Ket controlled_graph.Controls
        //dump controls
        
        ()