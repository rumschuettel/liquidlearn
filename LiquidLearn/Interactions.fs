// LiquidLearn (c) 2016 Johannes Bausch
module LiquidLearn.Interactions

open Microsoft.Research
open Liquid.Operations // for >< and !! operator

open Utils


// create sets of gates used in the training algorithms
(*
Remark: it seems like the gates used in the simulator are exponentiated Hamiltonian terms, i.e. for a coupling H,
we need to specify U(t) = exp(i H t), where t = theta.
That way our gates are actually unitary and everything works out.

The trotter number seems to be a parameter that controls how fine-grained the subdivision is, i.e. it expands
exp(i (A + B) t) = (exp(i A t/T)exp(i B t/T)^T for some trotter number T.
Hence the parameter T and theta has to make the coupling "smaller" in the sense that it applies the coupling multiple times.
*)
type InteractionT = (float -> Liquid.Qubits -> unit)

module Matrices =
    // some standard matrices
    let PauliX = Liquid.CSMat(2, [0, 1, 1.0, 0.0; 1, 0, 1.0, 0.0])
    let PauliY = Liquid.CSMat(2, [0, 1, 0.0, -1.0; 1, 0, 0.0, 1.0])
    let PauliZ = Liquid.CSMat(2, [0, 0, 1.0, 0.0; 1, 1, -1.0, 0.0])
    let Identity = Liquid.CSMat(2, [0, 0, 1.0, 0.0; 1, 1, 1.0, 0.0])
    let IdentityN n = Liquid.CSMat(n, [ for i in 0..n-1 -> i, i, 1.0, 0.0 ])

    // kronecker product for a list of matrices, or numbers, overloaded
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


let Controlled (interaction: InteractionT) theta qs =
    Liquid.Operations.CgateNC (interaction theta) qs

// some interaction sets
module Sets =
    type SetT = 
        | Names of string list
        | Interaction of string * (float -> Liquid.CSMat)
 
    // If an edge without matching control is given, a complete set of interaction names is returned.
    // If an edge with control is given, a matching interaction is returned.

    // all Pauli interactions.
    let FullPauli (edge : Graph.EdgeT) : SetT =
        // create names list 00, 01, 02, 03, 10, ..., 23, 30, 31, 32, 33
        let names n = [ for id in tuples ['0'; '1'; '3'] n -> string (new System.String(Seq.toArray id)) ]
        let namesWithoutControl = names (edge.Length - 1)

        // try to find a vertex C("001", ...) and return control identifier
        match (List.tryPick (fun vertex ->
            match vertex with
            | Graph.VertexT.C(name, _) when contains name namesWithoutControl -> Some name
            | _ -> None
        ) edge) with
        | Some name -> SetT.Interaction (name, (GeneratedCode.PauliProductMatrices.Get name))
        | _ -> SetT.Names (names edge.Length)

    // all Projector interactions
    let FullProjectors (edge : Graph.EdgeT) : SetT =
        // create names list 00, 01, 10, 11
        let names n = [ for id in tuples ['0'; '1'] n -> string (new System.String(Seq.toArray id)) ]
        let namesWithoutControl = names (edge.Length - 1)

        // try to find a vertex C("01", ...) and return control identifier
        match (List.tryPick (fun vertex ->
            match vertex with
            | Graph.VertexT.C(name, _) when contains name namesWithoutControl -> Some name
            | _ -> None
        ) edge) with
        | Some name -> SetT.Interaction (name, (GeneratedCode.Rank1ProjectionMatrices.Get name))
        | _ -> SetT.Names (names edge.Length)