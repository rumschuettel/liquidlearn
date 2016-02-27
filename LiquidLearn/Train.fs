// LiquidLearn (c) 2016 Johannes Bausch
module LiquidLearn.Train

open Microsoft.Research
open Liquid.Operations // for >< and !! operator

open Interactions
open Utils
open Data
open Graph

// extract measurement statistics from a list of qubits
let MeasurementStatistics (ket : Liquid.Ket) (qubits : int list) =
    ket.Probs !!(ket.Qubits, qubits) |> Array.toList

type TestResults = {
        YesStats : (float*float) list
        NoStats  : (float*float) list
}  with
    override this.ToString() =
        join "\n" (
            [ for (e, sigma) in this.YesStats -> sprintf "y\t%.2e\t%.2e" e sigma ]
            @
            [ for (e, sigma) in this.NoStats -> sprintf "n\t%.2e\t%.2e" e sigma ]
        )
    member this.ToFile filename =
        System.IO.File.WriteAllText(filename, string this)
        
// simple controlled gate trainer
type SimpleControlledTrainer (graph : Graph.Hypergraph, interactions : Graph.EdgeT -> Interactions.Sets.SetT, ?trainOnQudits : bool) = class
    let trainOnQudits = defaultArg trainOnQudits false

    // create controlled version of graph
    let controlled_graph = graph.ControlGraph ( fun edge ->
        match interactions edge with
        | Interactions.Sets.SetT.Names names -> names
        | _ -> failwith "interactions should be just a list of names at this point"
    )
    do dump controlled_graph

    // interactions to train
    let controlled_interactions = [
        for edge in controlled_graph.Edges do
            match interactions edge with
            | Interactions.Sets.SetT.Interaction (name, h) -> yield Liquid.SpinTerm(2, Interactions.Controlled (Interactions.MatrixInteraction name h), controlled_graph.whichQubit /@ edge, 1.0)
            | _ -> ()
    ]

    let mutable parameters = None
    member this.Train (data : Dataset) =
        // run training once for YES and once for NO-instances
        let res =
            ( fun (tag, data : Dataset, weight) ->
                // projector on training data. Interaction strength scaled to be dominating term
                let projector = Liquid.SpinTerm(1, Interactions.Projector (data.ValuatedList weight), controlled_graph.Outputs, 10. * (float controlled_graph.Size))

                // create spin model and train
                let spin = Liquid.Spin(projector :: controlled_interactions, controlled_graph.Size, Liquid.RunMode.Trotter1X)
                Liquid.Spin.Test(
                    tag = "train " + tag,
                    repeats = 20,
                    trotter = 20,
                    schedule = [
                        0,     [|1.00; 0.00; 0.00|];
                        50,   [|1.00; 0.25; 0.00|];
                        200,  [|0.00; 1.00; 1.00|]
                    ],
                    res = 50,
                    spin = spin,
                    runonce = true,
                    decohereModel = []
                )

                // extract trained control probabilities
                [ for control in controlled_graph.Controls -> MeasurementStatistics spin.Ket [controlled_graph.whichQubit control] ]

            ) /@ [("YES", data.YesInstances, -1.0); ("NO", data.NoInstances, 1.0)]

        // combine training results: extract control weights
        let yesWeights = (fun (prob : float list) -> prob.[0]) /@ res.[0]
        let noWeights = (fun (prob : float list) -> prob.[1]) /@ res.[1]
        let weights = 
            [ for i in 0..yesWeights.Length-1 -> yesWeights.[i] + noWeights.[i] ]
            |>
            // normalize to -1 to 1
            (fun weights ->
                let min, max = List.min weights, List.max weights
                [ for weight in weights ->
                    if max <> min then
                        (weight - min) / (max - min) * 2. - 1.
                    else
                        0.
                ]
            )
        
        parameters <- Some((List.zip controlled_graph.Controls weights) |> Map.ofList)
        dump parameters
        
        this
 
    member this.Test (data : Dataset) =
        match parameters with
        | None -> failwith "model not trained"
        | Some parameters ->
            // build interactions from previously trained model
            let interactions = [
                for edge in graph.Edges do
                  match interactions edge with
                  | Interactions.Sets.SetT.Names names ->
                      yield! [
                        for name in names ->
                            // dummy control vertex
                            let control = Graph.C(name, edge)
                            // get interaction for edge
                            let interaction = (
                                // interaction function on dummy edge with the right control vertex telling the interaction function which interaction to return
                                match (interactions (control --- edge)) with
                                | Interactions.Sets.SetT.Interaction (name, h) -> h
                                | _ -> failwith "interaction function should return a valid interaction for given name"
                            )
                            let strength = parameters.[control]
                            Liquid.SpinTerm(1, Interactions.MatrixInteraction name interaction, graph.whichQubit /@ edge, -strength)
                      ]
                  | _ -> ()
            ]

            dump interactions

            let spin = Liquid.Spin(interactions, graph.Size, Liquid.RunMode.Trotter1X)
            Liquid.Spin.Test(
                tag = "test",
                repeats = 20,
                trotter = 20,
                schedule = [
                    0,    [|1.0; 0.0|];
                    200,  [|0.0; 1.0|]
                ],
                res = 50,
                spin = spin,
                runonce = true,
                decohereModel = []
            )

            let state = Liquid.Ket(graph.Size)
            let qubits = state.Qubits
            let outputs = graph.Outputs // list of qubits that act as output qubits
 
            // set part of state vector that is not output to |0> + |1>, so that we trace out the hidden part
            [ for i in 0..graph.Size-1 -> i ] |> List.map (fun i ->
                if not (contains i outputs) then
                    qubits.[i].StateSet(1., 0., 1., 0.)
            ) |> ignore
            state.Dump Liquid.Util.showInd

            let updateStateData (data : DataT) =
                let data = List.rev data // TODO check order of data and why we have to flip it around here
                [ for i in 0..outputs.Length-1 -> i ] |> List.map (fun i ->
                    match data.[i] with
                    | BitT.Zero _ -> qubits.[outputs.[i]].StateSet(Liquid.Zero)
                    | BitT.One _ -> qubits.[outputs.[i]].StateSet(Liquid.One)
                    | _ -> failwith "invalid bit type"
                ) |> ignore
            
            // build statistics
            let yesStats = [
                for yes in data.Yes -> 
                    updateStateData yes
                    spin.EnergyExpectation( stdev = true, qubits = state.Qubits )
            ]
            let noStats = [
                for no in data.No -> 
                    updateStateData no
                    spin.EnergyExpectation( stdev = true, qubits = state.Qubits )
            ]

            { YesStats = yesStats; NoStats = noStats }

        
        // extract trained controls
        //dump !!(spin.Ket.Qubits, controlled_graph.Controls)
        //let controls = MeasurementStatistics spin.Ket controlled_graph.Controls
        //dump controls
end