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
    ket.Probs !!(ket.Qubits, List.rev qubits) |> Array.toList

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
type SimpleControlledTrainer (graph : Graph.Hypergraph, interactions : Interactions.Sets.InteractionFactory, ?trainOnQudits : bool) = class
    let trainOnQudits = defaultArg trainOnQudits true

    // create controlled version of graph
    let trainingGraph = graph.AddControls ( fun edge ->
        if trainOnQudits then
            [{
                id = randomString 5;
                interactions = interactions.ListPossibleInteractions edge
            }]
        else
            [ for interaction in interactions.ListPossibleInteractions edge ->
              {
                id = randomString 5;
                interactions = [interaction]
            }]
    )
    do 
        dump trainingGraph
        dump [ for e in trainingGraph.Edges -> trainingGraph.WhichQubits e ]

    // interactions to train
    let couplings = [
        for edge in trainingGraph.Edges do
            yield Liquid.SpinTerm(2, interactions.ControlledInteraction edge, trainingGraph.WhichQubits edge, 1.0)
    ]

    let mutable parameters = None
    member this.Train (data : Dataset) =
        // run training once for YES and once for NO-instances
        // result format is
        // yes instances : [ for all edges : [("interaction1", strength); ...]; [...]; ...];
        // no instances ...
        let results =
            [("YES", data.YesInstances, -1.0); ("NO", data.NoInstances, 1.0)]
            ||>
            ( fun (tag, data : Dataset, weight) ->
                // projector on training data. Interaction strength scaled to be dominating term
                dump (data.ValuatedList weight)
                let projector = Liquid.SpinTerm(1, Interactions.Projector (data.ValuatedList weight), trainingGraph.WhichQubits trainingGraph.Outputs, 10. * (float trainingGraph.Size))

                // create spin model and train
                let spin = Liquid.Spin(projector :: couplings, trainingGraph.Size, Liquid.RunMode.Trotter1X)
                Liquid.Spin.Test(
                    tag = "train " + tag,
                    repeats = 20,
                    trotter = 50,
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
                let controlProbabilities = [
                    for edge in trainingGraph.Edges ->
                        let control = edge.GetControl
                        let results = MeasurementStatistics spin.Ket ((trainingGraph.WhichQubits control))
                        Interactions.Sets.InterpretControlMeasurement results control
                ]
                
                dump controlProbabilities
                controlProbabilities
            )
            |> (fun res ->
                let yes = res.[0]
                let no = res.[1]

                // normalize weights to [-1, 1]
                let minYes = yes |> List.concat |> List.minBy snd |> snd
                let maxYes = yes |> List.concat |> List.maxBy snd |> snd
                let minNo = no |> List.concat |> List.minBy snd |> snd
                let maxNo = no |> List.concat |> List.maxBy snd |> snd

                let yesNormalizer = if (maxYes - minYes) <> 0.0 then fun v -> (v - minYes) / (maxYes - minYes) else id
                let noNormalizer = if (maxNo - minNo) <> 0.0 then fun v -> (v - minNo) / (maxNo - minNo) else id

                // for every edge, assemble list of interaction name * strength
                let noDict = no ||> Map.ofList
                let addedWeights = [
                    for edge in 0..yes.Length-1 ->
                    [
                        for (name, yesWeight) in yes.[edge] do
                            let noWeight = noDict.[edge].[name]
                            yield name, (yesNormalizer yesWeight) - (noNormalizer noWeight)
                    ]
                ] 

                // normalize weights to [-1, 1]
                let weightsMax = addedWeights |> List.concat |> List.maxBy snd |> snd
                [ for edge in addedWeights -> [ for interaction in edge -> fst(interaction), snd(interaction) / weightsMax ] ]               
            )

        // combine training results with edges and flatten,
        // so that we have tuples (edge, interaction, weight)
        parameters <- Some (
            (List.zip trainingGraph.Edges results)
            ||>
            (
                fun (edge, interactions) ->
                    let plainEdge = edge.StripControl
                    [ for i in interactions -> plainEdge, fst(i), snd(i) ]
            )
            |> List.concat
        )
        dump parameters
        
        this
 
    member this.Test (data : Dataset) =
        match parameters with
        | None -> failwith "model not trained"
        | Some parameters ->
            // build interactions from previously trained model
            let couplings = [
                for p in parameters ->
                    let (edge, name, strength) = p
                    Liquid.SpinTerm(1, interactions.Interaction name, graph.WhichQubits edge, strength)
            ]

            let spin = Liquid.Spin(couplings, graph.Size, Liquid.RunMode.Trotter1X)
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
            let outputs = graph.WhichQubits graph.Outputs // list of qubits that act as output qubits
 
            // set part of state vector that is not output to |0> + |1>, so that we trace out the hidden part
            [ for i in 0..graph.Size-1 -> i ] |> List.map (fun i ->
                if not (contains i outputs) then
                    qubits.[i].StateSet(1., 0., 1., 0.)
            ) |> ignore
            state.Dump Liquid.Util.showInd

            let updateStateData (data : DataT) =
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
        //dump !!(spin.Ket.Qubits, trainingGraph.Controls)
        //let controls = MeasurementStatistics spin.Ket trainingGraph.Controls
        //dump controls*)
end