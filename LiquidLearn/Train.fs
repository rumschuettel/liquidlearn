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


// interpret measurement result on controlled interaction
let InterpretControlMeasurement (results : float list) (control : Graph.VertexT) =
    let interactions = control.Unwrap.interactions
    let totalBlocks = nextPowerOf2 (interactions.Length + 1)

    // for e.g. 5 interactions, we reserved 3 qubits, so totalBlocks = 8; which should equal the measured qubit list (2^3=8)
    assert (totalBlocks = results.Length)

    // number of identity blocks in the top left
    let controlBlocks = (totalBlocks - interactions.Length)

    // support on control
    let zeroInteractionSupport =
        results
        |> List.take controlBlocks
        |> List.sum

    let results =
        results
        |> List.skip controlBlocks
        ||> (fun prob -> (prob - zeroInteractionSupport) * (float interactions.Length))


    List.zip interactions results


[<StructuredFormatDisplay("TestResults {TableForm}")>]
type TestResults = {
        YesStats : (float*float) list
        NoStats  : (float*float) list
}  with
    member this.TableForm =
        join "" (
            [ for (e, sigma) in this.YesStats -> sprintf "\n  y\t%.2e\t%.2e" e sigma ]
            @
            [ for (e, sigma) in this.NoStats -> sprintf "\n  n\t%.2e\t%.2e" e sigma ]
        )

    member this.ToFile (filename, ?append) =
        let append = defaultArg append false
        if append then
            System.IO.File.AppendAllText(filename, this.TableForm)
        else
            System.IO.File.WriteAllText(filename, this.TableForm)



// simple controlled gate trainer
type SimpleControlledTrainer
    (
        graph : Graph.Hypergraph,
        interactions : Interactions.Sets.IInteractionFactory,
        ?trainOnQudits : int,
        ?maxVertices : int,
        ?trotter : int,
        ?resolution : int
    ) = class
    let trainOnQudits = defaultArg trainOnQudits 2
    let maxVertices = defaultArg maxVertices 3
    let trotter = defaultArg trotter 20
    let resolution = defaultArg resolution 20

    do
        Dumps "Simple Controlled Qubit Trainer"
        dumps (sprintf "trainOnQudits=%s, trotter=%d, steps=%d" (if trainOnQudits > 1 then (sprintf "yes (%d)" trainOnQudits) else "no") trotter (12*resolution))
        dump graph

    // create controlled version of graph
    let trainingGraph =
        (graph.AddControls ( fun edge ->
        // for now, every edge gets one control with all possible interactions on that edge
            [{
                id = UniqueID()
                interactions = interactions.ListPossibleInteractions edge
            }]
        )).OptimizeControls(
            maxInteractions = (pown 2 trainOnQudits) - 1,
            maxVertices = maxVertices
        )   

    do
        dumps (sprintf "training on %A" trainingGraph)
        dumps (sprintf "with interaction set %A" interactions)

    // interactions to train
    let couplings = [
        for edge in trainingGraph.Edges do
            yield new Liquid.SpinTerm(
                s = 2,
                o = interactions.ControlledInteraction edge,
                idx = trainingGraph.WhichQubits edge,
                a = 1.0
            )
    ]

    // annealing schedule
    let schedule = [
        0,              [|1.00; 0.00; 0.00|];
        2*resolution,   [|1.00; 0.25; 0.00|];
        12*resolution,  [|0.00; 1.00; 1.00|]
    ]

    let mutable parameters = None

    // return size of training graph
    member this.Size = trainingGraph.Size

    // return training graph
    member this.TrainingGraph = trainingGraph

    // train dataset
    member this.Train (data : DataSet, ?dataProjectorWeight : float, ?postSelector : string -> Liquid.Ket -> Liquid.Ket) =
        let dataProjectorWeight = defaultArg dataProjectorWeight 10.
        let postSelector = defaultArg postSelector (fun _ -> id)
        // run training once for YES and once for NO-instances

        let results =
            (("YES", data, (1.0, -0.0)), ("NO", data, (-0.0, 1.0)))
            ||>
            ( fun (tag, data : DataSet, weight) ->
                // projector on training data. Interaction strength scaled to be dominating term
                dumps (sprintf "training %s (weights %.2f, %.2f) using %A" tag (fst weight) (snd weight) data)
                let projector =
                    new Liquid.SpinTerm(
                        s = 1,
                        o = Interactions.Projector (graph.Outputs.Length) (data.ValuatedList weight),
                        idx = trainingGraph.WhichQubits trainingGraph.Outputs,
                        a = dataProjectorWeight * (float trainingGraph.Size)
                    )

                // create spin model and train
                dumps (sprintf "allocating %d qubits" trainingGraph.Size)
                let spin = new Liquid.Spin(projector :: couplings, trainingGraph.Size, Liquid.RunMode.Trotter1X)
                Liquid.Spin.Test(
                    tag = "train " + tag,
                    repeats = 20,
                    trotter = trotter,
                    schedule = schedule,
                    res = 3*resolution,
                    spin = spin,
                    runonce = true,
                    decohereModel = []
                )
                // allow postselection function, also used for test injections
                let ket = postSelector tag spin.Ket

                // extract trained control probabilities
                [ for edge in trainingGraph.Edges ->
                    let control = edge.GetControl
                    let results = MeasurementStatistics ket ((trainingGraph.WhichQubits control))
                    let weights = InterpretControlMeasurement results control

                    dumps (sprintf "un-normalized interaction probabilities for edge %A" edge.StripControl)
                    weights ||> dump |> ignore

                    weights
                ]
            )
            |> (fun (yes, no) ->
                // both yes and no are lists of probabilities, one entry per edge, of elements ["interaction1", weight1; "interaction2", weight2; ...]
                let max = List.concat >> List.maxBy snd >> snd
                let min = List.concat >> List.minBy snd >> snd
                // normalize weights to [0, 1]
                let maxYes = max yes
                let maxNo = max no

                let yesNormalizer = if maxYes <> 0.0 then fun v -> v / maxYes else id
                let noNormalizer = if maxNo <> 0.0 then fun v -> v / maxNo else id

                // for every edge, assemble list of interaction name * strength
                let addedWeights =
                    List.zip yes no
                    ||> fun (y, n) ->
                            List.zip3 (fst <|| y) (snd <|| y) (snd <|| n)
                    |||> fun (name, yesWeight, noWeight) -> 
                            // this line determins how the yes and no weights are interpreted and combined to form one single interaction weight
                            name, (yesNormalizer yesWeight) - (noNormalizer noWeight)

                // normalize weights to [-1, 1]
                let weightsMax = System.Math.Max(max addedWeights, min addedWeights)
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
        dumps "trained interaction parameters"
        parameters.Value ||> dump |> ignore
        
        this
 
    member this.Test (data : DataSet) =
        match parameters with
        | None -> failwith "model not trained"
        | Some parameters ->
            // build interactions from previously trained model
            let couplings = [
                for (_, interaction, strength) in parameters ->
                    new Liquid.SpinTerm(
                        s = 1,
                        o = interactions.Interaction interaction,
                        idx = graph.WhichQubits interaction.vertices,
                        a = strength
                    )
            ]

            let spin = new Liquid.Spin(couplings, graph.Size, Liquid.RunMode.Trotter1X)
            Liquid.Spin.Test(
                tag = "test",
                repeats = 20,
                trotter = trotter,
                schedule = schedule,
                res = 3*resolution,
                spin = spin,
                runonce = true,
                decohereModel = []
            )

            let state = new Liquid.Ket(graph.Size)
            let qubits = state.Qubits
            let outputs = graph.WhichQubits graph.Outputs // list of qubits that act as output qubits
 
            // set part of state vector that is not output to |0> + |1>, so that we partially trace out the hidden part
            [ for i in 0..graph.Size-1 -> i ] |> List.map (fun i ->
                if not (contains i outputs) then
                    qubits.[i].StateSet(1., 0., 1., 0.)
            ) |> ignore

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

end