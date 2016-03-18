// LiquidLearn (c) 2016 Johannes Bausch

namespace Microsoft.Research.Liquid



module LearnApp =
    open LiquidLearn.Graph
    open LiquidLearn.Utils
    open LiquidLearn.Utils.Data
    open LiquidLearn.Train
    open LiquidLearn.Interactions

    open Util
    open System
    open System.IO
    open System.Diagnostics

    // splits list in two
    let SplitYesNo (list : 'a list) = [List.take (list.Length/2) list; List.skip (list.Length/2) list]

    // all possible data sets
    let AllDataSets n =
        ["0"; "1"]
            |> tuples n // all possible data tuples
            |> permutations // and all possible permutations of these datasets
            ||> SplitYesNo // create all possible training sets
            |||> Set.ofList // delete all duplicates, e.g. yes and no swapped, order of data in yes or no (training being symmetric)
            ||> Set.ofList
            |> Set.ofList
            |> Set.toList
            ||> Set.toList
            |||> Set.toList
            ||||> join ""
            ||||> FromString
            ||> fun sets -> { Yes = sets.[0]; No = sets.[1] }

    let Run(datasets : (DataSet * DataSet) list, edges : EdgeT list, interactions : Sets.IInteractionFactory list) =
        let graph = new Hypergraph(edges)

        // iterate over all interaction sets
        for interaction in interactions do
            let filename = sprintf "graph-%s-%s-data-%d" graph.ShortForm interaction.ShortForm datasets.Length 
            if not <| File.Exists(filename + ".stats") then
                if File.Exists(filename + ".test") then File.Delete(filename + ".test")

                let stopwatch = Stopwatch.StartNew()

                let model = new SimpleControlledTrainer(graph, interaction, trainOnQudits = 5, maxVertices = 3, trotter = 25, resolution = 25)
                let timeModelCreated = stopwatch.ElapsedMilliseconds
               
                if model.Size > 19 then
                    File.WriteAllText(filename + ".fail", sprintf "too many qubits needed: %d" model.Size)
                else
                    // run model for every training set
                    datasets
                        ||> fun (train, test) ->
                                let results = (model.Train train).Test test
                                results.ToFile(filename + ".test", append = true)
                                results
                        ||> Dump |> ignore
                    let timeDone = stopwatch.ElapsedMilliseconds

                    // write stats file
                    File.WriteAllText(filename + ".stats", sprintf "timeModelCreated %d\ntimeDone %d\n\n%A\n%A\n\n%A\n\n%A" timeModelCreated timeDone graph model.TrainingGraph interaction datasets)

                            


    [<LQD>]
    let MSRSubmissionData() =
        let runs : ((DataSet*DataSet) list * EdgeT list * Sets.IInteractionFactory list) list = [
            List.zip (AllDataSets 2) (AllDataSets 2), [ O"1" --- O"2" ], [Sets.Projectors(); Sets.Paulis(); Sets.Heisenberg(); Sets.Ising(); Sets.Random()]
            List.zip (AllDataSets 2) (AllDataSets 2), [ O"1" --- V"h"; V"h" --- O"2" ], [Sets.Projectors(); Sets.Paulis(); Sets.Heisenberg(); Sets.Ising(); Sets.Random()]
            List.zip (AllDataSets 3) (AllDataSets 3), [ O"1" --- V"h"; V"h" --- O"2"; V"h" --- O"3" ], [Sets.Projectors(); Sets.Heisenberg(); Sets.Ising(); Sets.Random(); Sets.Paulis()]
        ]

        runs ||> Run |> ignore

        ()

    [<LQD>]
    let Learn() =
        let data = (AllDataSets 2) |> List.take 3
        let edges = [ O"1" --- O"2" ]
        let graph = new Hypergraph(edges)
        let model = new SimpleControlledTrainer(graph, Sets.Ising(), trainOnQudits = 5, maxVertices = 3)

        data
            ||> fun d -> 
                let trained = model.Train d
                let results = model.Test d
                results
            |> Dump
            |> ignore

        ()

    [<LQD>]
    let Benchmark() =
        Dumps "Training Benchmark Mode"

        // create all possible training sets
        let data = AllDataSets 2

        data ||> dump |> ignore

        let edges = [ O"1" --- O"2" ]
        let graph = new Hypergraph(edges)
        let model = new SimpleControlledTrainer(graph, Sets.Projectors(), trainOnQudits = 2)

        data
            ||> fun data ->
                    let trained = model.Train data
                    let results = model.Test data
                    results.ToFile ("benchmarkRandom.test", append = true)
                    results
            ||> dump |> ignore
        ()


    [<EntryPoint>]
    let main _ = 
        Dumps "LiquidLearn (c) 2016 Johannes Bausch (jkrb2@cam.ac.uk)"

        // tone down liquid output
        System.Console.ForegroundColor <- if Type.GetType("Mono.Runtime") <> null then System.ConsoleColor.Gray else System.ConsoleColor.DarkGray 
        App.RunLiquid()
