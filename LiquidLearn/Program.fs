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

    // all possible unique data sets invariant under the given symmetries, plus yes/no symmetry
    // for some reason, writing all the pipeline in one massively slows down compilation,
    // to the extend of making it unusable. This sounds like a bug in the F# compiler.
    // Note that this function grows superfactorially, so don't feed it more than 3.
    let AllDataSets n (symmetries : (int list * int list) list) =
        let symmetries = id :: (symmetries ||> fun (a, b) -> getPermutation a b)

        let allPossible =
            ["0"; "1"]
            |> tuples n // all possible data tuples
            |> permutations // and all possible permutations of these datasets
            ||> SplitYesNo // create all possible training sets

        let duplicatesRemoved =
            allPossible
            |||> Set.ofList // delete all duplicates, e.g. yes and no swapped, order of data in yes or no (training being symmetric)
            ||> Set.ofList
            |> List.distinct
            |||> Set.toList
            ||> Set.toList

        let symmetriesRemoved =
            duplicatesRemoved
                |> List.distinctBy (fun instances ->
                    let withSymmetries = symmetries ||> (fun s -> instances |||> permute s)
                    Set.ofList withSymmetries
                )

        symmetriesRemoved
            ||||> join ""
            ||||> FromString
            ||> fun sets -> { Yes = sets.[0]; No = sets.[1] }

    // randomized version
    // for big n, it is unlikely to have symmetries, so we ignore that check
    let SomeDataSets n count min max =
        let max = Math.Min(pown 2 (n-1), max) + 1 // guarantee no overlap between yes and no dataset
        let allWords = ["0"; "1"] |> tuples n |> List.toArray

        [ for _ in 1..count ->
            shuffle allWords |> ignore
            let yes =
                allWords
                |> Array.take (rand.Next(min, max))
                |> Array.toList
                ||> join ""
            let no =
                allWords
                |> Array.rev
                |> Array.take (rand.Next(min, max))
                |> Array.toList
                ||> join ""
            { Yes = FromString <|| yes; No = FromString <|| no }
         ]
        

    let Run(datasets : (DataSet * DataSet) list, edges : EdgeT list, interactions : Sets.IInteractionFactory list) =
        let graph = new Hypergraph(edges)

        // iterate over all interaction sets
        for interaction in interactions do
            let filename = sprintf "graph-%s-%s-data-%d" graph.ShortForm interaction.ShortForm datasets.Length 
            if not <| File.Exists(filename + ".stats") then
                File.Create(filename + ".stats") |> ignore
                if File.Exists(filename + ".test") then File.Delete(filename + ".test")

                let stopwatch = Stopwatch.StartNew()

                let model = new SimpleControlledTrainer(graph, interaction, trainOnQudits = 5, maxVertices = 4, trotter = 25, resolution = 25)
                let timeModelCreated = stopwatch.ElapsedMilliseconds
               
                if model.Size > 22 then
                    dumps (sprintf "too many qubits needed: %d. Aborting." model.Size)
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
                    File.WriteAllText(filename + ".stats", sprintf "timeModelCreated %d\ntimeDone %d\narg per run %d\n%A\n%A\n\n%A\n\n%A" timeModelCreated timeDone (timeDone/(int64 datasets.Length)) graph model.TrainingGraph interaction datasets)

                            


    [<LQD>]
    let MSRSubmissionData(mode : string) =
        match mode with
        | "benchmark" ->
            // training data
            let symmetry2 = permutations [1; 2] ||> fun a -> [1; 2], a
            let symmetry3 = permutations [1; 2; 3] ||> fun a -> [1; 2; 3], a

            let dataSets2 = AllDataSets 2 symmetry2 |> fun a -> List.zip a a
            let dataSets3 = AllDataSets 3 symmetry3 |> fun a -> List.zip a a

            let dataSets6 = SomeDataSets 6 10 5 25 |> fun a -> List.zip a a

            // batch list of runs
            let runs : ((DataSet*DataSet) list * EdgeT list * Sets.IInteractionFactory list) list = [
                dataSets2, [ O"1" --- O"2" ], [Sets.Projectors(); Sets.Paulis(); Sets.Heisenberg(); Sets.Ising(); Sets.Random()]
                dataSets2, [ O"1" --- V"h"; V"h" --- O"2" ], [Sets.Projectors(); Sets.Paulis(); Sets.Heisenberg(); Sets.Ising(); Sets.Random()]
                dataSets3, [ O"1" --- V"h"; V"h" --- O"2"; V"h" --- O"3" ], [Sets.Projectors(); Sets.Heisenberg(); Sets.Ising(); Sets.Random(); Sets.Paulis()]
            
                dataSets6, [ O"1" --- V"a"; V"a" --- O"4"; V"a" --- O"2"; V"a" --- V"b"; V"a" --- O"5"; O"2" --- V"b"; O"5" --- V"b"; V"b" --- O"3"; V"b" --- O"6"], [ Sets.Projectors(); Sets.Ising(); Sets.Heisenberg()  ]
            ]

            // run
            runs ||> Run |> ignore

        | "color" ->
            let edges = [
                O"r1" --- V"c"
                O"r2" --- V"c"
                O"r3" --- V"c"

                O"g1" --- V"c"
                O"g2" --- V"c"
                O"g3" --- V"c"

                O"b1" --- V"c"
                O"b2" --- V"c"
                O"b3" --- V"c"
            ]

            Run([LearnAppData.ColourDataset, { Yes = LearnAppData.AllColours; No = [] }], edges, [Sets.Projectors()])

        | _ ->
            dumps "specify either \"benchmark\" or \"color\" as argument to MSRSubmissionData()"

        ()

    [<LQD>]
    let Learn() =
        let data = (AllDataSets 2 []) |> List.take 3
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



    [<EntryPoint>]
    let main _ = 
        Dumps "LiquidLearn (c) 2016 Johannes Bausch (jkrb2@cam.ac.uk)"

        // tone down liquid output
        System.Console.ForegroundColor <- if Type.GetType("Mono.Runtime") <> null then System.ConsoleColor.Gray else System.ConsoleColor.DarkGray 
        App.RunLiquid()
