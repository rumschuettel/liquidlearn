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

    [<LQD>]
    let Learn() =
        let data = {
            Yes = (FromString <|| ["001"; "111"])
            No  = (FromString <|| ["000"; "101"; "110"])
        }
        let edges = [ O"1" --- O"2" --- O"3"; ]
        let graph = new Hypergraph(edges)
        let model = new SimpleControlledTrainer(graph, Sets.CompressedProjectors(), trainOnQudits = true)
        let trained = model.Train data
        
        let results = model.Test {
            Yes = (FromString <|| ["001"; "111"; "010"; "100";])
            No  = (FromString <|| ["000"; "110"; "011"; "101"])
        }
        printfn "%s" (string results)
        results.ToFile "model2.test"
        ()

    [<LQD>]
    let Benchmark() =
        // splits list in two
        let splitYesNo (list : 'a list) = [List.take (list.Length/2) list; List.skip (list.Length/2) list]

        // create all possible training sets
        let data =
            ["0"; "1"]
            |> tuples 2
            |> permute
            ||> splitYesNo // create all possible training sets
            |||> Set.ofList // delete all duplocates, e.g. yes and no swapped, order of data in yes or no
            ||> Set.ofList
            |> Set.ofList
            |> Set.toList
            ||> Set.toList
            |||> Set.toList
            ||||> join ""
            ||||> FromString
            ||> fun sets -> { Yes = sets.[0]; No = sets.[1] }

        dump data

        let edges = [ O"1" --- O"2" ]
        let graph = new Hypergraph(edges)
        let model = new SimpleControlledTrainer(graph, Sets.Projectors(), trainOnQudits = true)

        data
            ||> fun data ->
                let trained = model.Train data
                let results = model.Test data
                results.ToFile ("benchmark2.test", append = true)
                results
            |> dump
        ()


    [<EntryPoint>]
    let main _ = 
        show "LiquidLearn"

        App.RunLiquid()
