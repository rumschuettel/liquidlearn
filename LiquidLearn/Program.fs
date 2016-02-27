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
            Yes = (FromString /@ ["00"; "10"])
            No  = (FromString /@ ["11"])
        }
        let edges = [ O"1" --- O"2";]
        let graph = new Hypergraph(edges)
        let model = new SimpleControlledTrainer(graph, Sets.Projectors())
        let trained = model.Train data
        
        let results = model.Test {
            Yes = (FromString /@ ["000"; "10";])
            No  = (FromString /@ ["01"; "11";])
        }
        printfn "%s" (string results)
        results.ToFile "model2.test"
        ()

        

    [<EntryPoint>]
    let main _ = 
        show "LiquidLearn"

        App.RunLiquid()
