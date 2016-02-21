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
            Yes = (Parse /@ ["01"])
            No  = (Parse /@ ["11"; "00"])
        }
        let edges = [ O"1" --- V"hidden";
                      V"hidden" --- O"2" ]
        let graph = new Hypergraph(edges)
        let model = (new SimpleControlledTrainer(graph, Sets.FullProjectors)).Train data
        model.Test data
        ()

        

    [<EntryPoint>]
    let main _ = 
        show "LiquidLearn"

        App.RunLiquid()
