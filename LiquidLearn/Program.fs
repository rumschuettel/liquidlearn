// LiquidLearn (c) 2016 Johannes Bausch

namespace Microsoft.Research.Liquid



module LearnApp =
    open LiquidLearn.Graph
    open LiquidLearn.Utils
    open LiquidLearn.Utils.Data
    open LiquidLearn.Train
    open Util
    open System

    [<LQD>]
    let Learn() =
        let edges = [ O"1" --- O"2" ]
        let graph = new Hypergraph(edges)
        ControlledTrainer.Train graph {
            Yes = (Parse /@ ["01"])
            No  = (Parse /@ ["11"; "00"])
        } Interactions.Sets.FullPauli
        ()

        

    [<EntryPoint>]
    let main _ = 
        show "LiquidLearn"

        App.RunLiquid()
