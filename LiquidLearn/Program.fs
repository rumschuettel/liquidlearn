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
        (*let edges = [
            O"i0" --- V"h0" --- O"i1"
            O"i1" --- V"h1" --- O"i2"

            V"h0" --- O"i1" --- V"h1"

            V"h0" --- O"o" --- V"h1"
        ]
        let graph = new Hypergraph(edges)
        ControlledTrainer.Train graph {
            Yes = (Parse /@ ["000"; "010"])
            No  = (Parse /@ ["111"; "110"; "011"])
        }*)

        let edges = [ O"1" --- O"2" ]
        let graph = new Hypergraph(edges)
        ControlledTrainer.Train graph {
            Yes = (Parse /@ ["01"])
            No  = (Parse /@ ["11"; "00"])
        }
        ()


        

    [<EntryPoint>]
    let main _ = 
        show "LiquidLearn"

        App.RunLiquid()
