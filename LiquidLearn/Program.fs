﻿// LiquidLearn (c) 2016 Johannes Bausch

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
        ()
        

    [<EntryPoint>]
    let main _ = 
        show "LiquidLearn"

        App.RunLiquid()
