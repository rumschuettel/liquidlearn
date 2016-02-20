// LiquidLearn (c) 2016 Johannes Bausch
// Auto-Generated File with gates.nb on Sat 20 Feb 2016 16:12:27
namespace LiquidLearn.GeneratedCode

module Rank1ProjectionMatrices =
    open Microsoft.Research.Liquid
    let Get (id : string) t =
        // local mapping
        let Cos = System.Math.Cos
        let Sin = System.Math.Sin
            
        match id with
        | "0" -> CSMat(2, [0, 0, Cos(t), Sin(t); 1, 1, 1., 0.])
        | "1" -> CSMat(2, [0, 0, 1., 0.; 1, 1, Cos(t), Sin(t)])
        | "00" -> CSMat(4, [0, 0, Cos(t), Sin(t); 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, 1., 0.])
        | "01" -> CSMat(4, [0, 0, 1., 0.; 1, 1, Cos(t), Sin(t); 2, 2, 1., 0.; 3, 3, 1., 0.])
        | "10" -> CSMat(4, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, Cos(t), Sin(t); 3, 3, 1., 0.])
        | "11" -> CSMat(4, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, Cos(t), Sin(t)])
        | "000" -> CSMat(8, [0, 0, Cos(t), Sin(t); 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "001" -> CSMat(8, [0, 0, 1., 0.; 1, 1, Cos(t), Sin(t); 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "010" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, Cos(t), Sin(t); 3, 3, 1., 0.; 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "011" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, Cos(t), Sin(t); 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "100" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, Cos(t), Sin(t); 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "101" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, 1., 0.; 5, 5, Cos(t), Sin(t); 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "110" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, Cos(t), Sin(t); 7, 7, 1., 0.])
        | "111" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, Cos(t), Sin(t)])
        | _ -> failwith "Projection not precomputed"
