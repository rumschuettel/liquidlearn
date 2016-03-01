// LiquidLearn (c) 2016 Johannes Bausch
// Auto-Generated File with gates.nb on Tue 1 Mar 2016 20:38:45
namespace LiquidLearn.GeneratedCode

module Rank1CompressedProjectionMatrices =
    open Microsoft.Research.Liquid
    let List = [ "00"; "01"; "10"; "11"; "00i"; "01i"; "0i0"; "0i1"; "10i"; "11i"; "1i0"; "1i1"; "i00"; "i01"; "i10"; "i11"; ]

    let Get (id : string) t =
        // local mapping
        let Cos = System.Math.Cos
        let Sin = System.Math.Sin
        let Cosh = System.Math.Cosh
        let Sinh = System.Math.Sinh
        let Power(a, b) = a**b
            
        match id with
        | "00" -> CSMat(4, [0, 0, Cos(t), Sin(t); 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, 1., 0.])
        | "01" -> CSMat(4, [0, 0, 1., 0.; 1, 1, Cos(t), Sin(t); 2, 2, 1., 0.; 3, 3, 1., 0.])
        | "10" -> CSMat(4, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, Cos(t), Sin(t); 3, 3, 1., 0.])
        | "11" -> CSMat(4, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, Cos(t), Sin(t)])
        | "00i" -> CSMat(8, [0, 0, Cos(t), Sin(t); 1, 1, Cos(t), Sin(t); 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "01i" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, Cos(t), Sin(t); 3, 3, Cos(t), Sin(t); 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "0i0" -> CSMat(8, [0, 0, Cos(t), Sin(t); 1, 1, 1., 0.; 2, 2, Cos(t), Sin(t); 3, 3, 1., 0.; 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "0i1" -> CSMat(8, [0, 0, 1., 0.; 1, 1, Cos(t), Sin(t); 2, 2, 1., 0.; 3, 3, Cos(t), Sin(t); 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "10i" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, Cos(t), Sin(t); 5, 5, Cos(t), Sin(t); 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "11i" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, Cos(t), Sin(t); 7, 7, Cos(t), Sin(t)])
        | "1i0" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, Cos(t), Sin(t); 5, 5, 1., 0.; 6, 6, Cos(t), Sin(t); 7, 7, 1., 0.])
        | "1i1" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, 1., 0.; 5, 5, Cos(t), Sin(t); 6, 6, 1., 0.; 7, 7, Cos(t), Sin(t)])
        | "i00" -> CSMat(8, [0, 0, Cos(t), Sin(t); 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, Cos(t), Sin(t); 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "i01" -> CSMat(8, [0, 0, 1., 0.; 1, 1, Cos(t), Sin(t); 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, 1., 0.; 5, 5, Cos(t), Sin(t); 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "i10" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, Cos(t), Sin(t); 3, 3, 1., 0.; 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, Cos(t), Sin(t); 7, 7, 1., 0.])
        | "i11" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, Cos(t), Sin(t); 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, Cos(t), Sin(t)])
        | _ -> failwith "Compressed Projection not precomputed"
