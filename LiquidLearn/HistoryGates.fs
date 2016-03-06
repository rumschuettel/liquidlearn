// LiquidLearn (c) 2016 Johannes Bausch
// Auto-Generated File with gates.nb on Sun 6 Mar 2016 14:32:07
namespace LiquidLearn.GeneratedCode

module UniversalHistory =
    open Microsoft.Research.Liquid
    let List = [ "Copy"; "SwapA"; "SwapB"; "PhA"; "PhB"; ]

    let Get (id : string) t =
        // local mapping
        let Cos = System.Math.Cos
        let Sin = System.Math.Sin
        let Cosh = System.Math.Cosh
        let Sinh = System.Math.Sinh
        let Power(a, b) = a**b
            
        match id with
        | "Copy" -> CSMat(4, [0, 0, Cos(0.5*t), Sin(0.5*t); 1, 1, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 1, 2, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 2, 1, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 2, 2, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 3, 3, Cos(0.5*t), Sin(0.5*t)])
        | "SwapA" -> CSMat(4, [0, 0, 1., 0.; 1, 1, Power(Cos(t),2.), Cos(t)*Sin(t); 1, 2, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 2, 1, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 2, 2, Power(Cos(t),2.), Cos(t)*Sin(t); 3, 3, 1., 0.])
        | "SwapB" -> CSMat(4, [0, 0, Power(Cos(t),2.), Cos(t)*Sin(t); 0, 1, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 1, 0, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 1, 1, Power(Cos(t),2.), Cos(t)*Sin(t); 2, 2, 1., 0.; 3, 3, 1., 0.])
        | "PhA" -> CSMat(4, [0, 0, 1., 0.; 1, 1, Cos(0.25*t), Sin(0.25*t); 2, 2, 1., 0.; 3, 3, 1., 0.])
        | "PhB" -> CSMat(4, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, Cos(0.25*t), Sin(0.25*t)])
        | _ -> failwith "Universal history state matrices not precomputed"
