// LiquidLearn (c) 2016 Johannes Bausch
// Auto-Generated File with gates.nb on Fri 11 Mar 2016 21:02:30
namespace LiquidLearn.GeneratedCode

module UniversalHistory =
    open Microsoft.Research.Liquid
    let List = [ "Copy"; "xA"; "xB"; "xC"; "xD"; "xE"; "xF"; ]

    let Get (id : string) t =
        // local mapping
        let Cos = System.Math.Cos
        let Sin = System.Math.Sin
        let Cosh = System.Math.Cosh
        let Sinh = System.Math.Sinh
        let Power(a, b) = a**b
            
        match id with
        | "Copy" -> CSMat(4, [0, 0, Cos(0.5*t), Sin(0.5*t); 1, 1, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 1, 2, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 2, 1, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 2, 2, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 3, 3, Cos(0.5*t), Sin(0.5*t)])
        | "xA" -> CSMat(4, [0, 0, Power(Cos(t),2.), Cos(t)*Sin(t); 0, 1, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 1, 0, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 1, 1, Power(Cos(t),2.), Cos(t)*Sin(t); 2, 2, 1., 0.; 3, 3, 1., 0.])
        | "xB" -> CSMat(4, [0, 0, 1., 0.; 1, 1, Power(Cos(t),2.), Cos(t)*Sin(t); 1, 2, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 2, 1, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 2, 2, Power(Cos(t),2.), Cos(t)*Sin(t); 3, 3, 1., 0.])
        | "xC" -> CSMat(4, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, Power(Cos(t),2.), Cos(t)*Sin(t); 2, 3, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 3, 2, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 3, 3, Power(Cos(t),2.), Cos(t)*Sin(t)])
        | "xD" -> CSMat(4, [0, 0, Power(Cos(t),2.), Cos(t)*Sin(t); 0, 2, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 1, 1, 1., 0.; 2, 0, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 2, 2, Power(Cos(t),2.), Cos(t)*Sin(t); 3, 3, 1., 0.])
        | "xE" -> CSMat(4, [0, 0, 1., 0.; 1, 1, Power(Cos(t),2.), Cos(t)*Sin(t); 1, 3, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 2, 2, 1., 0.; 3, 1, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 3, 3, Power(Cos(t),2.), Cos(t)*Sin(t)])
        | "xF" -> CSMat(4, [0, 0, Power(Cos(t),2.), Cos(t)*Sin(t); 0, 3, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 0, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 3, 3, Power(Cos(t),2.), Cos(t)*Sin(t)])
        | _ -> failwith "Universal history state matrices not precomputed"
