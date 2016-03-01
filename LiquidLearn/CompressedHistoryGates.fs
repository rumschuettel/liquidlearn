// LiquidLearn (c) 2016 Johannes Bausch
// Auto-Generated File with gates.nb on Tue 1 Mar 2016 22:02:28
namespace LiquidLearn.GeneratedCode

module UniversalHistoryCompressed =
    open Microsoft.Research.Liquid
    let List = [ "Copyxx"; "Copyxx1"; "Copyx1x"; "Copy1xx"; "SwapAxx"; "SwapAxx1"; "SwapAx1x"; "SwapA1xx"; "SwapBxx"; "SwapBxx1"; "SwapBx1x"; "SwapB1xx"; "PhAxx"; "PhAxx1"; "PhAx1x"; "PhA1xx"; "PhBxx"; "PhBxx1"; "PhBx1x"; "PhB1xx"; ]

    let Get (id : string) t =
        // local mapping
        let Cos = System.Math.Cos
        let Sin = System.Math.Sin
        let Cosh = System.Math.Cosh
        let Sinh = System.Math.Sinh
        let Power(a, b) = a**b
            
        match id with
        | "Copyxx" -> CSMat(4, [0, 0, Cos(0.5*t), Sin(0.5*t); 1, 1, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 1, 2, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 2, 1, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 2, 2, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 3, 3, Cos(0.5*t), Sin(0.5*t)])
        | "Copyxx1" -> CSMat(8, [0, 0, Cos(0.5*t), Sin(0.5*t); 1, 1, Cos(0.5*t), Sin(0.5*t); 2, 2, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 2, 4, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 3, 3, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 3, 5, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 4, 2, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 4, 4, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 5, 3, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 5, 5, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 6, 6, Cos(0.5*t), Sin(0.5*t); 7, 7, Cos(0.5*t), Sin(0.5*t)])
        | "Copyx1x" -> CSMat(8, [0, 0, Cos(0.5*t), Sin(0.5*t); 1, 1, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 1, 4, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 2, 2, Cos(0.5*t), Sin(0.5*t); 3, 3, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 3, 6, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 4, 1, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 4, 4, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 5, 5, Cos(0.5*t), Sin(0.5*t); 6, 3, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 6, 6, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 7, 7, Cos(0.5*t), Sin(0.5*t)])
        | "Copy1xx" -> CSMat(8, [0, 0, Cos(0.5*t), Sin(0.5*t); 1, 1, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 1, 2, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 2, 1, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 2, 2, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 3, 3, Cos(0.5*t), Sin(0.5*t); 4, 4, Cos(0.5*t), Sin(0.5*t); 5, 5, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 5, 6, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 6, 5, Sin(0.5*t)*Sin(t), Cos(0.5*t)*Sin(t); 6, 6, Cos(0.5*t)*Cos(t), -1.*Cos(t)*Sin(0.5*t); 7, 7, Cos(0.5*t), Sin(0.5*t)])
        | "SwapAxx" -> CSMat(4, [0, 0, 1., 0.; 1, 1, Power(Cos(t),2.), Cos(t)*Sin(t); 1, 2, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 2, 1, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 2, 2, Power(Cos(t),2.), Cos(t)*Sin(t); 3, 3, 1., 0.])
        | "SwapAxx1" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, Power(Cos(t),2.), Cos(t)*Sin(t); 2, 4, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 3, 3, Power(Cos(t),2.), Cos(t)*Sin(t); 3, 5, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 4, 2, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 4, 4, Power(Cos(t),2.), Cos(t)*Sin(t); 5, 3, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 5, 5, Power(Cos(t),2.), Cos(t)*Sin(t); 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "SwapAx1x" -> CSMat(8, [0, 0, 1., 0.; 1, 1, Power(Cos(t),2.), Cos(t)*Sin(t); 1, 4, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 2, 2, 1., 0.; 3, 3, Power(Cos(t),2.), Cos(t)*Sin(t); 3, 6, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 4, 1, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 4, 4, Power(Cos(t),2.), Cos(t)*Sin(t); 5, 5, 1., 0.; 6, 3, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 6, 6, Power(Cos(t),2.), Cos(t)*Sin(t); 7, 7, 1., 0.])
        | "SwapA1xx" -> CSMat(8, [0, 0, 1., 0.; 1, 1, Power(Cos(t),2.), Cos(t)*Sin(t); 1, 2, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 2, 1, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 2, 2, Power(Cos(t),2.), Cos(t)*Sin(t); 3, 3, 1., 0.; 4, 4, 1., 0.; 5, 5, Power(Cos(t),2.), Cos(t)*Sin(t); 5, 6, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 6, 5, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 6, 6, Power(Cos(t),2.), Cos(t)*Sin(t); 7, 7, 1., 0.])
        | "SwapBxx" -> CSMat(4, [0, 0, Power(Cos(t),2.), Cos(t)*Sin(t); 0, 1, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 1, 0, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 1, 1, Power(Cos(t),2.), Cos(t)*Sin(t); 2, 2, 1., 0.; 3, 3, 1., 0.])
        | "SwapBxx1" -> CSMat(8, [0, 0, Power(Cos(t),2.), Cos(t)*Sin(t); 0, 2, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 1, 1, Power(Cos(t),2.), Cos(t)*Sin(t); 1, 3, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 2, 0, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 2, 2, Power(Cos(t),2.), Cos(t)*Sin(t); 3, 1, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 3, 3, Power(Cos(t),2.), Cos(t)*Sin(t); 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "SwapBx1x" -> CSMat(8, [0, 0, Power(Cos(t),2.), Cos(t)*Sin(t); 0, 1, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 1, 0, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 1, 1, Power(Cos(t),2.), Cos(t)*Sin(t); 2, 2, Power(Cos(t),2.), Cos(t)*Sin(t); 2, 3, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 3, 2, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 3, 3, Power(Cos(t),2.), Cos(t)*Sin(t); 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "SwapB1xx" -> CSMat(8, [0, 0, Power(Cos(t),2.), Cos(t)*Sin(t); 0, 1, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 1, 0, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 1, 1, Power(Cos(t),2.), Cos(t)*Sin(t); 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, Power(Cos(t),2.), Cos(t)*Sin(t); 4, 5, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 5, 4, Power(Sin(t),2.), -1.*Cos(t)*Sin(t); 5, 5, Power(Cos(t),2.), Cos(t)*Sin(t); 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "PhAxx" -> CSMat(4, [0, 0, 1., 0.; 1, 1, Cos(0.25*t), Sin(0.25*t); 2, 2, 1., 0.; 3, 3, 1., 0.])
        | "PhAxx1" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, Cos(0.25*t), Sin(0.25*t); 3, 3, Cos(0.25*t), Sin(0.25*t); 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "PhAx1x" -> CSMat(8, [0, 0, 1., 0.; 1, 1, Cos(0.25*t), Sin(0.25*t); 2, 2, 1., 0.; 3, 3, Cos(0.25*t), Sin(0.25*t); 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "PhA1xx" -> CSMat(8, [0, 0, 1., 0.; 1, 1, Cos(0.25*t), Sin(0.25*t); 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, 1., 0.; 5, 5, Cos(0.25*t), Sin(0.25*t); 6, 6, 1., 0.; 7, 7, 1., 0.])
        | "PhBxx" -> CSMat(4, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, Cos(0.25*t), Sin(0.25*t)])
        | "PhBxx1" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, Cos(0.25*t), Sin(0.25*t); 7, 7, Cos(0.25*t), Sin(0.25*t)])
        | "PhBx1x" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, 1., 0.; 4, 4, 1., 0.; 5, 5, Cos(0.25*t), Sin(0.25*t); 6, 6, 1., 0.; 7, 7, Cos(0.25*t), Sin(0.25*t)])
        | "PhB1xx" -> CSMat(8, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 1., 0.; 3, 3, Cos(0.25*t), Sin(0.25*t); 4, 4, 1., 0.; 5, 5, 1., 0.; 6, 6, 1., 0.; 7, 7, Cos(0.25*t), Sin(0.25*t)])
        | _ -> failwith "Universal history state matrices compressed not precomputed"
