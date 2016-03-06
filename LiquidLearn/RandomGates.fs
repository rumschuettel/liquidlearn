// LiquidLearn (c) 2016 Johannes Bausch
// Auto-Generated File with gates.nb on Sun 6 Mar 2016 15:12:34
namespace LiquidLearn.GeneratedCode

module SparseRandomHermitian =
    open Microsoft.Research.Liquid
    let List = [ "o"; "p"; "q"; "r"; "s"; ]

    let Get (id : string) t =
        // local mapping
        let Cos = System.Math.Cos
        let Sin = System.Math.Sin
        let Cosh = System.Math.Cosh
        let Sinh = System.Math.Sinh
        let Power(a, b) = a**b
            
        match id with
        | "o" -> CSMat(4, [0, 0, 1., 0.; 1, 1, -0.49942 - 0.49942*Cos(1.*t), 0.02399 + 0.024*Cos(1.*t); 1, 2, 0. + 0.48168*Sin(1.*t), 0. - 0.51768*Sin(1.*t); 1, 3, 0.012 - 0.012*Cos(1.*t), 0.49986 - 0.49986*Cos(1.*t); 2, 1, 0. + 0.48168*Sin(1.*t), 0. - 0.51768*Sin(1.*t); 2, 2, 0. - 0.024*Cos(1.*t), 0. - 0.99972*Cos(1.*t); 2, 3, 0. + 0.50596*Sin(1.*t), 0. + 0.49396*Sin(1.*t); 3, 1, 0.012 - 0.012*Cos(1.*t), 0.49986 - 0.49986*Cos(1.*t); 3, 2, 0. + 0.50596*Sin(1.*t), 0. + 0.49396*Sin(1.*t); 3, 3, 0.5 + 0.5*Cos(1.*t), 0.])
        | "p" -> CSMat(4, [0, 0, 0. - 0.43978*Cos(1.*t), 0. - 0.8981*Cos(1.*t); 0, 2, 0., 0.; 0, 3, 0. + 0.84846*Sin(1.*t), 0. + 0.52926*Sin(1.*t); 1, 1, 1., 0.; 2, 0, 0., 0.; 2, 2, -0.43979, 0.8981; 3, 0, 0. + 0.84846*Sin(1.*t), 0. + 0.52926*Sin(1.*t); 3, 3, 1.*Cos(1.*t), 0.])
        | "q" -> CSMat(4, [0, 0, 1., 0.; 1, 1, 1., 0.; 2, 2, 0. - 0.70918*Cos(1.*t), 0. + 0.70502*Cos(1.*t); 2, 3, 0. - 0.92444*Sin(1.*t), 0. + 0.38132*Sin(1.*t); 3, 2, 0. - 0.92444*Sin(1.*t), 0. + 0.38132*Sin(1.*t); 3, 3, 1.*Cos(1.*t), 0.])
        | "r" -> CSMat(4, [0, 0, 0.44824 - 0.3571*Cos(1.*t), 0.22155 + 0.34996*Cos(1.*t); 0, 2, 0.16506 + 0.5*Cos(1.*t), 0.47197; 0, 3, 0. - 0.65464*Sin(1.*t), 0. + 0.2673*Sin(1.*t); 1, 1, 1., 0.; 2, 0, 0.16506 + 0.5*Cos(1.*t), 0.47197; 2, 2, -0.21246 - 0.3571*Cos(1.*t), 0.45261 - 0.34996*Cos(1.*t); 2, 3, 0. + 0.65464*Sin(1.*t), 0. + 0.2673*Sin(1.*t); 3, 0, 0. - 0.65464*Sin(1.*t), 0. + 0.2673*Sin(1.*t); 3, 2, 0. + 0.65464*Sin(1.*t), 0. + 0.2673*Sin(1.*t); 3, 3, 1.*Cos(1.*t), 0.])
        | "s" -> CSMat(4, [0, 0, 0. + 0.9786*Cos(0.5027*t), 0. + 0.20574*Cos(0.5027*t); 0, 3, 0. - 0.10342*Sin(0.5027*t), 0. + 0.99464*Sin(0.5027*t); 1, 1, 1.*Cos(1.*t), 0.; 1, 2, 0., 0. + 1.*Sin(1.*t); 2, 1, 0., 0. + 1.*Sin(1.*t); 2, 2, 0. + 1.*Cos(1.*t), 0.; 3, 0, 0. - 0.10342*Sin(0.5027*t), 0. + 0.99464*Sin(0.5027*t); 3, 3, 1.*Cos(0.5027*t), 0.])
        | _ -> failwith "Sparse Random Hermitian matrices not precomputed"
