// LiquidLearn (c) 2016 Johannes Bausch
// Auto-Generated File with gates.nb on Mon 8 Feb 2016 12:58:11
namespace LiquidLearn.GeneratedCode

module Matrices =
    open Microsoft.Research.Liquid
    let PauliProduct (id : string) t =
        // local mapping
        let Cos = System.Math.Cos
        let Sin = System.Math.Sin
            
        match id with
        | "00" -> CSMat(4, [0, 0, Cos(t), Sin(t); 1, 1, Cos(t), Sin(t); 2, 2, Cos(t), Sin(t); 3, 3, Cos(t), Sin(t)])
        | "01" -> CSMat(4, [0, 0, 2.*Cos(t), 0.; 0, 1, 0., 2.*Sin(t); 1, 0, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 2, 2, 2.*Cos(t), 0.; 2, 3, 0., 2.*Sin(t); 3, 2, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.])
        | "02" -> CSMat(4, [0, 0, -2.*Cos(t), 0.; 0, 1, 2.*Sin(t), 0.; 1, 0, 2.*Sin(t), 0.; 1, 1, 2.*Cos(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 3, 2.*Sin(t), 0.; 3, 2, 2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.])
        | "03" -> CSMat(4, [0, 0, Cos(t), Sin(t); 1, 1, Cos(t), -1.*Sin(t); 2, 2, Cos(t), Sin(t); 3, 3, Cos(t), -1.*Sin(t)])
        | "10" -> CSMat(4, [0, 0, 2.*Cos(t), 0.; 0, 2, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 3, 0., 2.*Sin(t); 2, 0, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 3, 1, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.])
        | "11" -> CSMat(4, [0, 0, 2.*Cos(t), 0.; 0, 3, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 2, 0., 2.*Sin(t); 2, 1, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 3, 0, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.])
        | "12" -> CSMat(4, [0, 0, -2.*Cos(t), 0.; 0, 3, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 2, -2.*Sin(t), 0.; 2, 1, -2.*Sin(t), 0.; 2, 2, 2.*Cos(t), 0.; 3, 0, 2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.])
        | "13" -> CSMat(4, [0, 0, 2.*Cos(t), 0.; 0, 2, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 3, 0., -2.*Sin(t); 2, 0, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 3, 1, 0., -2.*Sin(t); 3, 3, 2.*Cos(t), 0.])
        | "20" -> CSMat(4, [0, 0, -2.*Cos(t), 0.; 0, 2, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 3, 2.*Sin(t), 0.; 2, 0, 2.*Sin(t), 0.; 2, 2, 2.*Cos(t), 0.; 3, 1, 2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.])
        | "21" -> CSMat(4, [0, 0, -2.*Cos(t), 0.; 0, 3, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 2, 2.*Sin(t), 0.; 2, 1, 2.*Sin(t), 0.; 2, 2, 2.*Cos(t), 0.; 3, 0, 2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.])
        | "22" -> CSMat(4, [0, 0, 2.*Cos(t), 0.; 0, 3, 0., -2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 2, 0., 2.*Sin(t); 2, 1, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 3, 0, 0., -2.*Sin(t); 3, 3, 2.*Cos(t), 0.])
        | "23" -> CSMat(4, [0, 0, -2.*Cos(t), 0.; 0, 2, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 3, -2.*Sin(t), 0.; 2, 0, 2.*Sin(t), 0.; 2, 2, 2.*Cos(t), 0.; 3, 1, -2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.])
        | "30" -> CSMat(4, [0, 0, Cos(t), Sin(t); 1, 1, Cos(t), Sin(t); 2, 2, Cos(t), -1.*Sin(t); 3, 3, Cos(t), -1.*Sin(t)])
        | "31" -> CSMat(4, [0, 0, 2.*Cos(t), 0.; 0, 1, 0., 2.*Sin(t); 1, 0, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 2, 2, 2.*Cos(t), 0.; 2, 3, 0., -2.*Sin(t); 3, 2, 0., -2.*Sin(t); 3, 3, 2.*Cos(t), 0.])
        | "32" -> CSMat(4, [0, 0, -2.*Cos(t), 0.; 0, 1, 2.*Sin(t), 0.; 1, 0, 2.*Sin(t), 0.; 1, 1, 2.*Cos(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 3, -2.*Sin(t), 0.; 3, 2, -2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.])
        | "33" -> CSMat(4, [0, 0, Cos(t), Sin(t); 1, 1, Cos(t), -1.*Sin(t); 2, 2, Cos(t), -1.*Sin(t); 3, 3, Cos(t), Sin(t)])
        | "000" -> CSMat(8, [0, 0, Cos(t), Sin(t); 1, 1, Cos(t), Sin(t); 2, 2, Cos(t), Sin(t); 3, 3, Cos(t), Sin(t); 4, 4, Cos(t), Sin(t); 5, 5, Cos(t), Sin(t); 6, 6, Cos(t), Sin(t); 7, 7, Cos(t), Sin(t)])
        | "001" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 1, 0., 2.*Sin(t); 1, 0, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 2, 2, 2.*Cos(t), 0.; 2, 3, 0., 2.*Sin(t); 3, 2, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 4, 4, 2.*Cos(t), 0.; 4, 5, 0., 2.*Sin(t); 5, 4, 0., 2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 6, 2.*Cos(t), 0.; 6, 7, 0., 2.*Sin(t); 7, 6, 0., 2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "002" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 1, 2.*Sin(t), 0.; 1, 0, 2.*Sin(t), 0.; 1, 1, 2.*Cos(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 3, 2.*Sin(t), 0.; 3, 2, 2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.; 4, 4, -2.*Cos(t), 0.; 4, 5, 2.*Sin(t), 0.; 5, 4, 2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 6, -2.*Cos(t), 0.; 6, 7, 2.*Sin(t), 0.; 7, 6, 2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "003" -> CSMat(8, [0, 0, Cos(t), Sin(t); 1, 1, Cos(t), -1.*Sin(t); 2, 2, Cos(t), Sin(t); 3, 3, Cos(t), -1.*Sin(t); 4, 4, Cos(t), Sin(t); 5, 5, Cos(t), -1.*Sin(t); 6, 6, Cos(t), Sin(t); 7, 7, Cos(t), -1.*Sin(t)])
        | "010" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 2, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 3, 0., 2.*Sin(t); 2, 0, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 3, 1, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 4, 4, 2.*Cos(t), 0.; 4, 6, 0., 2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 5, 7, 0., 2.*Sin(t); 6, 4, 0., 2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 5, 0., 2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "011" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 3, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 2, 0., 2.*Sin(t); 2, 1, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 3, 0, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 4, 4, 2.*Cos(t), 0.; 4, 7, 0., 2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 5, 6, 0., 2.*Sin(t); 6, 5, 0., 2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 4, 0., 2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "012" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 3, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 2, -2.*Sin(t), 0.; 2, 1, -2.*Sin(t), 0.; 2, 2, 2.*Cos(t), 0.; 3, 0, 2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.; 4, 4, -2.*Cos(t), 0.; 4, 7, 2.*Sin(t), 0.; 5, 5, -2.*Cos(t), 0.; 5, 6, -2.*Sin(t), 0.; 6, 5, -2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 4, 2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "013" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 2, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 3, 0., -2.*Sin(t); 2, 0, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 3, 1, 0., -2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 4, 4, 2.*Cos(t), 0.; 4, 6, 0., 2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 5, 7, 0., -2.*Sin(t); 6, 4, 0., 2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 5, 0., -2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "020" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 2, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 3, 2.*Sin(t), 0.; 2, 0, 2.*Sin(t), 0.; 2, 2, 2.*Cos(t), 0.; 3, 1, 2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.; 4, 4, -2.*Cos(t), 0.; 4, 6, 2.*Sin(t), 0.; 5, 5, -2.*Cos(t), 0.; 5, 7, 2.*Sin(t), 0.; 6, 4, 2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 5, 2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "021" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 3, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 2, 2.*Sin(t), 0.; 2, 1, 2.*Sin(t), 0.; 2, 2, 2.*Cos(t), 0.; 3, 0, 2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.; 4, 4, -2.*Cos(t), 0.; 4, 7, 2.*Sin(t), 0.; 5, 5, -2.*Cos(t), 0.; 5, 6, 2.*Sin(t), 0.; 6, 5, 2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 4, 2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "022" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 3, 0., -2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 2, 0., 2.*Sin(t); 2, 1, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 3, 0, 0., -2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 4, 4, 2.*Cos(t), 0.; 4, 7, 0., -2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 5, 6, 0., 2.*Sin(t); 6, 5, 0., 2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 4, 0., -2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "023" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 2, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 3, -2.*Sin(t), 0.; 2, 0, 2.*Sin(t), 0.; 2, 2, 2.*Cos(t), 0.; 3, 1, -2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.; 4, 4, -2.*Cos(t), 0.; 4, 6, 2.*Sin(t), 0.; 5, 5, -2.*Cos(t), 0.; 5, 7, -2.*Sin(t), 0.; 6, 4, 2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 5, -2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "030" -> CSMat(8, [0, 0, Cos(t), Sin(t); 1, 1, Cos(t), Sin(t); 2, 2, Cos(t), -1.*Sin(t); 3, 3, Cos(t), -1.*Sin(t); 4, 4, Cos(t), Sin(t); 5, 5, Cos(t), Sin(t); 6, 6, Cos(t), -1.*Sin(t); 7, 7, Cos(t), -1.*Sin(t)])
        | "031" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 1, 0., 2.*Sin(t); 1, 0, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 2, 2, 2.*Cos(t), 0.; 2, 3, 0., -2.*Sin(t); 3, 2, 0., -2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 4, 4, 2.*Cos(t), 0.; 4, 5, 0., 2.*Sin(t); 5, 4, 0., 2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 6, 2.*Cos(t), 0.; 6, 7, 0., -2.*Sin(t); 7, 6, 0., -2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "032" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 1, 2.*Sin(t), 0.; 1, 0, 2.*Sin(t), 0.; 1, 1, 2.*Cos(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 3, -2.*Sin(t), 0.; 3, 2, -2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.; 4, 4, -2.*Cos(t), 0.; 4, 5, 2.*Sin(t), 0.; 5, 4, 2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 6, -2.*Cos(t), 0.; 6, 7, -2.*Sin(t), 0.; 7, 6, -2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "033" -> CSMat(8, [0, 0, Cos(t), Sin(t); 1, 1, Cos(t), -1.*Sin(t); 2, 2, Cos(t), -1.*Sin(t); 3, 3, Cos(t), Sin(t); 4, 4, Cos(t), Sin(t); 5, 5, Cos(t), -1.*Sin(t); 6, 6, Cos(t), -1.*Sin(t); 7, 7, Cos(t), Sin(t)])
        | "100" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 4, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 5, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 2, 6, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 3, 7, 0., 2.*Sin(t); 4, 0, 0., 2.*Sin(t); 4, 4, 2.*Cos(t), 0.; 5, 1, 0., 2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 2, 0., 2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 3, 0., 2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "101" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 5, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 4, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 2, 7, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 3, 6, 0., 2.*Sin(t); 4, 1, 0., 2.*Sin(t); 4, 4, 2.*Cos(t), 0.; 5, 0, 0., 2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 3, 0., 2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 2, 0., 2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "102" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 5, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 4, -2.*Sin(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 7, 2.*Sin(t), 0.; 3, 3, -2.*Cos(t), 0.; 3, 6, -2.*Sin(t), 0.; 4, 1, -2.*Sin(t), 0.; 4, 4, 2.*Cos(t), 0.; 5, 0, 2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 3, -2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 2, 2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "103" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 4, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 5, 0., -2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 2, 6, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 3, 7, 0., -2.*Sin(t); 4, 0, 0., 2.*Sin(t); 4, 4, 2.*Cos(t), 0.; 5, 1, 0., -2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 2, 0., 2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 3, 0., -2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "110" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 6, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 7, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 2, 4, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 3, 5, 0., 2.*Sin(t); 4, 2, 0., 2.*Sin(t); 4, 4, 2.*Cos(t), 0.; 5, 3, 0., 2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 0, 0., 2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 1, 0., 2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "111" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 7, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 6, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 2, 5, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 3, 4, 0., 2.*Sin(t); 4, 3, 0., 2.*Sin(t); 4, 4, 2.*Cos(t), 0.; 5, 2, 0., 2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 1, 0., 2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 0, 0., 2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "112" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 7, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 6, -2.*Sin(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 5, 2.*Sin(t), 0.; 3, 3, -2.*Cos(t), 0.; 3, 4, -2.*Sin(t), 0.; 4, 3, -2.*Sin(t), 0.; 4, 4, 2.*Cos(t), 0.; 5, 2, 2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 1, -2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 0, 2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "113" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 6, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 7, 0., -2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 2, 4, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 3, 5, 0., -2.*Sin(t); 4, 2, 0., 2.*Sin(t); 4, 4, 2.*Cos(t), 0.; 5, 3, 0., -2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 0, 0., 2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 1, 0., -2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "120" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 6, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 7, 2.*Sin(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 4, -2.*Sin(t), 0.; 3, 3, -2.*Cos(t), 0.; 3, 5, -2.*Sin(t), 0.; 4, 2, -2.*Sin(t), 0.; 4, 4, 2.*Cos(t), 0.; 5, 3, -2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 0, 2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 1, 2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "121" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 7, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 6, 2.*Sin(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 5, -2.*Sin(t), 0.; 3, 3, -2.*Cos(t), 0.; 3, 4, -2.*Sin(t), 0.; 4, 3, -2.*Sin(t), 0.; 4, 4, 2.*Cos(t), 0.; 5, 2, -2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 1, 2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 0, 2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "122" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 7, 0., -2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 6, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 2, 5, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 3, 4, 0., -2.*Sin(t); 4, 3, 0., -2.*Sin(t); 4, 4, 2.*Cos(t), 0.; 5, 2, 0., 2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 1, 0., 2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 0, 0., -2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "123" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 6, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 7, -2.*Sin(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 4, -2.*Sin(t), 0.; 3, 3, -2.*Cos(t), 0.; 3, 5, 2.*Sin(t), 0.; 4, 2, -2.*Sin(t), 0.; 4, 4, 2.*Cos(t), 0.; 5, 3, 2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 0, 2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 1, -2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "130" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 4, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 5, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 2, 6, 0., -2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 3, 7, 0., -2.*Sin(t); 4, 0, 0., 2.*Sin(t); 4, 4, 2.*Cos(t), 0.; 5, 1, 0., 2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 2, 0., -2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 3, 0., -2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "131" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 5, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 4, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 2, 7, 0., -2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 3, 6, 0., -2.*Sin(t); 4, 1, 0., 2.*Sin(t); 4, 4, 2.*Cos(t), 0.; 5, 0, 0., 2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 3, 0., -2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 2, 0., -2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "132" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 5, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 4, -2.*Sin(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 7, -2.*Sin(t), 0.; 3, 3, -2.*Cos(t), 0.; 3, 6, 2.*Sin(t), 0.; 4, 1, -2.*Sin(t), 0.; 4, 4, 2.*Cos(t), 0.; 5, 0, 2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 3, 2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 2, -2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "133" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 4, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 5, 0., -2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 2, 6, 0., -2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 3, 7, 0., 2.*Sin(t); 4, 0, 0., 2.*Sin(t); 4, 4, 2.*Cos(t), 0.; 5, 1, 0., -2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 2, 0., -2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 3, 0., 2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "200" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 4, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 5, 2.*Sin(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 6, 2.*Sin(t), 0.; 3, 3, -2.*Cos(t), 0.; 3, 7, 2.*Sin(t), 0.; 4, 0, 2.*Sin(t), 0.; 4, 4, 2.*Cos(t), 0.; 5, 1, 2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 2, 2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 3, 2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "201" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 5, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 4, 2.*Sin(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 7, 2.*Sin(t), 0.; 3, 3, -2.*Cos(t), 0.; 3, 6, 2.*Sin(t), 0.; 4, 1, 2.*Sin(t), 0.; 4, 4, 2.*Cos(t), 0.; 5, 0, 2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 3, 2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 2, 2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "202" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 5, 0., -2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 4, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 2, 7, 0., -2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 3, 6, 0., 2.*Sin(t); 4, 1, 0., 2.*Sin(t); 4, 4, 2.*Cos(t), 0.; 5, 0, 0., -2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 3, 0., 2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 2, 0., -2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "203" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 4, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 5, -2.*Sin(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 6, 2.*Sin(t), 0.; 3, 3, -2.*Cos(t), 0.; 3, 7, -2.*Sin(t), 0.; 4, 0, 2.*Sin(t), 0.; 4, 4, 2.*Cos(t), 0.; 5, 1, -2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 2, 2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 3, -2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "210" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 6, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 7, 2.*Sin(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 4, 2.*Sin(t), 0.; 3, 3, -2.*Cos(t), 0.; 3, 5, 2.*Sin(t), 0.; 4, 2, 2.*Sin(t), 0.; 4, 4, 2.*Cos(t), 0.; 5, 3, 2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 0, 2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 1, 2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "211" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 7, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 6, 2.*Sin(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 5, 2.*Sin(t), 0.; 3, 3, -2.*Cos(t), 0.; 3, 4, 2.*Sin(t), 0.; 4, 3, 2.*Sin(t), 0.; 4, 4, 2.*Cos(t), 0.; 5, 2, 2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 1, 2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 0, 2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "212" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 7, 0., -2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 6, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 2, 5, 0., -2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 3, 4, 0., 2.*Sin(t); 4, 3, 0., 2.*Sin(t); 4, 4, 2.*Cos(t), 0.; 5, 2, 0., -2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 1, 0., 2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 0, 0., -2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "213" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 6, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 7, -2.*Sin(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 4, 2.*Sin(t), 0.; 3, 3, -2.*Cos(t), 0.; 3, 5, -2.*Sin(t), 0.; 4, 2, 2.*Sin(t), 0.; 4, 4, 2.*Cos(t), 0.; 5, 3, -2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 0, 2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 1, -2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "220" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 6, 0., -2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 7, 0., -2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 2, 4, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 3, 5, 0., 2.*Sin(t); 4, 2, 0., 2.*Sin(t); 4, 4, 2.*Cos(t), 0.; 5, 3, 0., 2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 0, 0., -2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 1, 0., -2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "221" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 7, 0., -2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 6, 0., -2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 2, 5, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 3, 4, 0., 2.*Sin(t); 4, 3, 0., 2.*Sin(t); 4, 4, 2.*Cos(t), 0.; 5, 2, 0., 2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 1, 0., -2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 0, 0., -2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "222" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 7, -2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 6, 2.*Sin(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 5, 2.*Sin(t), 0.; 3, 3, -2.*Cos(t), 0.; 3, 4, -2.*Sin(t), 0.; 4, 3, -2.*Sin(t), 0.; 4, 4, 2.*Cos(t), 0.; 5, 2, 2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 1, 2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 0, -2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "223" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 6, 0., -2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 7, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 2, 4, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 3, 5, 0., -2.*Sin(t); 4, 2, 0., 2.*Sin(t); 4, 4, 2.*Cos(t), 0.; 5, 3, 0., -2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 0, 0., -2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 1, 0., 2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "230" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 4, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 5, 2.*Sin(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 6, -2.*Sin(t), 0.; 3, 3, -2.*Cos(t), 0.; 3, 7, -2.*Sin(t), 0.; 4, 0, 2.*Sin(t), 0.; 4, 4, 2.*Cos(t), 0.; 5, 1, 2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 2, -2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 3, -2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "231" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 5, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 4, 2.*Sin(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 7, -2.*Sin(t), 0.; 3, 3, -2.*Cos(t), 0.; 3, 6, -2.*Sin(t), 0.; 4, 1, 2.*Sin(t), 0.; 4, 4, 2.*Cos(t), 0.; 5, 0, 2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 3, -2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 2, -2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "232" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 5, 0., -2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 4, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 2, 7, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 3, 6, 0., -2.*Sin(t); 4, 1, 0., 2.*Sin(t); 4, 4, 2.*Cos(t), 0.; 5, 0, 0., -2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 3, 0., -2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 2, 0., 2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "233" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 4, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 5, -2.*Sin(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 6, -2.*Sin(t), 0.; 3, 3, -2.*Cos(t), 0.; 3, 7, 2.*Sin(t), 0.; 4, 0, 2.*Sin(t), 0.; 4, 4, 2.*Cos(t), 0.; 5, 1, -2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 2, -2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 3, 2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "300" -> CSMat(8, [0, 0, Cos(t), Sin(t); 1, 1, Cos(t), Sin(t); 2, 2, Cos(t), Sin(t); 3, 3, Cos(t), Sin(t); 4, 4, Cos(t), -1.*Sin(t); 5, 5, Cos(t), -1.*Sin(t); 6, 6, Cos(t), -1.*Sin(t); 7, 7, Cos(t), -1.*Sin(t)])
        | "301" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 1, 0., 2.*Sin(t); 1, 0, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 2, 2, 2.*Cos(t), 0.; 2, 3, 0., 2.*Sin(t); 3, 2, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 4, 4, 2.*Cos(t), 0.; 4, 5, 0., -2.*Sin(t); 5, 4, 0., -2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 6, 2.*Cos(t), 0.; 6, 7, 0., -2.*Sin(t); 7, 6, 0., -2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "302" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 1, 2.*Sin(t), 0.; 1, 0, 2.*Sin(t), 0.; 1, 1, 2.*Cos(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 3, 2.*Sin(t), 0.; 3, 2, 2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.; 4, 4, -2.*Cos(t), 0.; 4, 5, -2.*Sin(t), 0.; 5, 4, -2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 6, -2.*Cos(t), 0.; 6, 7, -2.*Sin(t), 0.; 7, 6, -2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "303" -> CSMat(8, [0, 0, Cos(t), Sin(t); 1, 1, Cos(t), -1.*Sin(t); 2, 2, Cos(t), Sin(t); 3, 3, Cos(t), -1.*Sin(t); 4, 4, Cos(t), -1.*Sin(t); 5, 5, Cos(t), Sin(t); 6, 6, Cos(t), -1.*Sin(t); 7, 7, Cos(t), Sin(t)])
        | "310" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 2, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 3, 0., 2.*Sin(t); 2, 0, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 3, 1, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 4, 4, 2.*Cos(t), 0.; 4, 6, 0., -2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 5, 7, 0., -2.*Sin(t); 6, 4, 0., -2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 5, 0., -2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "311" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 3, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 2, 0., 2.*Sin(t); 2, 1, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 3, 0, 0., 2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 4, 4, 2.*Cos(t), 0.; 4, 7, 0., -2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 5, 6, 0., -2.*Sin(t); 6, 5, 0., -2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 4, 0., -2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "312" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 3, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 2, -2.*Sin(t), 0.; 2, 1, -2.*Sin(t), 0.; 2, 2, 2.*Cos(t), 0.; 3, 0, 2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.; 4, 4, -2.*Cos(t), 0.; 4, 7, -2.*Sin(t), 0.; 5, 5, -2.*Cos(t), 0.; 5, 6, 2.*Sin(t), 0.; 6, 5, 2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 4, -2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "313" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 2, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 3, 0., -2.*Sin(t); 2, 0, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 3, 1, 0., -2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 4, 4, 2.*Cos(t), 0.; 4, 6, 0., -2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 5, 7, 0., 2.*Sin(t); 6, 4, 0., -2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 5, 0., 2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "320" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 2, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 3, 2.*Sin(t), 0.; 2, 0, 2.*Sin(t), 0.; 2, 2, 2.*Cos(t), 0.; 3, 1, 2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.; 4, 4, -2.*Cos(t), 0.; 4, 6, -2.*Sin(t), 0.; 5, 5, -2.*Cos(t), 0.; 5, 7, -2.*Sin(t), 0.; 6, 4, -2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 5, -2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "321" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 3, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 2, 2.*Sin(t), 0.; 2, 1, 2.*Sin(t), 0.; 2, 2, 2.*Cos(t), 0.; 3, 0, 2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.; 4, 4, -2.*Cos(t), 0.; 4, 7, -2.*Sin(t), 0.; 5, 5, -2.*Cos(t), 0.; 5, 6, -2.*Sin(t), 0.; 6, 5, -2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 4, -2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "322" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 3, 0., -2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 1, 2, 0., 2.*Sin(t); 2, 1, 0., 2.*Sin(t); 2, 2, 2.*Cos(t), 0.; 3, 0, 0., -2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 4, 4, 2.*Cos(t), 0.; 4, 7, 0., 2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 5, 6, 0., -2.*Sin(t); 6, 5, 0., -2.*Sin(t); 6, 6, 2.*Cos(t), 0.; 7, 4, 0., 2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "323" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 2, 2.*Sin(t), 0.; 1, 1, -2.*Cos(t), 0.; 1, 3, -2.*Sin(t), 0.; 2, 0, 2.*Sin(t), 0.; 2, 2, 2.*Cos(t), 0.; 3, 1, -2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.; 4, 4, -2.*Cos(t), 0.; 4, 6, -2.*Sin(t), 0.; 5, 5, -2.*Cos(t), 0.; 5, 7, 2.*Sin(t), 0.; 6, 4, -2.*Sin(t), 0.; 6, 6, 2.*Cos(t), 0.; 7, 5, 2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "330" -> CSMat(8, [0, 0, Cos(t), Sin(t); 1, 1, Cos(t), Sin(t); 2, 2, Cos(t), -1.*Sin(t); 3, 3, Cos(t), -1.*Sin(t); 4, 4, Cos(t), -1.*Sin(t); 5, 5, Cos(t), -1.*Sin(t); 6, 6, Cos(t), Sin(t); 7, 7, Cos(t), Sin(t)])
        | "331" -> CSMat(8, [0, 0, 2.*Cos(t), 0.; 0, 1, 0., 2.*Sin(t); 1, 0, 0., 2.*Sin(t); 1, 1, 2.*Cos(t), 0.; 2, 2, 2.*Cos(t), 0.; 2, 3, 0., -2.*Sin(t); 3, 2, 0., -2.*Sin(t); 3, 3, 2.*Cos(t), 0.; 4, 4, 2.*Cos(t), 0.; 4, 5, 0., -2.*Sin(t); 5, 4, 0., -2.*Sin(t); 5, 5, 2.*Cos(t), 0.; 6, 6, 2.*Cos(t), 0.; 6, 7, 0., 2.*Sin(t); 7, 6, 0., 2.*Sin(t); 7, 7, 2.*Cos(t), 0.])
        | "332" -> CSMat(8, [0, 0, -2.*Cos(t), 0.; 0, 1, 2.*Sin(t), 0.; 1, 0, 2.*Sin(t), 0.; 1, 1, 2.*Cos(t), 0.; 2, 2, -2.*Cos(t), 0.; 2, 3, -2.*Sin(t), 0.; 3, 2, -2.*Sin(t), 0.; 3, 3, 2.*Cos(t), 0.; 4, 4, -2.*Cos(t), 0.; 4, 5, -2.*Sin(t), 0.; 5, 4, -2.*Sin(t), 0.; 5, 5, 2.*Cos(t), 0.; 6, 6, -2.*Cos(t), 0.; 6, 7, 2.*Sin(t), 0.; 7, 6, 2.*Sin(t), 0.; 7, 7, 2.*Cos(t), 0.])
        | "333" -> CSMat(8, [0, 0, Cos(t), Sin(t); 1, 1, Cos(t), -1.*Sin(t); 2, 2, Cos(t), -1.*Sin(t); 3, 3, Cos(t), Sin(t); 4, 4, Cos(t), -1.*Sin(t); 5, 5, Cos(t), Sin(t); 6, 6, Cos(t), Sin(t); 7, 7, Cos(t), -1.*Sin(t)])
        | _ -> failwith "PauliProduct not precomputed"
