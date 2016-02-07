// LiquidLearn (c) 2016 Johannes Bausch
module LiquidLearn.Utils

// check if list contains a
let contains a list = List.exists (fun element -> element = a) list
    
// shortcut aliases
let join = String.concat
let inline (/@) f list = List.map f list

// dump function
let dump sth = printfn "%A" sth


// input and output data specification
module Data =
    type BitT = Zero=0 | One=1
    type DataT = BitT list

    type Dataset =
        {
            Yes : DataT list
            No  : DataT list
        }
        member this.ValuatedList (weight : float) =
            [ for y in this.Yes -> (weight, y) ] @ [ for n in this.No -> (-weight, n)]

    // parse from string or others
    let Parse (str:string) =
        [ for c in str do
            if c = '0' then yield BitT.Zero
            elif c = '1' then yield BitT.One
            elif c = ' ' then ()
            else failwith "invalid character in data string"
        ] |> List.rev

    // flip bits
    let Flip (data:DataT) =
        [ for d in data ->
            if d = BitT.Zero then BitT.One
            else BitT.Zero
        ]
