// LiquidLearn (c) 2016 Johannes Bausch
module LiquidLearn.Utils

// check if list contains a
let contains a list = List.exists (fun element -> element = a) list

// cartesian product of a sequence of sequences
let rec cartesian (lists : 'a list list) = 
    let f0 a = function
        | [] -> [[a]]
        | lists -> List.map (fun list -> a::list) lists
    
    match lists with
    | [] -> []
    | head::tail -> List.collect (fun a -> f0 a (cartesian tail)) head

let tuples (list: 'a list) (n : int) =
    cartesian [ for i in 1..n -> list ]

// shortcut aliases
let join = String.concat
let inline (/@) f list = Seq.map f list |> Seq.toList

// dump function
let dump sth = printfn "%A" sth


// input and output data specification
module Data =
    type BitT = Zero=0 | One=1
    type StateT = BitT list
    type DataT = BitT list

    type Dataset =
        {
            Yes : DataT list
            No  : DataT list
        }
        member this.ValuatedList (weight : float) =
            [ for y in this.Yes -> (-weight, y) ] @ [ for n in this.No -> (weight, n)]
        member this.YesInstances = { Yes = this.Yes; No = [] }
        member this.NoInstances = { Yes = []; No = this.No }

    // parse from string or others
    let FromString string =
        [ for c in string do
            if c = '0' then yield BitT.Zero
            elif c = '1' then yield BitT.One
            elif c = ' ' then ()
            else failwith "invalid character in data string"
        ] |> List.rev

    // parse from file
    let FromFile filename =
        FromString /@ (System.IO.File.ReadLines filename)
        
    // flip bits
    let Flip (data:DataT) =
        [ for d in data ->
            if d = BitT.Zero then BitT.One
            else BitT.Zero
        ]
