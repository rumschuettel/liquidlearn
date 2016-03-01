// LiquidLearn (c) 2016 Johannes Bausch
module LiquidLearn.Utils

// check if list contains a
let contains a list = List.exists (fun element -> element = a) list

// return index of maximum element
let maxIndex seq =  
    seq
    |> Seq.mapi (fun i x -> i, x)
    |> Seq.maxBy snd 
    |> fst

// normalize list
let normalizeBy f list =
    let max = f list |> abs
    match max with
    | 0.0 -> failwith "list of zeroes cannot be normalized"
    | _ -> list |> List.map (fun v -> v / max)
let normalize list = normalizeBy List.max list

// cartesian product of a sequence of sequences
let rec cartesian (lists : 'a list list) = 
    let f0 a = function
        | [] -> [[a]]
        | lists -> List.map (fun list -> a::list) lists
    
    match lists with
    | [] -> []
    | head::tail -> List.collect (fun a -> f0 a (cartesian tail)) head

// tuples of size n
let tuples n (list: 'a list) =
    cartesian [ for i in 1..n -> list ]

// distribute element across list
let rec distribute e = function
    | [] -> [[e]]
    | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]

// all permutations of list
let rec permute = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permute xs)


// shortcut aliases
let join = String.concat
let inline (<||) f list = Seq.map f list |> Seq.toList
let inline (||>) list f = Seq.map f list |> Seq.toList
let inline (|||>) listofLists f = listofLists ||> (fun list -> list ||> f)
let inline (||||>) listofLists f = listofLists |||> (fun list -> list ||> f)

// math stuff
let log2 n = System.Math.Log(float n, 2.0)
let nextPowerOf2 n = 2.0**(log2 n |> ceil) |> round |> int

// dump function
let dumpsWithColor color sth =
    let oldColor = System.Console.ForegroundColor
    System.Console.ForegroundColor <- color
    printfn "%s" sth
    System.Console.ForegroundColor <- oldColor
let Dumps = dumpsWithColor System.ConsoleColor.DarkMagenta
let dumps = dumpsWithColor System.ConsoleColor.White
let dump sth =
    dumps (sprintf "%A" sth)

// unique incrementing id
let mutable internal _uniqueIDCounter = 0
let uniqueID =
    _uniqueIDCounter <- _uniqueIDCounter + 1
    string _uniqueIDCounter

// input and output data specification
module Data =
    type BitT = Zero=0 | One=1
    type StateT = BitT list        
    type DataT = StateT

    [<StructuredFormatDisplay("DataSet {TableForm}")>]
    type DataSet =
        {
            Yes : DataT list
            No  : DataT list
        }
        member this.TableForm =
            "\nyes: " + (join ", " (this.Yes |||> int |||> string ||> join "" ||> sprintf "%s")) +
            "\nno:  " + (join ", " (this.No  |||> int |||> string ||> join "" ||> sprintf "%s"))


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
        ]

    // parse from file
    let FromFile filename =
        FromString <|| (System.IO.File.ReadLines filename)
        
    // flip bits
    let Flip (data:DataT) =
        [ for d in data ->
            if d = BitT.Zero then BitT.One
            else BitT.Zero
        ]
