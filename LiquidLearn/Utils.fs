// LiquidLearn (c) 2016 Johannes Bausch
module LiquidLearn.Utils


// math stuff
let rand = new System.Random()
let log2 n = System.Math.Log(float n, 2.0)
let flog2 n = System.Math.Log(n, 2.0)
let nextPowerOf2 n = 2.0**(log2 n |> ceil) |> round |> int


// aliases, shortcuts and F# greatness
let join = String.concat


type PipeForwardT = PipeForwardT with
    static member inline ($) (PipeForwardT, list: seq<'a>) = fun f -> list |> Seq.map f
    static member inline ($) (PipeForwardT, list: 'a []) = fun f -> list |> Array.map f
    static member inline ($) (PipeForwardT, list: Set<'a>) = fun f -> list |> Set.map f
    static member inline ($) (PipeForwardT, list: 'a list) = fun f -> list |> List.map f
    static member inline ($) (PipeForwardT, (a, b): 'a * 'a) = fun f -> [a; b] |> List.map f |> function | [a; b] -> (a, b) | _ -> failwith "does not happen"

let inline (||>) list f = (PipeForwardT $ list) f
let inline (|||>) listofLists f = listofLists ||> (fun list -> list ||> f)
let inline (||||>) listofListsofLists f = listofListsofLists |||> (fun listofLists -> listofLists ||> f)

let inline (<||) f list = list ||> f


// shuffle array
let shuffle a =
    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp

    Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a
    a



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

// clamp list
let clamp a b list = list |> List.map (fun v -> if v > b then b elif v < a then a else v)

// cartesian product of a list of lists
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
    | x::xs' as xs -> (e::xs) :: [for xs in distribute e xs' -> x::xs]

// all permutations of list
let rec permutations = function
    | [] -> [[]]
    | e::xs -> List.collect (distribute e) (permutations xs)

// get permutation function mapping a to b
let getPermutation (l1 : 'a list) (l2 : 'a list) : int -> int =
    if l1.Length <> l2.Length then failwith "no permutation found"
    if l1.Length = 0 then id
    else
        let matrix = [|for i in 0..l1.Length-1 -> [|for j in 0..l2.Length-1 -> l1.[i] = l2.[j]|]|]
        for i = 0 to matrix.Length-1 do
            for j = i+1 to matrix.Length-1 do
                // for all rows after i that are identical, delete the first 'true'
               if matrix.[i] = matrix.[j] then
                    match Array.tryFindIndex id matrix.[j] with
                    | None -> failwith "no permutation found"
                    | Some n -> 
                        matrix.[j].[n] <- false

        let lookup =
            matrix
            ||> Array.tryFindIndex id
            ||> function Some n -> n | None -> failwith "no permutation found"

        function n when ((0 <= n) && (n < lookup.Length)) -> lookup.[n] | n -> n
            
// powerset
let rec powerset list = 
  seq {
    match list with
    | [] -> yield []
    | h::t -> for x in powerset t do yield! [x; h::x]
  }



// patterns
let (|EmptySet|_|) (set : Set<'T>) = 
    if set.IsEmpty then Some() else None



// dump function
let dumpsWithColor color sth =
    let oldColor = System.Console.ForegroundColor
    System.Console.ForegroundColor <- color
    printfn "%s" sth
    System.Console.ForegroundColor <- oldColor
let Dumps = dumpsWithColor System.ConsoleColor.Magenta
let dumps = dumpsWithColor System.ConsoleColor.White
let Dump sth = dumpsWithColor System.ConsoleColor.Yellow (sprintf "%A" sth)
let dump sth =
    dumps (sprintf "%A" sth)

// unique incrementing id
let mutable internal _uniqueIDCounter = 0
let UniqueID =
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
            "\n  yes: " + (join ", " (this.Yes |||> int |||> string ||> join "" ||> sprintf "%s")) +
            "\n  no:  " + (join ", " (this.No  |||> int |||> string ||> join "" ||> sprintf "%s"))


        member this.ValuatedList ((weightYes : float, weightNo : float), ?jitter) =
            let jitter = defaultArg jitter 0.00 // do not randomize weights by default
            [
                for y in this.Yes ->
                    (weightYes * (jitter * rand.NextDouble() + (1.-jitter)), y)
            ] @ [
                for n in this.No ->
                    (weightNo * (jitter * rand.NextDouble() + (1.-jitter)), n)
            ]
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
