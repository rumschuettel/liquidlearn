// LiquidLearn (c) 2016 Johannes Bausch
module LiquidLearn.Graph

open Utils

type VertexT =
    | V of string
    | O of string
    | C of string * string

type EdgeT = VertexT list 
let NormalOrderEdge edge =
    Seq.sortBy (fun vertex ->
        match vertex with
        | C(a, b) -> 0
        | O(a) -> 2
        | _ -> 1
    ) edge |> Seq.toList

let inline (---) a b = 
    unbox<VertexT> /@ (
        match box a, box b with
        | (:? list<VertexT> as a), (:? list<VertexT> as b) -> [a; b] |> List.concat
        | (:? VertexT as a), (:? list<VertexT> as b) -> [[a]; b] |> List.concat
        | (:? list<VertexT> as a), (:? VertexT as b) -> [a; [b]] |> List.concat
        | (:? VertexT as a), (:? VertexT as b) -> [a; b]
        | _ -> failwith "not a valid hyperedge type"
    )

let randomStr = 
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWUXYZ0123456789"
    let charsLen = chars.Length
    let random = System.Random()

    fun len -> 
        let randomChars = [|for i in 0..len -> chars.[random.Next(charsLen)]|]
        new System.String(randomChars)


type Hypergraph(edges : EdgeT list) = class
    // put edges in correct order, extract unique vertices, output and control
    let edges = NormalOrderEdge /@ edges
    let vertices = edges |> List.concat |> Set.ofList |> Set.toList
    let output =
        [
            for v in vertices do
                match v with
                | O x -> yield O x
                | _ -> ()
        ]
    let control =
        [
            for v in vertices do
                match v with
                | C (x, y) -> yield C (x, y)
                | _ -> ()
        ]
    // check that all output vertices are contained in vertex list
    do
        if (Set.difference (Set.ofList output) (Set.ofList vertices)).Count > 0 then failwith "some disconnected output vertices"

    member this.whichQubit (name : VertexT) = List.findIndex (fun v -> v = name) vertices

    // prints out the hypergraph structure
    override this.ToString() = sprintf "vertices:\n%A\nedges:\n%A" vertices edges

    // vertices and edges
    member this.Edges = edges
    member this.Vertices = this.whichQubit /@ vertices
    member this.Size = vertices.Length

    // get a list of interactions of the form [[a0; ... an]; ... [x0; ... xm]]
    member this.Interactions = (fun e -> this.whichQubit /@ e) /@ edges

    // get a list of output or control qubits in the form [q0; q1; ... ; qn]
    member this.Outputs = this.whichQubit /@ output
    member this.Controls = this.whichQubit /@ control

    // return graph where either every or only some hyperedges are extended by one or more control vertices
    member this.ControlGraph (f : EdgeT -> string list) =
        new Hypergraph
            ([
                for e in edges do
                    for s in (f e) do
                        yield C(s, randomStr 5) --- e            
            ])


end

