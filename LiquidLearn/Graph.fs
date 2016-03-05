// LiquidLearn (c) 2016 Johannes Bausch
module LiquidLearn.Graph

open Utils

// recursive vertex type
[<StructuredFormatDisplay("{DisplayForm}")>]
type VertexT =
    | V of string
    | O of string
    | C of ControlT
    with
    member this.DisplayForm =
        match this with
        | V v -> v
        | O o -> "*" + o
        | C c -> sprintf "c%s (%d)" c.id c.interactions.Length

    member this.IsControl =
        match this with
        | C _ -> true
        | _ -> false

    member this.Unwrap =
        match this with
        | C c -> c
        | _ -> failwith "not a control vertex"

    // return number of qubits necessary to represent vertex
    member this.HowManyQubits =
        match this with
        | C c -> System.Math.Log(float c.interactions.Length + 1.0, 2.0) |> ceil |> int
        | _ -> 1
and ControlT = {
    id : string
    interactions : InteractionT list // control vertex stores a list of interaction identifiers
}
and InteractionT = {
    name : string
    vertices : VertexT list
}

// edge as a vertex list
[<StructuredFormatDisplay("({DisplayForm})")>]
type EdgeT = EdgeT of Set<VertexT>
    with
    member this.DisplayForm = join " --- " (this.NormalOrder ||> sprintf "%A")

    member this.Vertices = match this with | EdgeT vertices -> vertices

    // true if edge has a controlled vertex
    member this.IsControlled =
        Set.exists (fun (vertex : VertexT) -> vertex.IsControl) this.Vertices

    // get control interaction
    member this.GetControl =
        match Set.filter (fun (vertex : VertexT) -> vertex.IsControl) this.Vertices |> Set.toList with
        | [] -> failwith "edge has no control vertex"
        | [c] -> c
        | _ -> failwith "edge has more than one control vertex"

    // strip control interaction
    member this.StripControl =
        this.Vertices
            |> Set.filter (fun vertex -> not vertex.IsControl)
            |> EdgeT


    // put the control vertex into normal order:
    // put controls to however controlled interactions are created,
    // i.e. in this case at the beginning, meaning that the control should be in the upper left block of any matrix
    // note that this does not mean that the control qubits have to be the first vertices of a graph
    member this.NormalOrder =
        this.Vertices
            |> Set.toList
            |> List.sortBy (fun vertex ->
                    match vertex with
                    | C _ -> 0
                    | _ -> 1
                )

    member this.Size = this.Vertices.Count


// F# does not support overloading non-tuple operators
let inline (---) a b = 
    (
        match box a, box b with
        | (:? EdgeT as a), (:? EdgeT as b) -> Set.union a.Vertices b.Vertices
        | (:? VertexT as a), (:? EdgeT as b) -> b.Vertices.Add a
        | (:? EdgeT as a), (:? VertexT as b) -> a.Vertices.Add b
        | (:? VertexT as a), (:? VertexT as b) -> Set.ofList [a; b]
        | _ -> failwith "not a valid hyperedge type"
    )
    ||> unbox<VertexT>
    |> EdgeT


[<StructuredFormatDisplay("Hypergraph {DisplayForm}")>]
type Hypergraph(edges : EdgeT list) = class
    // extract unique vertices and put into list for unique ordering, outputs and controls
    let vertices = [for e in edges -> e.Vertices] |> Set.unionMany |> Set.toList
    let outputs = vertices |> List.filter (function O _ -> true | _ -> false)
    let controls = vertices |> List.filter (function C _ -> true | _ -> false)

    // check that all output vertices are contained in vertex list
    do
        if (Set.difference (Set.ofList outputs) (Set.ofList vertices)).Count > 0 then failwith "some disconnected output vertices"

    // create a map vertex -> qubit number, e.g. if we have V"1", C"...", V"2" where C controls 10 interactions, we would have
    // V"1" -> 0, C"..." -> 1, 2, 3, 4 and V"2" -> 5
    let qubitLookupTable =
        vertices
        |> List.fold (fun (list : (VertexT * int list) list) v ->
            let qubits = list ||> snd |> List.concat
            let max = List.max (-1 :: qubits)
            (v, [for i in (max+1)..(max+v.HowManyQubits) -> i]) :: list
        ) []
        |> Map.ofList   
    let qubitCount = ((Map.toList qubitLookupTable) ||> snd |> List.concat |> List.max) + 1

    // allows to look up a unique qubit for vertices or an edge
    // the edge qubits are normal-ordered, as edges are sets where a control can have a specific position which we want to be unambiguous
    // the vertex list is unordered, so that the qubits for a; b are 0; 1 but for b; a it's 1; 0
    member this.WhichQubits (vertex : VertexT) = qubitLookupTable.[vertex]
    member this.WhichQubits (vertices : VertexT list) = vertices ||> this.WhichQubits |> List.concat
    member this.WhichQubits (edge : EdgeT) = edge.NormalOrder |> this.WhichQubits

    member this.DisplayForm =
        join "" (
            "\nvertices: " :: (List.zip vertices (vertices ||> this.WhichQubits) ||> sprintf "%A ") @
            "\nedges:    " :: (edges ||> sprintf "%A ")
        )

    // vertices and edges
    member this.Edges = edges
    member this.Vertices = vertices
    member this.Outputs = outputs
    member this.Controls = controls
    member this.Size = qubitCount
    
    // return graph where every hyperedge is extended by a control vertex
    // we are not checking whether the graph is already controlled
    member this.AddControls (f : EdgeT -> ControlT list) =
        new Hypergraph
            ([
                for e in edges do
                    for control in (f e) do
                        yield C control --- e
            ])

    // return an optimized graph
    member this.OptimizeControls (maxControlledInteractions : int) (maxControlledVertices : int) =
        ()
end

