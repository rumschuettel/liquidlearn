// LiquidLearn (c) 2016 Johannes Bausch
module LiquidLearn.Graph

open Utils

// recursive vertex type
type VertexT =
    | V of string
    | O of string
    | C of ControlT
    with
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
        | C c -> System.Math.Log(float c.interactions.Length, 2.0) + 1.0 |> ceil |> int
        | _ -> 1

and ControlT = {
    id : string
    interactions : string list // control vertex stores a list of interaction identifiers
}

// edge as a vertex list
type EdgeT = | EdgeT of VertexT list
    with
    member this.Vertices = match this with | EdgeT vertices -> vertices

    // true if edge has a controlled vertex
    member this.IsControlled =
        match List.tryFind (fun (vertex : VertexT) -> vertex.IsControl) this.Vertices with
        | Some _ -> true
        | None -> false

    // get control interaction
    member this.GetControl =
        match List.tryFind (fun (vertex : VertexT) -> vertex.IsControl) this.Vertices with
        | Some (C control) -> C control
        | _ -> failwith "edge has no control vertex"

    // strip control interaction
    member this.StripControl =
        this.Vertices
        |> List.filter (fun vertex -> not vertex.IsControl)
        |> EdgeT


    // put the control vertex to however controlled interactions are created
    member this.NormalOrder =
        EdgeT (Seq.sortBy (fun vertex ->
            match vertex with
            | C _ -> 0
            | _ -> 1
        ) this.Vertices |> Seq.toList)

    member this.Size = this.Vertices.Length


// F# does not support overloading non-tuple operators
let inline (---) a b = 
    (
        match box a, box b with
        | (:? EdgeT as a), (:? EdgeT as b) -> [a.Vertices; b.Vertices] |> List.concat
        | (:? VertexT as a), (:? EdgeT as b) -> [[a]; b.Vertices] |> List.concat
        | (:? EdgeT as a), (:? VertexT as b) -> [a.Vertices; [b]] |> List.concat
        | (:? VertexT as a), (:? VertexT as b) -> [a; b]
        | _ -> failwith "not a valid hyperedge type"
    )
    ||> unbox<VertexT>
    |> EdgeT



type Hypergraph(edges : EdgeT list) = class
    // put edges in correct order, extract unique vertices, outputs and controls
    let edges = [for e in edges -> e.NormalOrder]
    let vertices = [for e in edges -> e.Vertices] |> List.concat |> Set.ofList |> Set.toList
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

    do dump qubitLookupTable


    member this.WhichQubits (vertex : VertexT) = qubitLookupTable.[vertex]
    member this.WhichQubits (vertices : VertexT list) = vertices ||> this.WhichQubits |> List.concat
    member this.WhichQubits (edge : EdgeT) = edge.Vertices |> this.WhichQubits

    // prints out the hypergraph structure
    override this.ToString() = sprintf "vertices:\n%A\nedges:\n%A" vertices edges

    // vertices and edges
    member this.Edges = edges
    member this.Vertices = vertices
    member this.Outputs = outputs
    member this.Controls = controls
    member this.Size = qubitCount
    
    // return graph where either every or only some hyperedges are extended by one or more control vertices
    // we are not yet checking whether the graph is already controlled
    member this.AddControls (f : EdgeT -> ControlT list) =
        new Hypergraph
            ([
                for e in edges do
                    for control in (f e) do
                        yield C control --- e
            ])


end

