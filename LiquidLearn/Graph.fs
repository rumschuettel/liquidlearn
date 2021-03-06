﻿// LiquidLearn (c) 2016 Johannes Bausch
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
    vertices : VertexT list // order matters
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


    // put the control vertex into unique normal order:
    // put controls to however controlled interactions are created,
    // i.e. in this case at the beginning, meaning that the control should be in the upper left block of any matrix
    // note that this does not mean that the control qubits have to be the first vertices of a graph
    member this.NormalOrder =
        this.Vertices
            |> Set.toSeq
            |> Seq.sort
            |> Seq.sortBy (function C _ -> 0 | _ -> 1)
            |> Seq.toList

    member this.Size = this.Vertices.Count

    member this.Subsets = powerset this.NormalOrder |> Seq.groupBy (fun el -> el.Length) |> Seq.toList ||> snd


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

// helper types for graph
exception WouldSplitEdgeException
exception NoOptimalGraphFound

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
            "\n  vertices: " :: (List.zip vertices (vertices ||> this.WhichQubits) ||> sprintf "%A ") @
            "\n  edges:    " :: (edges ||> sprintf "%A ")
        )

    member this.ShortForm = sprintf "%dV%dE-%x" vertices.Length edges.Length (abs(hash edges) % (16*16*16))

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
    member this.OptimizeControls
        (
            maxInteractions : int,
            maxVertices : int,
            ?avoidSplittingEdges : bool,
            ?deleteDuplicateInteractions : bool
        ) =
        let avoidSplittingEdges = defaultArg avoidSplittingEdges true
        let deleteDuplicateInteractions = defaultArg deleteDuplicateInteractions true
        // this is a variant of integer list partitioning, which is known to be NP-hard
        // we use a poly-time greedy approximation, known to lie within 7/6 of the optimal solution
        
        // extract a list of all interactions in the graph, delete potential duplicates and shuffle
        let interactions =
            this.Controls
            ||> fun control -> control.Unwrap.interactions
            |> List.concat
            |> fun interactions ->
                if deleteDuplicateInteractions then
                    interactions |> Set.ofList |> Set.toArray
                else
                    interactions |> List.toArray
            |> shuffle
            |> Array.toList

        // count vertices in interaction sequence
        let countVertices interactions =
            interactions
            ||> fun el -> el.vertices
            |> Seq.concat
            |> Set.ofSeq
            |> Set.count

        // gives a score of adding the interaction to an array
        let score (interaction : InteractionT) (list : InteractionT list) =
            let listContainsEdge =
                if avoidSplittingEdges then
                    (list |> List.tryFind (fun l -> l.vertices = interaction.vertices)).IsSome
                else
                    false // ignore this check and bunch edges together at will
            // too many interactions in partition
            if list.Length >= maxInteractions then
                if listContainsEdge then raise WouldSplitEdgeException
                None
            // if the list already has an interaction between the given vertices, we want to add it to this list
            elif listContainsEdge then Some 1000
            // fill up empty arrays first, only if there's no way of adding an interaction to a list without increasing its score
            // this guarantees that a partitioning *will* be found eventually, when we distribute each interaction to its own edge
            elif list.Length = 0 then Some -1
            // score is difference in unique vertex count
            else
                let before = countVertices list
                let after  = countVertices (interaction :: list)
                // too many vertices?
                if after > maxVertices then None
                else Some (2 * (before - after))

        // try distributing the interactions to a partition of size n
        let rec tryPartition = function
            | n when n < 0 || n > interactions.Length -> raise NoOptimalGraphFound
            | n ->
                let partition : InteractionT list [] = [| for i in 1..n -> [] |]

                let rec tryDistribute = function
                    | [] -> true
                    | interaction::tail ->
                        shuffle partition |> ignore
                        // find all scores for adding interaction to each possible urn
                        // we don't care about tail recursion as we will not have a lot of calls
                        try
                            let scores = score interaction <|| partition

                            // can't add to any part? break.
                            if scores |> Array.forall (fun score -> score = None) then false
                            // otherwise, add to the item with highest score
                            else
                                let maxScore =
                                    scores
                                    |> Array.filter (function None -> false | _ -> true)
                                    |> Array.max

                                let maxScoreIndex = scores |> Array.findIndex (fun s -> s = maxScore)
                                partition.[maxScoreIndex] <- interaction :: partition.[maxScoreIndex]
                                tryDistribute tail
                        with
                        | :? WouldSplitEdgeException -> false

                
                match tryDistribute interactions with
                | true ->
                    // return with good partition
                    partition
                | false ->
                    // try with one more edge
                    tryPartition (n+1)
        
        // try building new graph with optimized interactions
        // since this is a stochastic algorithm, we try a few times and take the most optimal result
        try
            let g =
                [ for _ in 1..50 ->
                    new Hypergraph
                        ([ for part in tryPartition 0 ->
                            let edge =
                                part
                                ||> (fun interaction -> interaction.vertices)
                                |> List.concat
                                |> Set.ofList
                                |> EdgeT

                            C { id=UniqueID(); interactions=part } --- edge
                        ])
                ] |> List.minBy (fun g -> g.Size)
            dumps (sprintf "optimized graph with %d edge%s and %d qubit%s found" g.Edges.Length (pluralS g.Edges.Length) g.Size (pluralS g.Size) )
            g
        with
        | NoOptimalGraphFound ->
            dumps "WARN: no more optimal graph within restricted parameters found."
            this

end

