// LiquidLearn (c) 2016 Johannes Bausch
module LiquidLearn.Graph

open Utils

type vertexT =
    | V of string
    | O of string
    | C of string

let inline (---) a b = 
    unbox<vertexT> /@ (
        match box a, box b with
        | (:? list<vertexT> as a), (:? list<vertexT> as b) -> [a; b] |> List.concat
        | (:? vertexT as a), (:? list<vertexT> as b) -> [[a]; b] |> List.concat
        | (:? list<vertexT> as a), (:? vertexT as b) -> [a; [b]] |> List.concat
        | (:? vertexT as a), (:? vertexT as b) -> [a; b]
        | _ -> failwith "not a valid hyperedge type"
    )

let randomStr = 
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWUXYZ0123456789"
    let charsLen = chars.Length
    let random = System.Random()

    fun len -> 
        let randomChars = [|for i in 0..len -> chars.[random.Next(charsLen)]|]
        new System.String(randomChars)


type Hypergraph(edges : vertexT list list) = class
    // extract unique vertices, output and control
    let vertices = edges |> List.concat |> Set.ofList |> Set.toList
    let output =
        seq {
            for v in vertices do
                match v with
                | O x -> yield O x
                | _ -> ()
        } |> Seq.toList
    let control =
        seq {
            for v in vertices do
                match v with
                | C x -> yield C x
                | _ -> ()
        } |> Seq.toList
    // check that all visible vertices are contained in vertex list
    do
        if (Set.difference (Set.ofList output) (Set.ofList vertices)).Count > 0 then failwith "some disconnected surface vertices"

    member this.whichQubit (name:vertexT) = List.findIndex (fun v -> v = name) vertices

    // prints out the hypergraph structure
    override this.ToString() = sprintf "vertices:\n%A\nedges:\n%A" vertices edges

    // number of vertices
    member this.Size = vertices.Length

    // get a list of interactions of the form [[a0; ... an]; ... [x0; ... xm]]
    member this.Interactions = (fun e-> this.whichQubit /@ e) /@ edges

    // get a list of output or control qubits in the form [q0; q1; ... ; qn]
    member this.Outputs = this.whichQubit /@ output
    member this.Controls = this.whichQubit /@ control

    // return graph where every hyperedge is extended by a control vertex
    member this.ControlGraph =
        new Hypergraph(
            (fun e -> C (randomStr 5) --- e) /@ edges
        )


end

