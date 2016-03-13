namespace LiquidLearn.UnitTests

open NUnit.Framework
open FsUnit

module Utils =
    open LiquidLearn.Utils

    [<Test>]
    let ``array shuffle shuffles``() =
        let someArray = [|1..1000|]
        let shuffled = Array.copy someArray
        // in place
        shuffle shuffled |> should equal shuffled
        // should be different than before
        shuffled |> should not' (equal someArray)
        // sort it, then should be the same again
        shuffled |> Array.sort |> should equal someArray

    [<Test>]
    let ``list normalization and clamping``() =
        let someList = [ for _ in 0..1000 -> rand.NextDouble() ]
        // can normalize list to 1 by maximum function
        someList
            |> normalizeBy List.max
            |> List.max
            |> should equal 1.0
        // clamp list to range working
        someList
            |> clamp 0.3 0.4
            |> fun l -> (List.min l, List.max l)
            |> should equal (0.3, 0.4)


module CSMat =
    open LiquidLearn.Utils
    open LiquidLearn.Interactions
    open Microsoft.Research.Liquid

    let someMatrix size =
        let e = [| for _ in 1..size -> [| for _ in 1..size -> rand.NextDouble() |] |]
        new CSMat(new CMat(e, e))

    [<TestCase(1, 1)>]
    [<TestCase(1, 17)>]
    [<TestCase(8, 4)>]
    let ``direct sum matrix sizes add``(sizeA : int, sizeB : int) =
        (someMatrix sizeA).Plus(someMatrix sizeB).Length
            |> should equal (sizeA + sizeB)

    [<TestCase(1, 1)>]
    [<TestCase(2, 4)>]
    [<TestCase(12, 16)>]
    let ``direct sum matrix entries in right place``(sizeA : int, sizeB : int) =
        let eA = [| for _ in 1..sizeA -> [| for _ in 1..sizeA -> rand.NextDouble() |] |]
        let eB = [| for _ in 1..sizeB -> [| for _ in 1..sizeB -> -rand.NextDouble() |] |]
        let mA = new CSMat(new CMat(eA, eA))
        let mB = new CSMat(new CMat(eB, eB))
        let m = mA.Plus mB

        let cA, cB = Array.concat eA, Array.concat eB

        for i in 0..sizeA+sizeB-1 do
            for j in 0..sizeA+sizeB-1 do
                if i < sizeA then
                    if j < sizeA then
                        cA |> should contain (m.Item(i, j).r)
                    else
                        m.Item(i, j).MCC |> should equal 0.0
                else
                    if j >= sizeA then
                        cB |> should contain (m.Item(i, j).r)
                    else
                        m.Item(i, j).MCC |> should equal 0.0
    
    [<TestCase("1001")>]
    [<TestCase("0011")>]
    [<TestCase("1")>]
    [<TestCase("0")>]
    let ``state definitions are little-endian``(signature : string) =
        let s = (Data.FromString signature)
        CSMat.Sig2State s
            |> should equal (CSMat.Sig2State (Data.FromString ("0" + signature)))
        (CSMat.Sig2State s) * 2
            |> should equal (CSMat.Sig2State (Data.FromString (signature + "0")))
        CSMat.Sig2State s
            |> should be (lessThan (CSMat.Sig2State (Data.FromString ("1" + signature))))

    [<TestCase(10, "1001", true)>]
    [<TestCase(8, "1001", true)>]
    [<TestCase(8, "1110", false)>]
    let ``matrix Hilbert space extension``(size, signature : string, shouldThrow) =
        let state = Data.FromString signature
        // indirectly tests all basis permutation helpers

        let matrix = (someMatrix size)
        if shouldThrow then
            ( fun() -> matrix.SpreadTo state |> ignore ) |> should throw typeof<System.Exception>

        else
            let spread = matrix.SpreadTo state

            let entriesBefore =
                [ for i in 0..matrix.Length-1 -> [ for j in 0..matrix.Length-1 -> matrix.Item(i, j) ]] |> List.concat |> Set.ofList
            let entriesAfter =
                [ for i in 0..spread.Length-1 -> [ for j in 0..spread.Length-1 -> spread.Item(i, j) ]] |> List.concat |> Set.ofList
        
            // no new entries as we're just shuffling them around (modulo duplicates)
            entriesBefore.IsSubsetOf entriesAfter |> should equal true
            // dimension has to fit
            spread.Length |> should equal (pown 2 (signature.Length))

            // all 1s should be same matrix
            let systems = log2 size |> int
            matrix.SpreadTo (Data.FromString (join "" [for _ in 1..systems -> "1"]))
                |> should equal matrix
            
            // 0111 should be a block matrix in the lower right
            let spread = matrix.SpreadTo (Data.FromString ("0" + (join "" [for _ in 1..systems -> "1"])))
            for i in 0..size-1 do
                for j in 0..size-1 do
                    spread.Item(size + i, size + j) |> should equal (matrix.Item(i, j))

            // 1110 should double every entry
            let spread = matrix.SpreadTo (Data.FromString ((join "" [for _ in 1..systems -> "1"]) + "0"))
            for i in 0..size-1 do
                for j in 0..size-1 do
                    spread.Item(2*i, 2*j) |> should equal (spread.Item(2*i+1, 2*j+1))

    
module Train =
    open LiquidLearn.Graph
    open LiquidLearn.Train
    open LiquidLearn.Interactions
    open LiquidLearn.Interactions.Sets
    open LiquidLearn.Utils
    open Microsoft.Research.Liquid

    // mock interface that generates a zero interaction
    type ZeroInteractionMock() =
        inherit IInteractionFactory()

        override this.ShortForm = "zero interactions"
        override this.GateName list = "zero interaction"
        override this.Names n = [join "" [for _ in 1..n -> "0"], n] // return one 0 interaction between all vertices
        override this.Matrix name = fun _ -> new CSMat(new CMat(pown 2 (name.Length), false))


    [<TestFixture>]
    type SimpleControlledTrainerFixture() = class

        [<SetUp>]
        member this.SetUp() =
            // liquid wants to write out circuits
            System.IO.Directory.SetCurrentDirectory(System.IO.Path.GetTempPath())

        [<TearDown>]
        member this.TearDown() = 
            ()

        [<Test>]
        member this.``train and test run without data``() =
            let graph = new Hypergraph([ O"1" --- O"2" ])
            let trainer = new SimpleControlledTrainer(graph, ZeroInteractionMock())
            let dataset = { Data.Yes = []; Data.No = [] }

            trainer.Train(dataset, 10.0, fun _ ket ->
                // expect the simulation to use 3 qubits, as we need to control one interaction
                ket.Qubits.Length |> should equal 3
                // absolutely no bias, as there's no data
                ket.Qubits.Item(0).Prob1 |> should equal 0.5
                ket.Qubits.Item(1).Prob1 |> should equal 0.5
                ket.Qubits.Item(2).Prob1 |> should equal 0.5

                ket
            ).Test(dataset) |> should equal { YesStats = []; NoStats = [] }

        [<TestCase("10", "01")>]
        [<TestCase("110", "100")>]
        [<TestCase("111", "000")>]
        [<TestCase("0101", "0100")>]
        member this.``training projection``(high : string, low : string) =
            // create graph with one big edge containing high.length output vertices
            let graph = new Hypergraph([EdgeT ([ for i in 1..high.Length -> O(string i)] |> Set.ofList)])

            let yes, no = Data.FromString <|| (high, low)
            let trainer = new SimpleControlledTrainer(graph, ZeroInteractionMock())
            let dataset = { Data.Yes = [yes]; Data.No = [no] }

            trainer.Train(dataset, 10.0, fun tag ket ->
                ket.Qubits.Length |> should equal (high.Length + 1)

                // approximately no control bias, as the interaction is zero
                System.Math.Round(ket.Qubits.Item(high.Length).Prob1, 6) |> should equal 0.5

                // liquid sorts qubits in reverse order, in conflict with its log output,
                // so we need to revert qubit order; for some reason, !! conflicts with the testing framework
                // so we trust in MeasurementStatistics, which is simple enough
                MeasurementStatistics ket [0..high.Length-1]
                    |> fun probs ->
                        if tag = "YES" then
                            probs.[CSMat.Sig2State yes] |> should be (greaterThan 0.95)
                        else
                            probs.[CSMat.Sig2State no] |> should be (greaterThan 0.95)


                ket
            ) |> ignore

    end