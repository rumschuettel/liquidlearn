LiquidLearn
===========
> LiquidLearn is a simulator for quantum neural networks, based on Microsoft Liquid, and part of my submission to the Microsoft Quantum Challenge in April 2016. It won the [second price](https://blogs.msdn.microsoft.com/msr_er/2016/05/16/microsoft-quantum-challenge-results-are-in/), together with some other amazing submissions!

I plan to update the codebase around July to make it more user-friendly, so feel free to send me suggestions or comments.

Setup
-----
To compile LiquidLearn on Windows, make sure the assembly references to the right path (in particular Liquid1.dll, depending on where it is installed on your system). LiquidLearn itself does not require further dependencies, the unit tests in UnitTest require NUnit (install using NuGet).

Usage
-----
The syntax is that of any compiled Liquid executable.
```sh
$ ./LiquidLearn "HelloWorld()"
```
Trains a simple neural network with two input/output nodes, random interactions and tests it on three datasets. The output should show a statistic similar to
```
 [TestResults 
  y	-3.95e-002	0.00e+000
  y	2.19e-008	0.00e+000
  n	9.92e-001	5.21e-001
  n	4.06e-003	0.00e+000;
 TestResults 
  y	1.29e-001	0.00e+000
  y	-9.10e-001	0.00e+000
  n	6.08e-008	2.89e-001
  n	8.57e-001	9.96e-001;
 TestResults 
  y	-1.06e-001	0.00e+000
  y	-6.98e-001	1.00e+000
  n	-5.42e-008	7.12e-001
  n	7.03e-001	7.12e-001]
```
Each TestResult has a list of yes and now instances (y and n) with measured respective energies and standard deviations thereof.
```sh
$ ./LiquidLearn "MSRSubmissionData(benchmark)"
```
Runs all classifications that go into the interaction benchmark in Table 1 in the submission, and saves the output data and statistics to a series of .test and .stats files. The .stat files serve as lock files in case the command is spread over a few distributed systems, so delete them to re-generate the particular dataset.

```sh
$ ./LiquidLearn "MSRSubmissionData(color)"
```
Runs the color classification task from Figure 4 in the submission. The output is a list of instances (all in the yes branch; we're not testing colors where we know which color they are, but just want the energies for further analysis.)

All gate commands used for the simulation have custom draw commands attached, and the simulated circuits are output during the computation.


Project Overview
----------------
### LiquidLearn
- *Program.fs*
    - `AllDataSets`: Generates all possible datasets for which a unique training outcome is expected, considering symmetries and invariant permutations of the datasets
    - `SomeDataSets`: Generates a few datasets that can be used to train. Might contain duplicates, but unlikely for bigger neural nets
    - `Run`: Training and test run on dataset and graph with certain interactions
    - `MSRSubmission`: See above.
    - `HelloWorld`: See above.
- *Utils.fs*: Various shortcuts, pipe operators, combinatorics, some log functions
    - `Data`: Contains `DataSet` type and functions to generate them from strings
- *Graph.fs*
    - `VertexT`: graph vertex type: `O` is an output vertex, `V` a standard vertex and `C` a control vertex type which is used for training.
    - `EdgeT`: graph hyperedge type. A hyperedge can be created using syntax like `V"1" --- V"2" --- O"out"`.
    - `Hypergraph`: hypergraph class. `AddControls` returns a new hypergraph with controls added by a closure.
    - `OptimizeControls`: tries to reduce the number of qubits necessary to train the graph, within the limits given. Returns either the optimized graph if one is found, or the old one.
- *Interactions.fs*
    - type extension of `Liquid.CSMat`: Direct sum function, basis permutation helpers for creating projectors or a basis permutation matrix. `PermuteBasis` expects a permutation function, which can be generated using `Utils.getPermutation [1; 2; 3] [1; 3; 2]`. `SpreadTo` allows a matrix to be extended to more qubits, e.g. a 2-qubit unitary to a system ABCD, acting non-trivially only on AD.
    - `Projector`: creates the unitary gate of a projector.
    - `MatrixInteraction`: creates a gate from a `float -> CSMat` unitary
    - `ControlledInteraction`: similar to Liquid's `CgateNC`, but allows several qubits to control multiple different interactions.
    - `Sets`: contains an interface for creating new interaction sets, as well as a series of built-in interactions (Paulis, Ising, Heisenberg, Random etc.).
- *Train.fs*
    - `MeasurementStatistics`: shortcut to get measurement probabilities for a list of qubits
    - `InterpretControlMeasurement`: translates probabilities from measuring control qubits into interaction couplings.
    - `TestResult`: test result table type
    - `SimpleControlledTrainer`: class to train and test quantum neural networks. The constructor expects a graph and an interaction generator, and creates an optimized training graph to work with. `Train` and `Test` then expect the respective datasets. The exact idea behind the training algorithm is outlined in the submission.
- **Gates.fs*: Interaction set matrices auto-generated with mathematica.


### UnitTests
- *Tests.fs*: series of tests for LiquidLearn. Run with NUnit3 test runner.