module FordFulkersonTests

open Assert
open TestRun
open Grid
open Graph
open FordFulkerson

let private makeGraph vertices : Graph = { Vertices = { Values = vertices } }
let private testMaxFlowValue testName vertices expected = 
    let graph = makeGraph vertices
    let ff = { Graph = graph };
    let maxFlowValue = ff.MaxFlowValue ()
    let failMessage = testName + ": expected max flow value of " + expected.ToString() + ", but was " + maxFlowValue.ToString()
    Assert.AreEqual expected maxFlowValue testName (failMessage + "\r\n" + graph.ToString())

let private testMaxFlowGraph testName vertices expected = 
    let graph = makeGraph vertices 
    let expectedGraphResult = makeGraph expected
    let ff = { Graph = graph };
    let maxFlowGraph = ff.MaxFlowGraph ()
    let failMessage = testName + ": expected max flow graph of \r\n" + expectedGraphResult.ToString() + "\r\nbut was \r\n" + maxFlowGraph.ToString()
    Assert.AreEqual expectedGraphResult maxFlowGraph testName failMessage

let emptyGraph () = 
    let grid = { Values = array2D [ ] }
    let emptyGraph = { Vertices = grid }
    let ff = { Graph = emptyGraph };
    Assert.IsTrue ((ff.MaxFlowValue ()) = 0) "empty graph should have 0 flow"
let singleEdgeFlowValue n () =
    let vertices = array2D [ [0;n]; [0;0] ]
    let testName = "singleEdgeFlowValue" + n.ToString()
    testMaxFlowValue testName vertices n
let singleEdgeGraph n () =
    let vertices = array2D [ [0;n]; [0;0] ]
    let testName = "singleEdgeGraph" + n.ToString()
    testMaxFlowGraph testName vertices (array2D [ [0;n]; [0;0] ])
let threeNodeUnconnectedFlowValue () =
    let vertices = array2D [ [0;1;0]; [0;0;0]; [0;0;0] ]
    testMaxFlowValue "threeNodeUnconnectedFlowValue" vertices 0
let threeNodeUnconnectedFlowGraph () =
    let vertices = array2D [ [0;1;0]; [0;0;0]; [0;0;0] ]
    let expected = array2D [ [0;0;0]; [0;0;0]; [0;0;0] ] 
    testMaxFlowGraph "threeNodeUnconnectedFlowGraph" vertices expected
let threeNodeConnectedFlowValue () =
    let vertices = array2D [ [0;2;0]; [0;0;2]; [0;0;0] ]
    testMaxFlowValue "threeNodeConnectedFlowValue" vertices 2
let threeNodeConnectedFlowGraph () =
    let vertices = array2D [ [0;2;0]; [0;0;2]; [0;0;0] ]
    let expected = array2D [ [0;2;0]; [0;0;2]; [0;0;0] ]
    testMaxFlowGraph "threeNodeConnectedFlowGraph" vertices expected
let fourNodeUnconnectedFlowValue verticesList () =
    testMaxFlowValue "fourNodeUnconnectedFlowValue" (array2D verticesList) 0
let fourNodeUnconnectedFlowGraph verticesList () =
    let vertices = array2D verticesList
    let expected = array2D [ [0;0;0;0]; [0;0;0;0]; [0;0;0;0]; [0;0;0;0]; ]
    testMaxFlowGraph "fourNodeUnconnectedFlowGraph" vertices expected
let fourNodeConnectedFlowValue () =
    let vertices = array2D [ [0;2;2;0]; [0;0;1;0]; [0;0;0;5]; [0;0;0;0]; ]
    testMaxFlowValue "fourNodeConnectedFlowValue" vertices 3
let fourNodeConnectedFlowGraph () =
    let vertices = array2D [ [0;2;2;0]; [0;0;1;0]; [0;0;0;5]; [0;0;0;0]; ]
    let expected = array2D [ [0;1;2;0]; [0;0;1;0]; [0;0;0;3]; [0;0;0;0]; ]
    testMaxFlowGraph "fourNodeConnectedFlowGraph" vertices expected
let integrationTest () = 
    let vertices = array2D [ [0;13;0;10;0;0]; [0;0;5;0;0;0]; [0;0;0;50;0;3]; [0;0;0;0;35;0]; [0;0;0;0;0;20]; [0;0;0;0;0;0] ]
    testMaxFlowValue "integrationTest" vertices 15 
let sandbox () =
    do (System.Console.WriteLine "sandbox")
    let vertices = array2D [ [0;13;0;10;0;0]; [0;0;5;0;0;0]; [0;0;0;50;0;3]; [0;0;0;0;35;0]; [0;0;0;0;0;20]; [0;0;0;0;0;0] ]
    let graph : Graph = makeGraph vertices
    let mst = graph.MinimumSpanningTree
    do (System.Console.WriteLine "graph")
    do (System.Console.WriteLine graph)
    let fordFulkerson = { Graph = graph }
    let maxFlowGraph: Graph = fordFulkerson.MaxFlowGraph ()
    do (System.Console.WriteLine "max flow graph")
    Assert.Pass (maxFlowGraph.ToString())

let TestRun () = 
    let tests = [
       emptyGraph;
       singleEdgeFlowValue 1;
       singleEdgeFlowValue 2;
       singleEdgeGraph 1;
       singleEdgeGraph 2;
       threeNodeUnconnectedFlowValue;
       threeNodeUnconnectedFlowGraph;
       threeNodeConnectedFlowValue;
       threeNodeConnectedFlowGraph;
       fourNodeUnconnectedFlowValue [ [0;0;0;0]; [0;0;0;0]; [0;0;0;0]; [0;0;0;0]; ];
       fourNodeUnconnectedFlowGraph [ [0;0;0;0]; [0;0;0;0]; [0;0;0;0]; [0;0;0;0]; ];
       fourNodeUnconnectedFlowValue [ [0;1;1;0]; [0;0;0;0]; [0;0;0;0]; [0;0;0;0]; ];
       fourNodeUnconnectedFlowGraph [ [0;1;1;0]; [0;0;0;0]; [0;0;0;0]; [0;0;0;0]; ];
       fourNodeUnconnectedFlowValue [ [0;1;0;0]; [0;0;0;0]; [0;0;0;1]; [0;0;0;0]; ];
       fourNodeUnconnectedFlowGraph [ [0;1;0;0]; [0;0;0;0]; [0;0;0;1]; [0;0;0;0]; ];
       fourNodeUnconnectedFlowValue [ [0;0;1;0]; [0;0;0;1]; [0;0;0;0]; [0;0;0;0]; ];
       fourNodeUnconnectedFlowGraph [ [0;0;1;0]; [0;0;0;1]; [0;0;0;0]; [0;0;0;0]; ];
       fourNodeConnectedFlowValue;
       fourNodeConnectedFlowGraph;
       integrationTest;
    //    sandbox;
    ]
    { Tests = tests; Name = "Ford fulkerson implementation tests" }