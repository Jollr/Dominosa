module FordFulkersonTests

open Assert
open TestRun
open Grid
open Graph
open FordFulkerson

let private testMaxFlowValue testName vertices expected = 
    let graph = { Vertices = { Values = vertices } }
    let ff = { Graph = graph };
    let failMessage = testName + ": expected max flow value of " + expected.ToString() + ", but was " + ff.MaxFlowValue.ToString()
    Assert.AreEqual expected ff.MaxFlowValue testName (failMessage + "\r\n" + graph.ToString())

let private testMaxFlowGraph testName vertices expected = 
    let graph = { Vertices = { Values = vertices } }
    let expectedGraphResult = { Vertices = { Values = expected } }
    let ff = { Graph = graph };
    let failMessage = testName + ": expected max flow graph of \r\n" + expectedGraphResult.Vertices.ToString() + "\r\nbut was \r\n"
    Assert.AreEqual expectedGraphResult ff.MaxFlowGraph testName failMessage

let emptyGraph () = 
    let grid = { Values = array2D [ ] }
    let emptyGraph = { Vertices = grid }
    let ff = { Graph = emptyGraph };
    Assert.IsTrue (ff.MaxFlowValue = 0) "empty graph should have 0 flow"

let singleEdgeFlowValue () =
    let vertices = array2D [ [0;1]; [1;0] ]
    testMaxFlowValue "singleEdgeFlowValue" vertices 1

let singleEdgeGraph () =
    let vertices = array2D [ [0;1]; [1;0] ]
    testMaxFlowGraph "singleEdgeGraph" vertices (array2D [ [0;1]; [1;0] ])

let TestRun () = 
    let tests = [
       emptyGraph;
       singleEdgeFlowValue;
       singleEdgeGraph;
    ]
    { Tests = tests; Name = "Ford fulkerson implementation tests" }