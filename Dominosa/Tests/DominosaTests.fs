module DominosaTests

open Assert
open TestRun
open Grid
open FordFulkerson
open Dominosa

let private MakeGrid values = { Values = values }
let private MakeFordFulkerson grid = 
    let grid = MakeGrid grid
    let puzzle = { Numbers = grid; HighestDomino = 1 }
    let intermediate = puzzle.AsMaxFlowProblem ()
    { Graph = intermediate }

let private solvablePuzzle () =
    let ff = MakeFordFulkerson (array2D [
        [0; 0; 0];
        [1; 1; 1];
    ])
    Assert.AreEqual 6 ff.MaxFlowValue "solvablePuzzle: flow is 6" "solvablePuzzle: flow is not equal to 6"

let private unsolvablePuzzle () =
    let ff = MakeFordFulkerson (array2D [
        [0; 1; 0];
        [1; 0; 1];
    ])
    Assert.IsTrue (ff.MaxFlowValue < 6) "unsolvable puzzle should have max flow < 6"

let private sandbox () =
    let grid = MakeGrid (array2D [
        [0; 0; 0];
        [1; 1; 1];
    ])
    let puzzle = { Numbers = grid; HighestDomino = 1 }
    let intermediate = puzzle.AsMaxFlowProblem ()
    let ff = { Graph = intermediate }
    let renderer : SolutionRenderer = { Original = puzzle; MaxFlow = ff.MaxFlowGraph }
    let solution = renderer.Render()

    do (System.Console.WriteLine "SANDBOX")
    do (System.Console.WriteLine grid)
    do (System.Console.WriteLine intermediate)
    do (System.Console.WriteLine ff.MaxFlowValue)
    do (System.Console.WriteLine ff.MaxFlowGraph)
    Assert.Pass solution

let TestRun () = 
    let tests = [
        solvablePuzzle
        unsolvablePuzzle
        sandbox
    ]
    { Tests = tests; Name = "DominosaTests" }