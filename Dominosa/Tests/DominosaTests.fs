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
    Assert.AreEqual 6 (ff.MaxFlowValue ()) "solvablePuzzle: flow is 6" "solvablePuzzle: flow is not equal to 6"

let private unsolvablePuzzle () =
    let ff = MakeFordFulkerson (array2D [
        [0; 1; 0];
        [1; 0; 1];
    ])
    Assert.IsTrue ((ff.MaxFlowValue ()) < 6) "unsolvable puzzle should have max flow < 6"

let private sandbox () =
    do (System.Console.WriteLine "SANDBOX")
    // let grid = MakeGrid (array2D [
    //     [0; 0; 0];
    //     [1; 1; 1];
    // ])
    let grid = MakeGrid (array2D [
        [0;2;2;3;6;5;4;0;];
        [6;0;4;2;5;3;0;1;];
        [0;0;6;6;4;1;5;6;];
        [4;5;2;1;3;0;2;5;];
        [4;1;3;6;1;6;1;2;];
        [6;3;3;2;5;5;0;4;];
        [2;5;4;4;1;1;3;3;]
    ])

    let puzzle = { Numbers = grid; HighestDomino = 6 }
    let intermediate = puzzle.AsMaxFlowProblem ()
    let ff = { Graph = intermediate }

    do (System.Console.WriteLine grid)
    System.IO.File.WriteAllText("d:\\1.txt", intermediate.ToString())

    // let maxFlowGraph = ff.MaxFlowGraph()
    // System.IO.File.WriteAllText("d:\\2.txt", maxFlowGraph.ToString())

    // do (System.Console.WriteLine intermediate)
    // do (System.Console.WriteLine (ff.MaxFlowValue ()))
    // do (System.Console.WriteLine (ff.MaxFlowGraph ()))
    let solution = puzzle.GetSolution ()    
    Assert.Pass (solution.ToString ())
    // Assert.Pass "sandbox"

let TestRun () = 
    let tests = [
        solvablePuzzle
        unsolvablePuzzle
        sandbox
    ]
    { Tests = tests; Name = "DominosaTests" }