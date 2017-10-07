module DominosaTests

open Assert
open TestRun
open Grid
open Dominosa

let private sandbox () =
    let grid : Grid = { Values = array2D [
        [0; 0; 0];
        [1; 1; 1];
    ]}
    do (System.Console.WriteLine grid)
    let puzzle = { Numbers = grid; HighestDomino = 1 }
    let intermediate = puzzle.AsMaxFlowProblem ()
    Assert.Pass (intermediate.ToString())

let TestRun () = 
    let tests = [
        sandbox;
    ]
    { Tests = tests; Name = "DominosaTests" }