module SolutionTests

open Assert
open TestRun
open Solutions

let private testCase isCorrect grid cover name =
    let solution = { 
        Grid = { Values = grid }
        Cover = cover
    }
    let testFunc = if isCorrect then Assert.IsTrue else Assert.IsFalse
    testFunc (solution.IsCorrect) name

let private correct = testCase true
let private incorrect = testCase false

let private dir d x y = { X=x; Y=y; Direction=d }
let private down x y = dir DOWN x y
let private right x y =  dir RIGHT x y
let private left x y =  dir LEFT x y
let private up x y =  dir UP x y
let private emptyGrid () = correct (array2D [ ]) Array.empty "empty grid" 
let private emptyGridWithDomino () = 
    incorrect (array2D [ ]) [| down 0 0 |] "empty grid with domino from (0, 0) down"

let private testGrid = array2D [[0; 1]; [0; 1]; [0; 1]]; 
let private testGridWithCover tc name cover = tc testGrid cover name
let private dominoOffGrid d1 d2 dtest () = 
    let name = "domino off grid " + (string dtest.X) + " " + (string dtest.Y)
    testGridWithCover incorrect name [| d1; d2; dtest |] 
let private dominoOffGridUp = dominoOffGrid (right 1 0) (right 1 1) (up 0 0)
let private dominoOffGridRight = dominoOffGrid (right 0 0) (right 0 1) (right 2 0)
let private dominoOffGridDown = dominoOffGrid (right 1 0) (right 1 1) (down 0 1)
let private dominoOffGridLeft = dominoOffGrid (right 1 0) (right 1 1) (left 0 0)
let private singleDominoInGrid () = 
    testGridWithCover incorrect "single domino in grid" [| (down 0 0); |]
let private singleOverlappingDomino () = 
    testGridWithCover incorrect "singleOverlappingDomino" [| (down 0 0); (down 0 0); |]
let private overlappingDominoes () = 
    testGridWithCover incorrect "overlapping dominoes" [| (down 0 0); (down 0 0); (down 0 0); |]
let private simplestActualSolution () = 
    testGridWithCover correct ("simplest actual solution") [| (down 0 0); (right 1 0); (right 1 1) |]

let private wrongSolution () = 
    testGridWithCover incorrect "wrong solution" [| (down 0 0); (down 1 0); (down 2 0); |]

let private largerGrid = array2D [[2;2;0;2;0]; [3;3;0;3;1]; [1;2;4;3;1]; [3;4;2;4;4]; [3;0;2;1;0]; [1;0;4;1;4]]

let private testLargerGrid tc name cover = tc largerGrid cover name
let private largerGridSolution = [| (down 0 0); (right 1 0); (right 1 1); (down 3 0); (down 4 0); (down 5 0); (right 0 2); (right 2 2); (down 4 2); (down 5 2); (down 0 3); (right 1 3); (right 1 4); (down 3 3); (right 4 4) |]
let private solveLargerGrid () = testLargerGrid correct "solve larger grid" largerGridSolution
let private errorInSolvingLargerGrid () = 
    testLargerGrid incorrect "errorInSolvingLargerGrid" [| (down 0 0); (right 1 0); (right 1 1); (up 3 0); (down 4 0); (down 5 0); (right 0 2); (right 2 2); (down 4 2); (down 5 2); (down 0 3); (right 1 3); (right 1 4); (down 3 3); (right 4 4) |]
let private errorInSolvingLargerGrid2 () = 
    testLargerGrid incorrect "errorInSolvingLargerGrid2" [| (down 0 0); (right 1 0); (right 1 1); (down 3 0); (down 4 0); (down 5 0); (right 0 2); (right 2 2); (right 4 2); (right 4 3); (down 0 3); (right 1 3); (right 1 4); (down 3 3); (right 4 4) |]

let private toString () =
    let solution : Solution = { 
        Grid = { Values = largerGrid }
        Cover = largerGridSolution
    }
    Assert.Pass (System.Environment.NewLine + solution.ToString())

let TestRun () = 
    let tests = [
        emptyGrid; 
        emptyGridWithDomino; 
        dominoOffGridUp;
        dominoOffGridRight;
        dominoOffGridDown;
        dominoOffGridLeft;
        singleDominoInGrid;
        singleOverlappingDomino;
        overlappingDominoes;
        simplestActualSolution;
        wrongSolution;
        solveLargerGrid;
        errorInSolvingLargerGrid;
        errorInSolvingLargerGrid2;
        // toString;
    ]
    { Tests = tests; Name = "Solution tests" }