module SolutionTests

open Assert
open Solutions

let private testCase (name: string) (s: Solution) isCorrect =
    let testFunc = if isCorrect then Assert.IsTrue else Assert.IsFalse
    testFunc (s.IsCorrect) (Printf.TextWriterFormat<unit> name)

let private correct name solution = testCase name solution true
let private incorrect name solution = testCase name solution false
let private dir d x y = { X=x; Y=y; Direction=d }
let private down x y = dir DOWN x y
let private right x y =  dir RIGHT x y
let private left x y =  dir LEFT x y
let private up x y =  dir UP x y
let private emptyGrid () = 
    let solution = { Grid = { Values = array2D [ ] }; Cover = Array.empty } 
    correct "empty grid" solution 
let private emptyGridWithDomino () = 
    let solution = { 
        Grid = { Values = array2D [ ] }
        Cover = [| down 0 0 |]
    }
    incorrect "empty grid with domino from (0, 0) down" solution

let private testGrid = { Values = array2D [[0; 1]; [0; 1]; [0; 1]]; }
let private testGridWithCover (test : string -> Solution -> unit) name cover  =
    let solution = { 
        Grid = testGrid
        Cover = cover
    }
    test name solution
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

let private largerGrid = { Values = array2D [[2;2;0;2;0]; [3;3;0;3;1]; [1;2;4;3;1]; [3;4;2;4;4]; [3;0;2;1;0]; [1;0;4;1;4]]};

let private testLargerGrid (test : string -> Solution -> unit) name cover  =
    let solution = { 
        Grid = largerGrid
        Cover = cover
    }
    test name solution

let private largerGridSolution = [| (down 0 0); (right 1 0); (right 1 1); (down 3 0); (down 4 0); (down 5 0); (right 0 2); (right 2 2); (down 4 2); (down 5 2); (down 0 3); (right 1 3); (right 1 4); (down 3 3); (right 4 4) |]
let private solveLargerGrid () = testLargerGrid correct "solve larger grid" largerGridSolution
let private errorInSolvingLargerGrid () = 
    testLargerGrid incorrect "errorInSolvingLargerGrid" [| (down 0 0); (right 1 0); (right 1 1); (up 3 0); (down 4 0); (down 5 0); (right 0 2); (right 2 2); (down 4 2); (down 5 2); (down 0 3); (right 1 3); (right 1 4); (down 3 3); (right 4 4) |]
let private errorInSolvingLargerGrid2 () = 
    testLargerGrid incorrect "errorInSolvingLargerGrid2" [| (down 0 0); (right 1 0); (right 1 1); (down 3 0); (down 4 0); (down 5 0); (right 0 2); (right 2 2); (right 4 2); (right 4 3); (down 0 3); (right 1 3); (right 1 4); (down 3 3); (right 4 4) |]

let Tests = [
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
]