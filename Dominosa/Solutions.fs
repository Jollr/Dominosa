module Solutions

open System;

type Point = { X: int; Y:int } with 
    override this.ToString () = "(" + (string this.X) + ", " + (string this.Y) + ")"

type Grid = { Values: int[,] } with
    member this.Size : int = (Array2D.length1 this.Values) * (Array2D.length2 this.Values);
    member this.MaxX : int = (Array2D.length1 this.Values) - 1;
    member this.MaxY : int = (Array2D.length2 this.Values) - 1;
    member this.GetValue x y : int = this.Values.[x, y]

type Direction = LEFT | RIGHT | UP | DOWN
type Domino = { X: int; Y: int; Direction: Direction } with
    member this.GetPoints: seq<Point> = seq {
        yield { X = this.X; Y = this.Y }
        yield 
            match this.Direction with
            | LEFT -> { X = this.X - 1; Y = this.Y }
            | RIGHT -> { X = this.X + 1; Y = this.Y }
            | UP -> { X = this.X; Y = this.Y - 1 }
            | DOWN -> { X = this.X; Y = this.Y + 1 }
    };
    member this.GetCoveringValues (grid: Grid) = 
        this.GetPoints
        |> Seq.map (fun p -> grid.GetValue p.X p.Y)

type Solution = { Grid: Grid; Cover: Domino[]; } with
    member this.IsCorrect: bool = 
        let points = Seq.collect (fun (d:Domino) -> d.GetPoints) this.Cover

        // for p in points do
        //     Console.WriteLine (p.ToString ())
        // Console.WriteLine this.Grid.MaxX
        // Console.WriteLine this.Grid.MaxY 

        let allPointsAreInGrid  ()= 
            let isInvalid (p: Point) = p.X < 0 || p.X > this.Grid.MaxX || p.Y < 0 || p.Y > this.Grid.MaxY 
            points |> Seq.filter isInvalid |> Seq.length |> fun l -> l = 0

        let hasCorrectNumberOfPoints  (): bool =      
            (Seq.length points) = this.Grid.Size

        let allUnique seq = seq |> Seq.distinct |> Seq.length |> fun l -> l = (Seq.length seq)
        let allPointsAreUnique () : bool = allUnique points
        let allDominoesAreUnique () : bool = 
            let getCoveringValues = (fun (d: Domino) -> d.GetCoveringValues this.Grid)
            let getHashString = (fun x -> "(" + (string (Seq.head x)) + ", " + (string (Seq.last x)) + ")" )

            this.Cover
            |> Seq.map (getCoveringValues >> getHashString)
            |> allUnique

        // // let temp = 
        // //     this.Cover 
        // //     |> Seq.map (fun d -> d.GetCoveringValues this.Grid)
        // //     |> Seq.map (fun r -> "(" + (string (Seq.head r)) + ", " + (string (Seq.last r)) + ")")

        // for p in temp do
        //     Console.WriteLine (p.ToString ())
    
        allPointsAreInGrid () && hasCorrectNumberOfPoints () && allPointsAreUnique () && allDominoesAreUnique ()