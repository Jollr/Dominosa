module Solutions

open System;
open Grid;

type Point = { X: int; Y:int } with 
    override this.ToString () = "(" + (string this.X) + ", " + (string this.Y) + ")"

type Direction = LEFT | RIGHT | UP | DOWN
type Domino = { X: int; Y: int; Direction: Direction } with
    member this.GetPoints: seq<Point> = seq {
        yield { X = this.X; Y = this.Y }
        yield this.GetDirectionPoint ()
    };

    member this.GetCoveringValues (grid: Grid) = 
        this.GetPoints
        |> Seq.map (fun p -> grid.GetValue p.X p.Y)

    member this.Covers x1 y1 x2 y2 : bool = 
        let p1 : Point = { X = this.X; Y = this.Y }
        let p2 = this.GetDirectionPoint ()
        ((x1 = p1.X && y1 = p1.Y && x2 = p2.X && y2 = p2.Y) || (x1 = p2.X && y1 = p2.Y && x2 = p1.X && y2 = p1.Y))

    member private this.GetDirectionPoint () : Point = 
        match this.Direction with
            | LEFT -> { X = this.X - 1; Y = this.Y }
            | RIGHT -> { X = this.X + 1; Y = this.Y }
            | UP -> { X = this.X; Y = this.Y - 1 }
            | DOWN -> { X = this.X; Y = this.Y + 1 }

type Solution = { Grid: Grid; Cover: Domino[]; } with
    member this.IsCorrect: bool = 
        let points = Seq.collect (fun (d:Domino) -> d.GetPoints) this.Cover

        let allPointsAreInGrid () = 
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
    
        allPointsAreInGrid () && hasCorrectNumberOfPoints () && allPointsAreUnique () && allDominoesAreUnique ()

    override this.ToString () : string =
        let topRow = this.RenderDashesLine ()
        let rowNumbers = [0 .. this.Grid.MaxX]
        let renderedRows = Seq.map this.RenderRow rowNumbers
        let rowsResult = this.Concatenate renderedRows
        topRow + rowsResult
    
    member private this.divider = "|"

    member private this.RenderRow n : string =
        let numbers = Seq.map (this.RenderNumber n) [0 .. this.Grid.MaxY]
        let dashes = this.RenderDashes n
        this.divider + (this.Concatenate numbers) + System.Environment.NewLine + dashes + System.Environment.NewLine

    member private this.RenderNumber x y : string =
        let number = this.Grid.GetValue x y
        let hasCoveringDomino = Seq.filter (fun (d : Domino) -> d.Covers x y x (y + 1)) this.Cover |> Seq.isEmpty |> not
        let connectionChar = if (hasCoveringDomino) then " " else this.divider
        (number.ToString ()) + connectionChar

    member private this.RenderDashes n  : string =
        if n = this.Grid.MaxX
        then this.RenderDashesLine ()
        else Seq.map (this.RenderBottomDash n) [0 .. this.Grid.MaxY] |> this.Concatenate

    member private this.RenderBottomDash x y : string =
        let hasCoveringDomino = Seq.filter (fun (d : Domino) -> d.Covers x y (x + 1) y) this.Cover |> Seq.isEmpty |> not
        let connectionChar = if (hasCoveringDomino) then " " else "-"
        if (y = 0)
        then this.divider + connectionChar + this.divider
        else connectionChar + this.divider

    member private this.RenderDashesLine () : string = 
        let length = this.Grid.YLength * 2 + 1
        [0 .. length - 1] |>
        Seq.map (fun _ -> "-") |>
        this.Concatenate |>
        (fun x -> match x.Length with | 0 -> x | _ -> x + System.Environment.NewLine)

    member private this.Concatenate strings : string = Seq.fold (+) "" strings
        
