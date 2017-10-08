module Dominosa

open Grid 
open Graph 

type private NumNodes = { DominoNodes : int; ConnectionNodes : int; NumberNodes : int }

type Puzzle = { Numbers : Grid; HighestDomino : int } with

    member this.AsMaxFlowProblem () : Graph =
        let num : NumNodes = {
            DominoNodes = (this.HighestDomino + 1) * (this.HighestDomino + 2) / 2;
            ConnectionNodes = this.Numbers.XLength * (this.Numbers.YLength - 1) + (this.Numbers.XLength - 1) * this.Numbers.YLength;
            NumberNodes = this.Numbers.Size;
        }
        let gridSize : int = num.DominoNodes + num.ConnectionNodes + num.NumberNodes + 2
        let grid : Grid = Grid.Empty gridSize gridSize
        let resultGrid = grid |> 
            this.FillSinkToDominoEdges num |> 
            this.FillDominoToConnectionEdges num |> 
            this.FillConnectionToNumberEdges num |> 
            this.FillNumberToSinkEdges num 
        { Vertices = resultGrid }
    
    member private this.FillSinkToDominoEdges (num : NumNodes) (grid : Grid) : Grid =
        for index in [1 .. num.DominoNodes] do
            grid.MutateValue 0 index 2
        grid

    member private this.FillDominoToConnectionEdges (num : NumNodes) (grid : Grid) : Grid =
        let firstDominoNodeIndex = 1
        let firstConnectionNodeIndex = 1 + num.DominoNodes

        let mutate x y firstPass =
            let d1 = this.Numbers.GetValue x y
            let d2 = if firstPass then this.Numbers.GetValue (x + 1) y else this.Numbers.GetValue x (y + 1)
            let dominoNodeIndex = (this.GetDominoIndexOffset d1 d2 this.HighestDomino) + firstDominoNodeIndex
            let connectionNodeIndexOffset = this.GetConnectionIndexOffset this.Numbers.MaxX this.Numbers.MaxY x y firstPass
            let connectionNodeIndex = firstConnectionNodeIndex + connectionNodeIndexOffset
            grid.MutateValue dominoNodeIndex connectionNodeIndex 2
        
        for x in [0 .. this.Numbers.MaxX - 1] do
            for y in [0 .. this.Numbers.MaxY] do
                mutate x y true

        for x in [0 .. this.Numbers.MaxX] do
            for y in [0 .. this.Numbers.MaxY - 1] do
                mutate x y false
        grid

    member private this.GetDominoIndexOffset d1 d2 max : int =
        let offset highest lowest = lowest * (max + 1) - lowest * (lowest - 1) / 2 + max - highest
        if (d1 > d2)
        then offset d1 d2
        else offset d2 d1

    member private this.GetConnectionIndexOffset maxX maxY x y firstPass =
        if firstPass
        then x * maxX + y
        else x * (maxX + 1) + y + maxX * (maxY + 1)

    member private this.FillConnectionToNumberEdges (num : NumNodes) (grid : Grid) : Grid =
        let firstConnectionNodeIndex = 1 + num.DominoNodes
        let firstNumberNodeIndex = firstConnectionNodeIndex + num.ConnectionNodes
        let firstPassLength = (this.Numbers.XLength - 1) * this.Numbers.YLength
        let secondPassLength = (this.Numbers.YLength - 1) * this.Numbers.XLength
                
        for index in [ 0 .. firstPassLength - 1] do
            let connectionNodeIndex = index + firstConnectionNodeIndex;
            let numberNodeIndex1 = index + firstNumberNodeIndex
            let numberNodeIndex2 = numberNodeIndex1 + this.Numbers.YLength
            grid.MutateValue connectionNodeIndex numberNodeIndex1 1
            grid.MutateValue connectionNodeIndex numberNodeIndex2 1

        for index in [ 0 .. secondPassLength - 1] do
            let connectionNodeIndex = index + firstPassLength + firstConnectionNodeIndex;
            let numberNodeIndex1 = index + index / this.Numbers.XLength + firstNumberNodeIndex
            let numberNodeIndex2 = numberNodeIndex1 + 1
            grid.MutateValue connectionNodeIndex numberNodeIndex1 1
            grid.MutateValue connectionNodeIndex numberNodeIndex2 1
        grid

    member private this.FillNumberToSinkEdges (num : NumNodes) (grid : Grid) : Grid =
        let upper = grid.MaxX - 1
        let lower = upper - num.NumberNodes + 1
        for x in [lower .. upper] do
            grid.MutateValue x grid.MaxY 1
        grid

// let mutable row : list<string> = [ ]
// row <- (List.append row ["test"])
// row <- (List.append row ["test"])

type SolutionRenderer = { Original : Puzzle; MaxFlow : Graph } with
    member this.Render () : string =
        let rowNumbers = [0 .. this.Original.Numbers.MaxX]
        let renderedRows = Seq.map this.RenderRow rowNumbers
        this.Concatenate renderedRows

    member private this.RenderRow n : string =
        let numbers = Seq.map (this.RenderNumber n) [0 .. this.Original.Numbers.MaxY]
        (this.Concatenate numbers) + System.Environment.NewLine

    member private this.RenderNumber x y : string =
        let number = this.Original.Numbers.GetValue x y
        " " + (number.ToString ()) + " "

    member private this.Concatenate strings : string = Seq.fold (+) "" strings