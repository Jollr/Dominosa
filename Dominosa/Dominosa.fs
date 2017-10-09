module Dominosa

open Grid 
open Graph 
open Solutions 
open FordFulkerson

type private Indices = { NumDominoNodes : int; NumConnectionNodes : int; NumNumberNodes : int } with
    member this.FirstDominoNodeIndex = 1
    member this.FirstConnectionNodeIndex = this.FirstDominoNodeIndex + this.NumDominoNodes
    member this.FirstNumberNodeIndex = this.FirstConnectionNodeIndex + this.NumConnectionNodes

    member this.GetDominoIndex d1 d2 max : int =
        let getIndex highest lowest = this.FirstDominoNodeIndex + lowest * (max + 1) - lowest * (lowest - 1) / 2 + max - highest

        if (d1 > d2)
        then getIndex d1 d2
        else getIndex d2 d1

    member this.GetConnectionNodeIndex maxX maxY x y firstPass =
        if firstPass
        then this.FirstConnectionNodeIndex + x * maxX + y
        else this.FirstConnectionNodeIndex + x * (maxX + 1) + y + maxX * (maxY + 1)

    member this.GetNumberNodeIndices xLength yLength offset firstPass : int*int =
        let index1 = match firstPass with
            | true -> this.FirstNumberNodeIndex + offset
            | false -> this.FirstNumberNodeIndex + offset + offset / xLength

        let index2 = match firstPass with
            | true -> index1 + yLength
            | false -> index1 + 1
        
        (index1, index2)

type Puzzle = { Numbers : Grid; HighestDomino : int } with
    member private this.GetIndices () = 
        let indices : Indices = {
            NumDominoNodes = (this.HighestDomino + 1) * (this.HighestDomino + 2) / 2;
            NumConnectionNodes = this.Numbers.XLength * (this.Numbers.YLength - 1) + (this.Numbers.XLength - 1) * this.Numbers.YLength;
            NumNumberNodes = this.Numbers.Size;
        }
        indices

    member this.GetSolution () : Solution =
        let indices = this.GetIndices ()
        let mfp = this.AsMaxFlowProblem ()        
        let maxFlowGraph = { Graph = mfp }.MaxFlowGraph ()        

        let getPointFromNumberNodeIndex (index : int) : Point =
            let baseIndex = index - indices.FirstNumberNodeIndex
            let x = baseIndex / this.Numbers.YLength
            let y = baseIndex % this.Numbers.YLength
            let result : Point = { X = x; Y = y}
            result

        let getDominoFromPoints (p1 : Point) (p2 : Point) : Domino option = 
            let create x y dir = Some { X = x; Y = y; Direction = dir}
            // do (System.Console.WriteLine p1)
            // do (System.Console.WriteLine p2)
            if (p1.X = p2.X && p1.Y = p2.Y + 1) 
            then create p1.X p1.Y DOWN
            elif (p1.X = p2.X && p1.Y = p2.Y - 1) 
            then create p1.X p1.Y UP
            elif (p1.X = p2.X + 1 && p1.Y = p2.Y) 
            then create p1.X p1.Y LEFT
            elif (p1.X = p2.X - 1 && p1.Y = p2.Y) 
            then create p1.X p1.Y RIGHT    
            else option.None

        let getDomino connectionNodeIndex : Domino option =
            do (System.Console.WriteLine ("getDomino: " + connectionNodeIndex.ToString()))

            let hasCapacity (y : int) : bool = (maxFlowGraph.GetCapacity connectionNodeIndex y) > 0
            let numberNodesWithCapacity : seq<int> = Seq.filter hasCapacity [indices.FirstNumberNodeIndex .. indices.FirstNumberNodeIndex + indices.NumNumberNodes - 1]
            let points = Seq.map getPointFromNumberNodeIndex numberNodesWithCapacity
            match Seq.length points with
            | 2 -> getDominoFromPoints (Seq.head points) (Seq.nth 1 points) 
            | _ -> option.None

        let connectionNodeIndices = [ indices.FirstConnectionNodeIndex .. indices.FirstNumberNodeIndex - 1 ]
        let dominoes = 
            connectionNodeIndices
            |> Seq.map getDomino
            |> Seq.filter (fun o -> o.IsSome)
            |> Seq.map (fun o -> o.Value)
            |> Seq.toArray
            
        let solution : Solution = { 
            Grid = { Values = this.Numbers.Values }
            Cover = dominoes
        }
        solution

    member this.AsMaxFlowProblem () : Graph =
        let indices = this.GetIndices ()
        let gridSize : int = indices.NumDominoNodes + indices.NumConnectionNodes + indices.NumNumberNodes + 2
        let grid : Grid = Grid.Empty gridSize gridSize
        let resultGrid = grid |> 
            this.FillSourceToDominoEdges indices |> 
            this.FillDominoToConnectionEdges indices |> 
            this.FillConnectionToNumberEdges indices |> 
            this.FillNumberToSinkEdges indices 
        { Vertices = resultGrid }
    
    member private this.FillSourceToDominoEdges (indices : Indices) (grid : Grid) : Grid =
        for index in [1 .. indices.NumDominoNodes] do
            grid.MutateValue 0 index 2
        grid

    member private this.FillDominoToConnectionEdges (indices : Indices) (grid : Grid) : Grid =
        let mutate x y firstPass =
            let d1 = this.Numbers.GetValue x y
            let d2 = if firstPass then this.Numbers.GetValue (x + 1) y else this.Numbers.GetValue x (y + 1)
            let dominoNodeIndex = indices.GetDominoIndex d1 d2 this.HighestDomino
            let connectionNodeIndex = indices.GetConnectionNodeIndex this.Numbers.MaxX this.Numbers.MaxY x y firstPass
            grid.MutateValue dominoNodeIndex connectionNodeIndex 2
        
        for x in [0 .. this.Numbers.MaxX - 1] do
            for y in [0 .. this.Numbers.MaxY] do
                mutate x y true

        for x in [0 .. this.Numbers.MaxX] do
            for y in [0 .. this.Numbers.MaxY - 1] do
                mutate x y false
        grid

    member private this.FillConnectionToNumberEdges (indices : Indices) (grid : Grid) : Grid =
        let firstPassLength = (this.Numbers.XLength - 1) * this.Numbers.YLength
        let secondPassLength = (this.Numbers.YLength - 1) * this.Numbers.XLength
        let add2Edges connectionNodeIndex (numberNodeIndex1, numberNodeIndex2) =
            grid.MutateValue connectionNodeIndex numberNodeIndex1 1
            grid.MutateValue connectionNodeIndex numberNodeIndex2 1

        for index in [ 0 .. firstPassLength - 1] do
            let connectionNodeIndex = index + indices.FirstConnectionNodeIndex;
            let numberIndices = indices.GetNumberNodeIndices this.Numbers.XLength this.Numbers.YLength index true
            add2Edges connectionNodeIndex numberIndices

        for index in [ 0 .. secondPassLength - 1] do
            let connectionNodeIndex = index + firstPassLength + indices.FirstConnectionNodeIndex;
            let numberIndices = indices.GetNumberNodeIndices this.Numbers.XLength this.Numbers.YLength index false            
            add2Edges connectionNodeIndex numberIndices
        grid

    member private this.FillNumberToSinkEdges (indices : Indices) (grid : Grid) : Grid =
        let upper = grid.MaxX - 1
        let lower = upper - indices.NumNumberNodes + 1
        for x in [lower .. upper] do
            grid.MutateValue x grid.MaxY 1
        grid