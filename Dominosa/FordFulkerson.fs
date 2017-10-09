module FordFulkerson

open System;
open Grid;
open Graph;

type FordFulkerson = { Graph : Graph } with
    member private this.LastColumnTotal (graph: Graph) : int = 
        let lastColumnIndex : int = graph.NumVertices - 1;
        let lastColumn : seq<int> = graph.Vertices.GetColumn lastColumnIndex
        if lastColumnIndex < 0
        then 0
        else Seq.reduce (+) lastColumn

    member this.MaxFlowValue () : int = this.LastColumnTotal (this.MaxFlowGraph ())
    member this.MaxFlowGraph () : Graph = 
        let emptyGraph = { Vertices = Grid.Empty this.Graph.NumVertices this.Graph.NumVertices }
        this.MaxFlowGraphRecursive this.Graph emptyGraph

    member private this.MaxFlowGraphRecursive (capacities: Graph) (flow: Graph) : Graph =
        let mst : Graph = capacities.MinimumSpanningTree
        if ((this.LastColumnTotal mst) = 0)
        then this.MaxFlowGraphRecursiveResult flow
        else this.MaxFlowGraphRecursiveMst capacities flow mst

    member private this.MaxFlowGraphRecursiveResult (flow: Graph) : Graph =
        let getResultValue x y = 
            let capacity = flow.GetCapacity x y
            if (capacity > 0)
            then capacity
            else 0
        
        let resultGrid : Grid = Grid.Create flow.NumVertices flow.NumVertices getResultValue
        { Vertices = resultGrid }

    member private this.MaxFlowGraphRecursiveMst (capacities: Graph) (flow: Graph) (mst: Graph) : Graph = 
        let winningPath = this.WinningPath mst
        let adjustedCapacities = this.AdjustCapacities capacities winningPath
        let adjustedFlow = this.AdjustFlow flow winningPath
        this.MaxFlowGraphRecursive adjustedCapacities adjustedFlow

    member private this.AdjustCapacities (capacities: Graph) (winningPath: Graph) : Graph =
        let getUpdatedValue x y = 
            let capacity = winningPath.GetCapacity x y
            let residueCapacity = winningPath.GetCapacity y x
            if (capacity > 0)
            then (capacities.GetCapacity x y) - capacity
            else (capacities.GetCapacity x y) + residueCapacity
        { Vertices = Grid.Create capacities.NumVertices capacities.NumVertices getUpdatedValue  }
    
    member private this.AdjustFlow (flow: Graph) (winningPath: Graph) : Graph =
        let getUpdatedValue x y = 
            let previousValue = flow.GetCapacity x y
            let capacity = winningPath.GetCapacity x y
            let residueCapacity = winningPath.GetCapacity y x
            if (capacity > 0)
            then previousValue + capacity
            else previousValue - residueCapacity
        let newGrid = Grid.Create flow.NumVertices flow.NumVertices getUpdatedValue 
        let result = { Vertices = newGrid }
        result

    member private this.WinningPath (mst: Graph) : Graph = 
        this.WinningPathRecursive mst { Vertices = Grid.Empty mst.NumVertices mst.NumVertices } mst.Vertices.MaxX

    member private this.WinningPathRecursive (mst: Graph) (path: Graph) (index: int) : Graph = 
        if (index = 0)
        then this.WinningPathResult path
        else this.WinningPathIteration mst path index
    
    member private this.WinningPathResult (path: Graph) : Graph =
        let getForRow x : int =
            let numbers = Seq.map (path.GetCapacity x) [ 0 .. path.Vertices.MaxY ]
            this.GetLowestNonZeroIfAny numbers

        let lowestCapacity = 
            let numbers = Seq.map getForRow [ 0 .. path.Vertices.MaxX ]
            this.GetLowestNonZeroIfAny numbers
        
        let getNewCapacity x y =
            let currentCapacity = path.GetCapacity x y
            if (currentCapacity = 0)
            then 0
            else lowestCapacity
        
        { Vertices = Grid.Create path.NumVertices path.NumVertices getNewCapacity }

    member private this.WinningPathIteration (mst: Graph) (path: Graph) (index: int) : Graph = 
        let nextIndex : int = [0 .. mst.Vertices.MaxX] |> Seq.filter (fun x -> (mst.GetCapacity x index) > 0) |> Seq.head
        let newPath : Graph = path.AddEdge nextIndex index (mst.GetCapacity nextIndex index)
        this.WinningPathRecursive mst newPath nextIndex

    member private this.GetLowestNonZeroIfAny (numbers: seq<int>) : int = 
        let isNotZero x = x > 0
        let nonZeros = Seq.filter isNotZero numbers
        if (Seq.isEmpty nonZeros)
        then 0
        else Seq.head (Seq.sort nonZeros)