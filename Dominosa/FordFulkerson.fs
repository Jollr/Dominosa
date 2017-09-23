module FordFulkerson

open Graph;

type FordFulkerson = { Graph: Graph } with
    member this.MaxFlowValue : int = 
        let lastRowIndex : int = this.Graph.NumVertices - 1;
        let lastRow : seq<int> = this.Graph.Vertices.GetRow lastRowIndex;
        if lastRowIndex < 0
        then 0
        else Seq.reduce (+) lastRow
    
    member this.MaxFlowGraph: Graph = this.Graph;