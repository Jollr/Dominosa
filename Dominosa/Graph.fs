module Graph

open Grid;

type Graph = { Vertices: Grid } with
    member this.NumVertices : int = this.Vertices.MaxX + 1;
    member this.GetCapacity x y = this.Vertices.GetValue x y
    member this.MinimumSpanningTree : Graph = this.MinimumSpanningTreeRecursive (Grid.Empty this.NumVertices this.NumVertices) [0]
    member private this.MinimumSpanningTreeRecursive (current : Grid) (visited : list<int>) : Graph = 
        if (visited.Length = this.NumVertices)
        then { Vertices = current }
        else 
            let notVisited : seq<int> = Seq.filter (fun n -> not (List.contains n visited)) [0 .. this.Vertices.MaxX]
            let isCandidate v nv : bool = this.Vertices.GetValue v nv > 0
            let getCandidates v : seq<int*int> = notVisited |> Seq.filter (isCandidate v) |> Seq.map (fun nv -> (v,nv))
            let candidates : seq<int*int> = Seq.collect getCandidates visited
            if (Seq.isEmpty candidates)
            then { Vertices = current }
            else 
                let (winningCandidateX, winningCandidateY) : int*int = Seq.head candidates
                let winningValue = this.Vertices.GetValue winningCandidateX winningCandidateY 
                current.MutateValue winningCandidateX winningCandidateY winningValue
                let newVisited = List.append visited [winningCandidateY]
                this.MinimumSpanningTreeRecursive current newVisited

    member this.AddEdge x y capacity : Graph = 
        { Vertices = this.Vertices.SetValue x y capacity }

    override this.ToString () = this.Vertices.ToString ()