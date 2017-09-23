module Graph

open Grid;

type Graph = { Vertices: Grid } with
    member this.NumVertices : int = this.Vertices.MaxX + 1;
    override this.ToString () = 
        let indices = [0 .. (this.NumVertices - 1)];
        let mutable result = "";

        let renderRow (n : int) : string =
            let getValue i : int  = this.Vertices.GetValue n i
            let values : seq<string> = indices |> Seq.map getValue |> Seq.map (fun x -> x.ToString())
            String.concat " " values
        
        let render () = 
            let rows = Seq.map renderRow indices
            String.concat "\r\n" rows

        render()