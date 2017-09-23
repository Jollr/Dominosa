module Grid

type Grid = { Values: int[,] } with
    member this.Size : int = (Array2D.length1 this.Values) * (Array2D.length2 this.Values);
    member this.MaxX : int = (Array2D.length1 this.Values) - 1;
    member this.MaxY : int = (Array2D.length2 this.Values) - 1;
    member this.GetValue x y : int = this.Values.[x, y]
    member this.GetRow y : seq<int> = Seq.map (fun x -> this.GetValue x y) [0 .. this.MaxX]