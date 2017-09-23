module Grid

type Grid = { Values: int[,] } with
    member this.Size : int = (Array2D.length1 this.Values) * (Array2D.length2 this.Values);
    member this.MaxX : int = (Array2D.length1 this.Values) - 1;
    member this.MaxY : int = (Array2D.length2 this.Values) - 1;
    member this.SetValue x y value : Grid = 
        let initFunc a b =
            if (a = x && b = y)
            then value
            else this.GetValue a b
        Grid.Create (this.MaxX + 1) (this.MaxY + 1) initFunc
    member this.GetValue x y : int = this.Values.[x, y]
    member this.GetRow n : seq<int> = Seq.map (fun x -> this.GetValue n x) [0 .. this.MaxX]
    member this.GetColumn n : seq<int> = Seq.map (fun y -> this.GetValue y n) [0 .. this.MaxY]
    static member Empty width length = Grid.Create width length (fun a b -> 0)    
    static member Create width length (initFunc : int -> int -> int) = { Values = Array2D.init width length initFunc } 