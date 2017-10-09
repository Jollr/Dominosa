module Grid

type Grid = { Values: int[,] } with
    member this.Size : int = (Array2D.length1 this.Values) * (Array2D.length2 this.Values);
    member this.MaxX : int = this.XLength - 1;
    member this.MaxY : int = this.YLength - 1;
    member this.XLength : int = (Array2D.length1 this.Values);
    member this.YLength : int = (Array2D.length2 this.Values); 
    member this.Copy () : Grid = Grid.Create this.XLength this.YLength this.GetValue
    member this.SetValue x y value : Grid = 
        let initFunc a b =
            if (a = x && b = y)
            then value
            else this.GetValue a b
        Grid.Create (this.MaxX + 1) (this.MaxY + 1) initFunc
    member this.MutateValue x y value = 
        Array2D.set this.Values x y value
    member this.GetValue x y : int = this.Values.[x, y]
    member this.GetRow n : seq<int> = Seq.map (fun x -> this.GetValue n x) [0 .. this.MaxX]
    member this.GetColumn n : seq<int> = Seq.map (fun y -> this.GetValue y n) [0 .. this.MaxY]
    static member Empty width length = Grid.Create width length (fun a b -> 0)    
    static member Create width length (initFunc : int -> int -> int) = { Values = Array2D.init width length initFunc } 

    override this.ToString () = 
        let xIndices = [ 0 .. this.MaxX ]
        let yIndices = [ 0 .. this.MaxY ]
        let renderDigit (n : int) : string = n.ToString ()
            // let asString = n.ToString ()
        
            // if (asString.Length = 1)
            // then asString + "  "
            // elif (asString.Length = 2)
            // then asString + " "
            // else asString

        let renderRow (x : int) : string =
            let getValue i : int  = this.GetValue x i
            let values : seq<string> = yIndices |> Seq.map getValue |> Seq.map renderDigit
            String.concat " " values
        
        let render () = 
            let rows = Seq.map renderRow xIndices
            String.concat "\r\n" rows

        render()