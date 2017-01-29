module SolutionTests

open Assert

let private test1 () = 
    Assert.IsTrue true "test1"

let private test2 () = 
    Assert.IsFalse false "test2"

let Tests = 
    [| test1; test2|]