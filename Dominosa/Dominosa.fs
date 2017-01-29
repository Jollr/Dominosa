open System
open SolutionTests

[<EntryPoint>]
let main argv = 
    for test in SolutionTests.Tests do
        test ()
        
    Console.ReadLine() |> ignore
    0 