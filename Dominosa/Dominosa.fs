open System
open SolutionTests

[<EntryPoint>]
let main argv = 
    (SolutionTests.TestRun ()).Run ()
    
    0 