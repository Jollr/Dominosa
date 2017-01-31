open System
open SolutionTests

[<EntryPoint>]
let main argv = 
    (SolutionTests.TestRun ()).Run ()
    (FileReaderTests.TestRun ()).Run ()
    0 