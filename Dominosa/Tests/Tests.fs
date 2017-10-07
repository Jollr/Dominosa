open System

[<EntryPoint>]
let main argv = 
    (SolutionTests.TestRun ()).Run ()
    (FileReaderTests.TestRun ()).Run ()
    (FordFulkersonTests.TestRun ()).Run ()
    (DominosaTests.TestRun ()).Run ()
    0 