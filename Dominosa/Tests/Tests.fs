open System

[<EntryPoint>]
let main argv = 
    (FileReaderTests.TestRun ()).Run ()
    (SolutionTests.TestRun ()).Run ()
    (FordFulkersonTests.TestRun ()).Run ()
    (DominosaTests.TestRun ()).Run ()
    0 