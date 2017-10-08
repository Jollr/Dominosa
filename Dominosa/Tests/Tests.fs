open System

[<EntryPoint>]
let main argv = 
    (FileReaderTests.TestRun ()).Run ()
    (FordFulkersonTests.TestRun ()).Run ()
    (DominosaTests.TestRun ()).Run ()
    (SolutionTests.TestRun ()).Run ()
    0 