open System

[<EntryPoint>]
let main argv = 
    (SolutionTests.TestRun ()).Run ()
    (FileReaderTests.TestRun ()).Run ()
    (ProblemConverterTests.TestRun ()).Run ()
    (FordFulkersonTests.TestRun ()).Run ()
    0 