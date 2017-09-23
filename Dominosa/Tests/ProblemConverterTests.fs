module ProblemConverterTests

open Assert
open TestRun
let dummyTest () = 
    {
        Passed = true
        Message = "dummy"
    }
    
let TestRun () = 
    let tests = [
        dummyTest;
    ]
    { Tests = tests; Name = "Problem conversion tests" }