module Assert

open System

type TestResult = { Passed: bool; Message: string }

let IsTrue testValue (testMessage: string) = 
    { Passed = testValue; Message = testMessage }

let IsFalse testValue =
    IsTrue (not testValue) 