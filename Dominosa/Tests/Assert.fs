module Assert

open System

type TestResult = { Passed: bool; Message: string }
let private make passed message = { Passed = passed; Message = message }
let IsTrue testValue (testMessage: string) = 
    make testValue testMessage

let IsFalse testValue = IsTrue (not testValue) 
let Pass message = make true message
let Fail message = make false message