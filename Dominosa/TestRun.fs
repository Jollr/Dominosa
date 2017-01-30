module TestRun

open System
open Assert

let private printInColor (text: string) (color: ConsoleColor) =
    let currentColor = Console.ForegroundColor
    Console.ForegroundColor <- color
    
    Console.WriteLine text
    Console.ForegroundColor <- currentColor

let private printTestResult (result: TestResult) =
    let color = if result.Passed then ConsoleColor.Green else ConsoleColor.Red
    printInColor result.Message color

type TestRun = { Tests: (unit -> TestResult) list; Name: string } with
    member this.Run () =
        printInColor this.Name ConsoleColor.DarkCyan
        for test in this.Tests do
            printTestResult (test ())
        Console.WriteLine ()

    