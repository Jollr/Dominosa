module Assert

open System

let IsTrue testValue testMessage = 
    let currentColor = Console.ForegroundColor
    if testValue then 
        Console.ForegroundColor <- ConsoleColor.Green
    else
        Console.ForegroundColor <- ConsoleColor.Red
    
    printf testMessage
    printf "\r\n"
    Console.ForegroundColor <- currentColor

let IsFalse testValue testMessage =
    IsTrue (not testValue) testMessage