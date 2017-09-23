module FileReaderTests

open Assert
open TestRun
open FileReader
let private openNonExistingFile () = 
    if Seq.isEmpty (FileReader.ReadFile "no.txt")
    then Assert.Pass "cannot find non-existing file"
    else Assert.Fail "should not find non-existing file"

let private readFileContents () = 
    let contents: string list = FileReader.ReadFile ".\\build\\testfile.txt";

    let result lines =
        let message = lines |> String.concat ", "
        let isCorrect = (message = "testfile 1, testfile 2, testfile 3")

        if isCorrect
        then Assert.Pass message
        else Assert.Fail message

    result contents

let TestRun () = 
    let tests = [
        openNonExistingFile;
        readFileContents;
    ]
    { Tests = tests; Name = "FileReaderTests" }