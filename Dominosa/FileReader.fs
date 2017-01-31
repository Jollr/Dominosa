module FileReader

open System.IO

let ReadFile (filePath: string) : string list =
    let readLines () =
        // Read in a file with StreamReader.
        use stream = new StreamReader(filePath)
        let mutable lines = []
        // Continue reading while valid lines.
        let mutable valid = true
        while (valid) do
            let line = stream.ReadLine()
            if (line = null) then
                valid <- false
            else
                (lines <- List.append lines [line] );

        lines
    if File.Exists filePath
    then readLines ()
    else 
        List.empty