module Program

open System
open System.IO
open System.Text

let someFunction x y = x + y

[<EntryPoint>]
let main args = 
    if args.Length <= 2 then
        let exe = Path.GetFileName args.[0]
        eprintfn "Usage: %s dir pattern" exe
        exit 1
    let directory = args.[1]
    let pattern = args.[2]
    
    for fileName in Directory.GetFiles (directory, pattern) do
        use inputReader = File.OpenText fileName
        
        let lexBuffer = Lexing.LexBuffer<_>.FromTextReader inputReader
        
        let outputFile = Path.ChangeExtension (fileName, "html")
        use outputWriter = new StreamWriter (outputFile) :> TextWriter
        
        fprintfn outputWriter "<html>\n<head></head>\n<pre>"
        
        Text2HtmlLex.convertHtml outputWriter lexBuffer
        
        fprintfn outputWriter "</pre>\n</html>\n"
    0