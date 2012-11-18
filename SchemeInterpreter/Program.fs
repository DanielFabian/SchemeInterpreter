module Program

open System
open System.IO
open System.Text

open Evaluator


//let parseText text = 
//    let lexbuf = Lexing.LexBuffer<_>.FromString text
//    try
//        Choice1Of2 <| head <| start token lexbuf
//    with e ->
//        let pos = lexbuf.EndPos
//        Choice2Of2 <| ParserError <| sprintf "Error near line %d, character %d\n" pos.Line pos.Column    

[<EntryPoint>]
let main args = 
//    let evaluated = choice {
//        let! ast = parseText args.[0]
//        let! evaluated = eval <| ast
//        return evaluated
//    }
//    
//    let message = 
//        match evaluated with
//        | Choice1Of2 result -> showVal result
//        | Choice2Of2 error -> showError error 
//    
//    Console.WriteLine message
    0