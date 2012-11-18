module Program

open System
open System.IO
open System.Text

open Ast
open Parser
open Lexer
open Primitives
open Evaluator


let parseText text = 
    let lexbuf = Lexing.LexBuffer<_>.FromString text
    try
        match start token lexbuf with
        | Prog (head::tail) -> Choice1Of2 head
        | _ -> Choice2Of2 <| ParserError "Empty program"
    with e ->
        let pos = lexbuf.EndPos
        let message = sprintf "Error near line %d, character %d\n" pos.Line pos.Column
        Choice2Of2 <| ParserError message

[<EntryPoint>]
let main args = 
    let evaluated = choose {
        let! ast = parseText args.[0]
        printfn "%A" ast
        let! evaluated = eval <| ast
        return evaluated
    }
    
    let message = 
        match evaluated with
        | Choice1Of2 result -> showVal result
        | Choice2Of2 error -> showError error 
    
    Console.WriteLine message
    0