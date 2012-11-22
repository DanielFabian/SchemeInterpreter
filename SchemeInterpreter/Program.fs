module Program

open System
open System.IO
open System.Text

open Ast
open Parser
open Lexer
open Primitives
open Evaluator
open FSharpx.Choice


[<EntryPoint>]
let main args = 
    let parse text = 
        let lexbuf = Lexing.LexBuffer<_>.FromString text
        try
            match start token lexbuf with
            | Prog [] -> Choice2Of2 <| ParserError "Empty program"
            | Prog prog -> Choice1Of2 prog
        with e ->
            let pos = lexbuf.EndPos
            let message = sprintf "Error near line %d, character %d\n" pos.Line pos.Column
            Choice2Of2 <| ParserError message
    
    let evaluate env text = choose {
        let! prog = parse text
        let! evaluated = mapM (eval env) prog
        return evaluated
    }
    
    let rec repl env =
        let text = 
            Console.Write "Lisp>>> "
            Console.ReadLine()
        if text = "quit" then ()
        else
            match evaluate env text with
            | Choice1Of2 resultVals -> resultVals |> List.map showVal |> List.iter Console.WriteLine
            | Choice2Of2 error -> showError error |> Console.WriteLine
            repl env
    repl primitiveBindings
    0