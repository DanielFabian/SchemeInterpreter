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
            | Prog (head::tail) -> Choice1Of2 head
            | _ -> Choice2Of2 <| ParserError "Empty program"
        with e ->
            let pos = lexbuf.EndPos
            let message = sprintf "Error near line %d, character %d\n" pos.Line pos.Column
            Choice2Of2 <| ParserError message
    
    let evaluate env text = choose {
        let! ast = parse text
        let! evaluated = eval env ast
        return evaluated
    }
    
    let rec repl env =
        let text = 
            Console.Write "Lisp>>> "
            Console.ReadLine()
        if text = "quit" then ()
        else
            match evaluate env text with
            | Choice1Of2 (value, newEnv) -> 
                Console.WriteLine (showVal value)
                repl newEnv
            | Choice2Of2 error -> 
                Console.WriteLine (showError error)
                repl env
    repl primitiveBindings
    0