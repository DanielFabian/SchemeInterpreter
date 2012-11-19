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
    
    let evaluate text = choose {
        let! ast = parse text
        let! evaluated = eval ast
        return evaluated
    }
    
    let rec repl() =
        let text = 
            Console.Write "Lisp>>> "
            Console.ReadLine()
        if text = "quit" then ()
        else
            evaluate text
            |> choice showVal showError
            |> Console.WriteLine
            repl()
    repl()
    0