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