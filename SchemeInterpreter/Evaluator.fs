module Evaluator

open Ast
open System
open FSharpx.Choice

let choice = EitherBuilder()

type LispError = NumArgs of int * LispVal list
               | TypeMismatch of string * LispVal
               | BadSpecialForm of string * LispVal
               | NotFunction of string * string
               | UnboundVar of string * string
               | ParserError of string

let rec showVal = function
    | String str -> "\"" + str + "\""
    | Atom name -> name
    | Bool true -> "#t"
    | Bool false -> "#f"
    | DottedList (list, value) -> "(" + printList list + " . " + showVal value + ")"
    | Float num -> num.ToString()
    | List list -> "(" + printList list + ")"
    | Number num -> num.ToString()
and 
    printList = List.map showVal >> String.concat " "

let showError = function
    | UnboundVar (message, varname) -> message + ": " + varname
    | BadSpecialForm (message, form) -> message + ": " + showVal form
    | NotFunction (message, func) -> message + ": " + func
    | NumArgs (expected, found) -> "Expected " + expected.ToString() + " args; found values " + printList found
    | TypeMismatch (expected, found) -> "Invalid type: expected " + expected + " found " + showVal found
    | ParserError message -> message

let rec unpackNum = function
    | Number num -> returnM num
    | List ([value]) -> unpackNum value
    | _ -> returnM 0

let lift f list = 
    match list with 
    | [] | [_] as badArgs -> Choice2Of2 <| NumArgs (2, badArgs)
    | goodArgs -> choice {
        let! goodArgs = goodArgs |> mapM unpackNum
        return List.reduce f goodArgs }
    
let primitives = 
    Map.ofList <| [ ("+",           lift ( + ))
                    ("-",           lift ( - ))
                    ("*",           lift ( * ))
                    ("/",           lift ( / ))
                    ("mod",         lift ( % ))
                    ("quotient",    lift ( / ))
                    ("remainder",   lift ( % ))]

let apply funcName args =
    match Map.tryFind funcName primitives with
    | Some(func) -> func args
    | None -> Choice2Of2 <| NotFunction ("Unrecognized primitive function args", funcName)

let rec eval = function
    | String _ as res -> returnM res
    | List ([Atom "quote"; res]) -> returnM res
    | Bool _ as res -> returnM res
    | Float _ as res -> returnM res
    | Number _ as res -> returnM res
    | List (Atom func :: args) -> choice {
        let! evaluatedArgs = mapM eval args
        let! result = apply func evaluatedArgs
        return Number result }
    | badForm -> Choice2Of2 <| BadSpecialForm ("Unrecognized special form", badForm)