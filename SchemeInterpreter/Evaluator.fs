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
    | NotFunction (message, func) -> sprintf "%s: %A" message func
    | NumArgs (expected, found) -> "Expected " + expected.ToString() + " args; found values " + printList found
    | TypeMismatch (expected, found) -> "Invalid type: expected " + expected + ", found " + showVal found
    | ParserError message -> message

let rec unpackNum = function
    | Number num -> returnM num
    | List ([value]) -> unpackNum value
    | badValue -> Choice2Of2 <| TypeMismatch ("number", badValue)

let rec unpackBool = function
    | Bool value -> returnM value
    | List ([value]) -> unpackBool value
    | badValue -> Choice2Of2 <| TypeMismatch ("bool", badValue)

let rec unpackString = function
    | String str -> returnM str
    | List ([value]) -> unpackString value
    | badValue -> Choice2Of2 <| TypeMismatch ("string", badValue)

let numeric f = function
    | [] | [_] as badArgs -> Choice2Of2 <| NumArgs (2, badArgs)
    | goodArgs -> choice {
        let! goodArgs = goodArgs |> mapM unpackNum
        return Number <| List.reduce f goodArgs }

let numBool f = function
    | [left; right] -> choice { 
        let! left = unpackNum left
        let! right = unpackNum right
        return Bool <| f left right }
    | badArgs -> Choice2Of2 <| NumArgs (2, badArgs)

let bool f = function
    | [left; right] -> choice { 
        let! left = unpackBool left
        let! right = unpackBool right
        return Bool <| f left right }
    | badArgs -> Choice2Of2 <| NumArgs (2, badArgs)

let strBool f = function
    | [left; right] -> choice {
        let! left = unpackString left
        let! right = unpackString right
        return Bool <| f left right }
    | badArgs -> Choice2Of2 <| NumArgs (2, badArgs)
            
let primitives = 
    Map.ofList <| [ ("+",           numeric ( + ))
                    ("-",           numeric ( - ))
                    ("*",           numeric ( * ))
                    ("/",           numeric ( / ))
                    ("mod",         numeric ( % ))
                    ("quotient",    numeric ( / ))
                    ("remainder",   numeric ( % ))
                    ("=",           numBool ( = ))
                    ("<",           numBool ( < ))
                    (">",           numBool ( > ))
                    ("/=",          numBool ( <> ))
                    (">=",          numBool ( >= ))
                    ("<=",          numBool ( <= ))
                    ("&&",          bool ( && ))
                    ("||",          bool ( || ))
                    ("string=?",    strBool ( = ))
                    ("string<?",    strBool ( = ))
                    ("string>?",    strBool ( = ))
                    ("string<=?",   strBool ( = ))
                    ("string>=?",   strBool ( = ))]

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
        return result }
    | badForm -> Choice2Of2 <| BadSpecialForm ("Unrecognized special form", badForm)