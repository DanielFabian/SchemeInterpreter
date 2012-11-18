module Primitives

open Ast
open System.Linq

open FSharpx.Prelude
open FSharpx.Choice

type ListMonad() =
   member o.Bind (m, f) = List.concat <| List.map f m
   member o.Return x = [x]

let list = ListMonad()
let choose = EitherBuilder()

type LispError =
    | NumArgs of int * LispVal list
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
    | String (Int32 num) -> returnM num
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
    | goodArgs -> choose {
        let! goodArgs = goodArgs |> mapM unpackNum
        return Number <| List.reduce f goodArgs }

let numBool f = function
    | [left; right] -> choose { 
        let! left = unpackNum left
        let! right = unpackNum right
        return Bool <| f left right }
    | badArgs -> Choice2Of2 <| NumArgs (2, badArgs)

let bool f = function
    | [left; right] -> choose { 
        let! left = unpackBool left
        let! right = unpackBool right
        return Bool <| f left right }
    | badArgs -> Choice2Of2 <| NumArgs (2, badArgs)

let strBool f = function
    | [left; right] -> choose {
        let! left = unpackString left
        let! right = unpackString right
        return Bool <| f left right }
    | badArgs -> Choice2Of2 <| NumArgs (2, badArgs)
            
let car = function
    | [List (x::xs)] -> returnM x
    | [DottedList (x::xs, _)] -> returnM x
    | [badArg] -> Choice2Of2 <| TypeMismatch ("pair", badArg)
    | badArgList -> Choice2Of2 <| NumArgs (1, badArgList)

let cdr = function
    | [List (x::xs)] -> returnM <| List xs
    | [DottedList ([_], x)] -> returnM x
    | [DottedList (_::xs, x)] -> returnM <| DottedList (xs, x)
    | [badArg] -> Choice2Of2 <| TypeMismatch ("pair", badArg)
    | badArgList -> Choice2Of2 <| NumArgs (1, badArgList)
    
let cons = function
    | [x; List xs] -> returnM <| List (x::xs)
    | [x; DottedList (xs, xlast)] -> returnM <| DottedList (x::xs, xlast)
    | [x1; x2] -> returnM <| DottedList ([x1], x2)
    | badArgList -> Choice2Of2 <| NumArgs (2, badArgList)

let compareList eqv eqvPair leftArgs rightArgs =
    choose {
        if List.length leftArgs <> List.length rightArgs
        then return Bool false
        else
            let lazyEqualities = List.zip leftArgs rightArgs |> Seq.map eqvPair
            return Bool (lazyEqualities.All (fun x -> x)) }
    
let rec eqv = function
    | [Bool arg1; Bool arg2] -> returnM <| Bool (arg1 = arg2)
    | [Number arg1; Number arg2] -> returnM <| Bool (arg1 = arg2)
    | [String arg1; String arg2] -> returnM <| Bool (arg1 = arg2)
    | [Atom arg1; Atom arg2] -> returnM <| Bool (arg1 = arg2)
    | [DottedList (xs, x); DottedList (ys, y)] -> eqv [List (xs @ [x]); List (ys @ [y])]
    | [List leftArgs; List rightArgs] ->
        let eqvPair (leftArg, rightArg) = 
                match eqv [leftArg; rightArg] with
                | Choice1Of2 (Bool value) -> value
                | _ -> false
        compareList eqv eqvPair leftArgs rightArgs
    | [_; _] -> returnM <| Bool false
    | badArgList -> Choice2Of2 <| NumArgs (2, badArgList)

let rec equal = function
    | [Atom arg1; Atom arg2] -> returnM <| Bool (arg1 = arg2)
    | [DottedList (xs, x); DottedList (ys, y)] -> eqv [List (xs @ [x]); List (ys @ [y])]
    | [List leftArgs; List rightArgs] -> compareList equal primitiveEquals leftArgs rightArgs
    | [arg1; arg2] -> returnM <| Bool (primitiveEquals (arg1, arg2))
    | badArgList -> Choice2Of2 <| NumArgs (2, badArgList)
and primitiveEquals (arg1, arg2) =
    list {
        let compareUsing unpacker arg1 arg2 =
            match unpacker arg1, unpacker arg2 with
            | Choice1Of2 left, Choice1Of2 right -> left = right
            | _ -> false

        let! comparer = [compareUsing unpackBool
                         compareUsing unpackNum
                         compareUsing unpackString]
        return comparer arg1 arg2 }
    |> List.reduce (||)