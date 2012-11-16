module Evaluator

open Ast
open System


let printList showVal = List.map showVal >> String.concat " "

let rec showVal = function
    | String str -> "\"" + str + "\""
    | Atom name -> name
    | Bool true -> "#t"
    | Bool false -> "#f"
    | DottedList (list, value) -> "(" + printList showVal list + " . " + showVal value + ")"
    | Float num -> num.ToString()
    | List list -> "(" + printList showVal list + ")"
    | Number num -> num.ToString()

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let rec unpackNum = function
    | Number num -> num
    | String (Integer num) -> num
    | List ([value]) -> unpackNum value
    | _ -> 0

let lift f = List.map unpackNum >> List.reduce f >> Number
    
// TODO: do something with mod / remainder and "/" / "quotient"
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
    | None -> Bool false

let rec eval = function
    | String _ as res -> res
    | List ([Atom "quote"; res]) -> res
    | Bool _ as res -> res
    | Float _ as res -> res
    | Number _ as res -> res
    | List (Atom func :: args) -> apply func <| List.map eval args
    | _ -> failwith "blehm"