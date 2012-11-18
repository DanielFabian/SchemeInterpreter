module Evaluator

open Ast
open Primitives
open FSharpx.Choice

let primitives = 
    Map.ofList <| [("+",           numeric ( + ))
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
                   ("string>=?",   strBool ( = ))
                   ("car",         car)
                   ("cdr",         cdr)
                   ("cons",        cons)
                   ("eq?",         eqv)
                   ("eqv?",        eqv)
                   ("equal?",      equal)]


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
    | List ([Atom "if"; pred; conseq; alt]) -> choose {
        let! pred = eval pred
        let! result =
            match pred with
            | Bool false -> eval alt
            | Bool true -> eval conseq
            | badValue -> Choice2Of2 <| TypeMismatch ("bool", badValue)
        return result }
    | List (Atom func :: args) -> choose {
        let! evaluatedArgs = mapM eval args
        let! result = apply func evaluatedArgs
        return result }
    | badForm -> Choice2Of2 <| BadSpecialForm ("Unrecognized special form", badForm)