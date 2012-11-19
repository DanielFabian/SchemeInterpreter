module Evaluator

open Ast
open Primitives
open FSharpx.Choice
open FSharpx.Prelude
open FSharpx

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
                   ("string<?",    strBool ( < ))
                   ("string>?",    strBool ( > ))
                   ("string<=?",   strBool ( <= ))
                   ("string>=?",   strBool ( >= ))
                   ("car",         car)
                   ("cdr",         cdr)
                   ("cons",        cons)
                   ("eq?",         eqv)
                   ("eqv?",        eqv)
                   ("equal?",      equal)]

let isBound envRef var = Map.containsKey var envRef
let defineVar envRef var value = Map.add var value envRef

let getVar envRef var = 
    Map.tryFind var envRef
    |> flip fromOption <| UnboundVar ("Getting an unbound variable", var)

let setVar envRef var value = 
    if isBound envRef var then        
        Choice1Of2 (defineVar envRef var value)
    else
        Choice2Of2 <| UnboundVar ("Setting an unbound variable", var)

let rec eval envRef = function
    | String _ as res -> returnM (res, envRef)
    | List ([Atom "quote"; res]) -> returnM (res, envRef)
    | Bool _ as res -> returnM (res, envRef)
    | Float _ as res -> returnM (res, envRef)
    | Number _ as res -> returnM (res, envRef)
    | Atom id -> choose {
        let! var = getVar envRef id
        return var, envRef }
    | List ([Atom "if"; pred; conseq; alt]) -> choose {
        let! pred = eval envRef pred
        let! result =
            match fst pred with
            | Bool false -> eval envRef alt
            | Bool true -> eval envRef conseq
            | badValue -> Choice2Of2 <| TypeMismatch ("bool", badValue)
        return result }
    | List ([Atom "set!"; Atom var; form]) -> choose {
        let! (value,_) = eval envRef form
        let! newEnv = setVar envRef var form 
        return value, newEnv }
    | List ([Atom "define"; Atom var; form]) -> choose {
        let! (value,_) = eval envRef form
        let newEnv = defineVar envRef var value
        return value, newEnv }
    | List (Atom funcName :: args) -> choose {
        let! evaluatedArgs = mapM (eval envRef) args
        let! func = Map.tryFind funcName primitives |> Choice.fromOption (NotFunction ("Unrecognized primitive function args", funcName))
        let! result = apply (PrimitiveFunc func) (List.map fst evaluatedArgs)
        return result, envRef }
    | badForm -> Choice2Of2 <| BadSpecialForm ("Unrecognized special form", badForm)
    
and apply func args = 
    match func with
    | PrimitiveFunc func -> func args
    | CodedFunc ({parameters = parameters; vararg = vararg; body = body; closure = closure}) -> 
        let rec bindParameters env pars arguments =
            match pars, vararg, arguments with
            | x::xs, _, y::ys -> bindParameters (defineVar env x y) xs ys
            | _::_, _, [] 
            | [], None, _::_ -> Choice2Of2 <| NumArgs (List.length parameters, args)
            | [], Some argName, argList -> Choice1Of2 (defineVar env argName <| List argList)
            | [], None, [] -> Choice1Of2 env
        choose {
            let! env = bindParameters closure.env parameters args
            let! result = mapM (eval env) body
            return List.map fst result |> List.rev |> List.head
        }