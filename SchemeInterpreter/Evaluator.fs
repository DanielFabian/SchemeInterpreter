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

let isBound envRef var = Map.containsKey var envRef.env

let defineVar envRef var value = 
    envRef.env <- Map.add var value envRef.env
    value

let getVar envRef var = 
    Map.tryFind var envRef.env
    |> flip fromOption <| UnboundVar ("Getting an unbound variable", var)

let setVar envRef var value = 
    if isBound envRef var then        
        Choice1Of2 (defineVar envRef var value)
    else
        Choice2Of2 <| UnboundVar ("Setting an unbound variable", var)
        
let makeFunc varargs env pars body = CodedFunc {
    parameters = List.map showVal pars
    vararg = varargs
    body = body
    closure = {env = env.env} }
    
let makeNormalFunc = makeFunc None

let makeVarargFunc = makeFunc << Some << showVal

let rec eval envRef = function
    | String _ as res -> returnM res
    | List ([Atom "quote"; res]) -> returnM res
    | Bool _ as res -> returnM res
    | Float _ as res -> returnM res
    | Number _ as res -> returnM res
    | Atom id -> choose {
        let! var = getVar envRef id
        return var }
    | List ([Atom "if"; pred; conseq; alt]) -> choose {
        let! pred = eval envRef pred
        let! result =
            match pred with
            | Bool false -> eval envRef alt
            | Bool true -> eval envRef conseq
            | badValue -> Choice2Of2 <| TypeMismatch ("bool", badValue)
        return result }
    | List ([Atom "set!"; Atom var; form]) -> choose {
        let! value = eval envRef form
        let! result = setVar envRef var value 
        return result }
    | List ([Atom "define"; Atom var; form]) -> choose {
        let! value = eval envRef form
        return defineVar envRef var value }
    | List (Atom "define" :: List (Atom var :: pars) :: body) -> returnM (defineVar envRef var (makeNormalFunc envRef pars body))
    | List (Atom "define" :: DottedList (Atom var :: pars, varargs) :: body) -> returnM (defineVar envRef var (makeVarargFunc varargs envRef pars body))
    | List (Atom "lambda" :: List pars :: body) -> returnM (makeNormalFunc envRef pars body)
    | List (Atom "lambda" :: DottedList (pars, varargs) :: body) -> returnM (makeVarargFunc varargs envRef pars body)
    | List (Atom "lambda" :: (Atom _ as varargs) :: body) -> returnM (makeVarargFunc varargs envRef [] body)
    | List (func :: args) -> choose {
        let rec evalArgs env argVals = function
            | [] -> Choice1Of2 <| List.rev argVals
            | arg::args -> choose {
                let! argVal = eval env arg
                let! result = evalArgs envRef (argVal::argVals) args
                return result }
        let! func = eval envRef func
        let! argVals = evalArgs envRef [] args
        let! result = apply func argVals
        return result }
    | badForm -> Choice2Of2 <| BadSpecialForm ("Unrecognized special form", badForm)
    
and apply func args = 
    match func with
    | PrimitiveFunc func -> func args
    | CodedFunc ({parameters = parameters; vararg = vararg; body = body; closure = closure}) -> 
        let rec bindParameters env pars arguments =
            match pars, vararg, arguments with
            | x::xs, _, y::ys -> 
                defineVar env x y |> ignore
                bindParameters env xs ys
            | _::_, _, [] 
            | [], None, _::_ -> Choice2Of2 <| NumArgs (List.length parameters, args)
            | [], Some argName, argList ->
                defineVar env argName <| List argList |> ignore
                Choice1Of2 ()
            | [], None, [] -> Choice1Of2 ()
        choose {
            let! _ = bindParameters closure parameters args
            let! result = mapM (eval closure) body
            return result |> List.rev |> List.head
        }
    | _ -> Choice2Of2 <| NotFunction ("Not a function", showVal func)
        
let primitiveBindings =
    let makePrimitiveFunc var func = PrimitiveFunc func
    { env = Map.map makePrimitiveFunc primitives }