module Evaluator

open Ast
open Primitives
open FSharpx.Choice
open FSharpx.Prelude
open FSharpx

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
    closure = env.env }
    
let makeNormalFunc = makeFunc None

let makeVarargFunc = makeFunc << Some << showVal

let apply eval func args = 
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
            let! _ = bindParameters { env = closure } parameters args
            let! result = mapM (eval { env = closure }) body
            return result |> List.rev |> List.head
        }
    | _ -> Choice2Of2 <| NotFunction ("Not a function", showVal func)

let applyProc apply = function
    | [func; List args] -> apply func args
    | func::args -> apply func args
    | list -> Choice2Of2 <| NotFunction ("Not a function", showVal <| List list)

let primitives eval = 
    Map.ofList <| [("+",                    numeric ( + ))
                   ("-",                    numeric ( - ))
                   ("*",                    numeric ( * ))
                   ("/",                    numeric ( / ))
                   ("mod",                  numeric ( % ))
                   ("quotient",             numeric ( / ))
                   ("remainder",            numeric ( % ))
                   ("=",                    numBool ( = ))
                   ("<",                    numBool ( < ))
                   (">",                    numBool ( > ))
                   ("/=",                   numBool ( <> ))
                   (">=",                   numBool ( >= ))
                   ("<=",                   numBool ( <= ))
                   ("&&",                   bool ( && ))
                   ("||",                   bool ( || ))
                   ("string=?",             strBool ( = ))
                   ("string<?",             strBool ( < ))
                   ("string>?",             strBool ( > ))
                   ("string<=?",            strBool ( <= ))
                   ("string>=?",            strBool ( >= ))
                   ("car",                  car)
                   ("cdr",                  cdr)
                   ("cons",                 cons)
                   ("eq?",                  eqv)
                   ("eqv?",                 eqv)
                   ("equal?",               equal)
                   ("apply",                applyProc <| apply eval)
                   ("open-input-file",      makePort System.IO.FileMode.OpenOrCreate)
                   ("open-output-file",     makePort System.IO.FileMode.Truncate)
                   ("close-input-port",     closePort)
                   ("close-output-port",    closePort)
                   ("read",                 readPort)
                   ("write",                writePort)
                   ("read-contents",        readContents)
                   ("read-all",             readAll)]

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
    | List ([Atom "load"; String filename]) -> choose {
        let! statements = load filename
        let rec execute = function
            | [a] -> eval envRef a
            | x::xs -> choose {
                let! _ = eval envRef x
                let! result = execute xs
                return result }
            | _ -> Choice2Of2 <| ParserError "Empty Program"
        let! result = execute statements
        return result }
    | List (func :: args) -> choose {
        let rec evalArgs env argVals = function
            | [] -> Choice1Of2 <| List.rev argVals
            | arg::args -> choose {
                let! argVal = eval env arg
                let! result = evalArgs envRef (argVal::argVals) args
                return result }
        let! func = eval envRef func
        let! argVals = evalArgs envRef [] args
        let! result = apply eval func argVals
        return result }
    | badForm -> Choice2Of2 <| BadSpecialForm ("Unrecognized special form", badForm)
        
let primitiveBindings =
    let makePrimitiveFunc var func = PrimitiveFunc func
    { env = Map.map makePrimitiveFunc <| primitives eval }