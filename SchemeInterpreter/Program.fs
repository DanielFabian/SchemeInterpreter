module Program

open System
open System.IO
open System.Text
open Lexer
open Microsoft.FSharp.Text.Lexing

type Term = 
    | Term  of int * string * int
    | Const of int
    
type Polynomial = Term list
type TokenStream = LazyList<token * Position * Position>

let tryToken = function
    | LazyList.Cons ((tok, startPos, endPos), rest) -> Some (tok, rest)
    | _ -> None

let parseIndex src = 
    match tryToken src with
    | Some (HAT, src) -> 
        match tryToken src with
        | Some (INT exp, src) -> exp, src
        | _ -> failwith "expected an integer after '^'"
    | _ -> 1, src

let parseTerm src =
    match tryToken src with
    | Some (INT num, src) ->
        match tryToken src with
        | Some (ID id, src) ->
            let idx, src = parseIndex src
            Term (num, id, idx), src
        | _ -> Const num, src
    | Some (ID id, src) ->
        let idx, src = parseIndex src
        Term (1, id, idx), src
    | _ -> failwith "end of token steam in term"

let negateTerm = function
    | Term (fac, var, exp) -> Term (-fac, var, exp)
    | Const c -> Const -c

let rec parsePolynomial src = 
    let t1, src = parseTerm src
    match tryToken src with
    | Some (PLUS, src) ->
        let p2, src = parsePolynomial src
        (t1 :: p2), src
    | Some (MINUS, src) ->
        let p2, src = parsePolynomial src
        match p2 with
        | t2 :: rest -> (t1 :: negateTerm t2 :: rest), src
        | _ -> failwith "no term after a -"
    | _ -> [t1], src
    
let getTokenStream inp =
    seq {
        let lexbuf = LexBuffer<_>.FromString inp
        while not lexbuf.IsPastEndOfStream do
            match Lexer.token lexbuf with
            | EOF -> yield! []
            | token -> yield (token, lexbuf.StartPos, lexbuf.EndPos) }
    |> LazyList.ofSeq    

let parse input = 
    let src = getTokenStream input
    let result, src = parsePolynomial src
    match tryToken src with
    | Some _ -> failwith "unexpected input at end of token stream!"
    | None -> result

[<EntryPoint>]
let main _ = 

    let res = parse "x^5 - 2x^3 + 20"
    printf "%A" res      
    0