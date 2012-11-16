module Program

open System
open System.IO
open System.Text
open Lexer
open Parser

let parseText text = 
    let lexbuf = Lexing.LexBuffer<_>.FromString text
    try
        start token lexbuf
    with e ->
        let pos = lexbuf.EndPos
        failwithf "Error near line %d, character %d\n" pos.Line pos.Column    

[<EntryPoint>]
let main _ = 

    let sample = @""
    //start "x^5 - 2x^3 + 20"
    0