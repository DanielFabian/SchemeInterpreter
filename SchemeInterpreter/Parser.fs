// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"

open Ast

# 10 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | EOF
  | LPAREN
  | RPAREN
  | DOT
  | QUOTE
  | BOOL of (bool)
  | FLOAT of (float)
  | INT of (int)
  | STRING of (string)
  | ID of (string)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_EOF
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_DOT
    | TOKEN_QUOTE
    | TOKEN_BOOL
    | TOKEN_FLOAT
    | TOKEN_INT
    | TOKEN_STRING
    | TOKEN_ID
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_Value
    | NONTERM_ValueList

// This function maps tokens to integers indexes
let tagOfToken (t:token) = 
  match t with
  | EOF  -> 0 
  | LPAREN  -> 1 
  | RPAREN  -> 2 
  | DOT  -> 3 
  | QUOTE  -> 4 
  | BOOL _ -> 5 
  | FLOAT _ -> 6 
  | INT _ -> 7 
  | STRING _ -> 8 
  | ID _ -> 9 

// This function maps integers indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_EOF 
  | 1 -> TOKEN_LPAREN 
  | 2 -> TOKEN_RPAREN 
  | 3 -> TOKEN_DOT 
  | 4 -> TOKEN_QUOTE 
  | 5 -> TOKEN_BOOL 
  | 6 -> TOKEN_FLOAT 
  | 7 -> TOKEN_INT 
  | 8 -> TOKEN_STRING 
  | 9 -> TOKEN_ID 
  | 12 -> TOKEN_end_of_input
  | 10 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_Value 
    | 3 -> NONTERM_Value 
    | 4 -> NONTERM_Value 
    | 5 -> NONTERM_Value 
    | 6 -> NONTERM_Value 
    | 7 -> NONTERM_Value 
    | 8 -> NONTERM_Value 
    | 9 -> NONTERM_Value 
    | 10 -> NONTERM_ValueList 
    | 11 -> NONTERM_ValueList 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 12 
let _fsyacc_tagOfErrorTerminal = 10

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | EOF  -> "EOF" 
  | LPAREN  -> "LPAREN" 
  | RPAREN  -> "RPAREN" 
  | DOT  -> "DOT" 
  | QUOTE  -> "QUOTE" 
  | BOOL _ -> "BOOL" 
  | FLOAT _ -> "FLOAT" 
  | INT _ -> "INT" 
  | STRING _ -> "STRING" 
  | ID _ -> "ID" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | EOF  -> (null : System.Object) 
  | LPAREN  -> (null : System.Object) 
  | RPAREN  -> (null : System.Object) 
  | DOT  -> (null : System.Object) 
  | QUOTE  -> (null : System.Object) 
  | BOOL _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | FLOAT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | INT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | STRING _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | ID _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 4us; 65535us; 0us; 2us; 9us; 16us; 11us; 12us; 14us; 15us; 1us; 65535us; 8us; 9us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 8us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 2us; 1us; 3us; 1us; 4us; 1us; 5us; 1us; 6us; 2us; 7us; 8us; 3us; 7us; 8us; 11us; 1us; 7us; 1us; 8us; 1us; 8us; 1us; 8us; 1us; 9us; 1us; 9us; 1us; 11us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 10us; 12us; 14us; 16us; 19us; 23us; 25us; 27us; 29us; 31us; 33us; 35us; |]
let _fsyacc_action_rows = 17
let _fsyacc_actionTableElements = [|7us; 32768us; 1us; 8us; 4us; 14us; 5us; 7us; 6us; 5us; 7us; 4us; 8us; 6us; 9us; 3us; 0us; 49152us; 0us; 16385us; 0us; 16386us; 0us; 16387us; 0us; 16388us; 0us; 16389us; 0us; 16390us; 0us; 16394us; 9us; 32768us; 1us; 8us; 2us; 10us; 3us; 11us; 4us; 14us; 5us; 7us; 6us; 5us; 7us; 4us; 8us; 6us; 9us; 3us; 0us; 16391us; 7us; 32768us; 1us; 8us; 4us; 14us; 5us; 7us; 6us; 5us; 7us; 4us; 8us; 6us; 9us; 3us; 1us; 32768us; 2us; 13us; 0us; 16392us; 7us; 32768us; 1us; 8us; 4us; 14us; 5us; 7us; 6us; 5us; 7us; 4us; 8us; 6us; 9us; 3us; 0us; 16393us; 0us; 16395us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 8us; 9us; 10us; 11us; 12us; 13us; 14us; 15us; 16us; 26us; 27us; 35us; 37us; 38us; 46us; 47us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 1us; 1us; 1us; 1us; 1us; 1us; 3us; 5us; 2us; 0us; 2us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 2us; 3us; 3us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 16385us; 16386us; 16387us; 16388us; 16389us; 16390us; 65535us; 65535us; 16391us; 65535us; 65535us; 16392us; 65535us; 16393us; 16395us; |]
let _fsyacc_reductions ()  =    [| 
# 133 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Ast.LispVal)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 142 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Value)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 19 "Parser.fsy"
                                                                   _1 
                   )
# 19 "Parser.fsy"
                 : Ast.LispVal));
# 153 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 22 "Parser.fsy"
                                                                   Atom _1 
                   )
# 22 "Parser.fsy"
                 : 'Value));
# 164 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 23 "Parser.fsy"
                                                                   Number _1 
                   )
# 23 "Parser.fsy"
                 : 'Value));
# 175 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : float)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 24 "Parser.fsy"
                                                                   Float _1 
                   )
# 24 "Parser.fsy"
                 : 'Value));
# 186 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 25 "Parser.fsy"
                                                                   String _1 
                   )
# 25 "Parser.fsy"
                 : 'Value));
# 197 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bool)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 26 "Parser.fsy"
                                                                   Bool _1 
                   )
# 26 "Parser.fsy"
                 : 'Value));
# 208 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'ValueList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 27 "Parser.fsy"
                                                                   List (List.rev _2) 
                   )
# 27 "Parser.fsy"
                 : 'Value));
# 219 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'ValueList)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'Value)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 28 "Parser.fsy"
                                                                   DottedList (List.rev _2, _4) 
                   )
# 28 "Parser.fsy"
                 : 'Value));
# 231 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'Value)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 29 "Parser.fsy"
                                                                   List ( [Atom "quote"; _2] ) 
                   )
# 29 "Parser.fsy"
                 : 'Value));
# 242 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 "Parser.fsy"
                                                                   [] 
                   )
# 31 "Parser.fsy"
                 : 'ValueList));
# 252 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'ValueList)) in
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'Value)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 "Parser.fsy"
                                                                   _2 :: _1 
                   )
# 32 "Parser.fsy"
                 : 'ValueList));
|]
# 265 "Parser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 13;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : Ast.LispVal =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
