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
  | WHILE
  | DO
  | BEGIN
  | END
  | PRINT
  | SEMI
  | ASSIGN
  | EOF
  | PLUS
  | MINUS
  | TIMES
  | LPAREN
  | RPAREN
  | IF
  | THEN
  | ELSE
  | INT of (int)
  | ID of (string)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_WHILE
    | TOKEN_DO
    | TOKEN_BEGIN
    | TOKEN_END
    | TOKEN_PRINT
    | TOKEN_SEMI
    | TOKEN_ASSIGN
    | TOKEN_EOF
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_TIMES
    | TOKEN_LPAREN
    | TOKEN_RPAREN
    | TOKEN_IF
    | TOKEN_THEN
    | TOKEN_ELSE
    | TOKEN_INT
    | TOKEN_ID
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_Prog
    | NONTERM_Expr
    | NONTERM_Stmt
    | NONTERM_StmtList

// This function maps tokens to integers indexes
let tagOfToken (t:token) = 
  match t with
  | WHILE  -> 0 
  | DO  -> 1 
  | BEGIN  -> 2 
  | END  -> 3 
  | PRINT  -> 4 
  | SEMI  -> 5 
  | ASSIGN  -> 6 
  | EOF  -> 7 
  | PLUS  -> 8 
  | MINUS  -> 9 
  | TIMES  -> 10 
  | LPAREN  -> 11 
  | RPAREN  -> 12 
  | IF  -> 13 
  | THEN  -> 14 
  | ELSE  -> 15 
  | INT _ -> 16 
  | ID _ -> 17 

// This function maps integers indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_WHILE 
  | 1 -> TOKEN_DO 
  | 2 -> TOKEN_BEGIN 
  | 3 -> TOKEN_END 
  | 4 -> TOKEN_PRINT 
  | 5 -> TOKEN_SEMI 
  | 6 -> TOKEN_ASSIGN 
  | 7 -> TOKEN_EOF 
  | 8 -> TOKEN_PLUS 
  | 9 -> TOKEN_MINUS 
  | 10 -> TOKEN_TIMES 
  | 11 -> TOKEN_LPAREN 
  | 12 -> TOKEN_RPAREN 
  | 13 -> TOKEN_IF 
  | 14 -> TOKEN_THEN 
  | 15 -> TOKEN_ELSE 
  | 16 -> TOKEN_INT 
  | 17 -> TOKEN_ID 
  | 20 -> TOKEN_end_of_input
  | 18 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_Prog 
    | 3 -> NONTERM_Expr 
    | 4 -> NONTERM_Expr 
    | 5 -> NONTERM_Expr 
    | 6 -> NONTERM_Expr 
    | 7 -> NONTERM_Expr 
    | 8 -> NONTERM_Expr 
    | 9 -> NONTERM_Stmt 
    | 10 -> NONTERM_Stmt 
    | 11 -> NONTERM_Stmt 
    | 12 -> NONTERM_Stmt 
    | 13 -> NONTERM_Stmt 
    | 14 -> NONTERM_Stmt 
    | 15 -> NONTERM_StmtList 
    | 16 -> NONTERM_StmtList 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 20 
let _fsyacc_tagOfErrorTerminal = 18

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | WHILE  -> "WHILE" 
  | DO  -> "DO" 
  | BEGIN  -> "BEGIN" 
  | END  -> "END" 
  | PRINT  -> "PRINT" 
  | SEMI  -> "SEMI" 
  | ASSIGN  -> "ASSIGN" 
  | EOF  -> "EOF" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | TIMES  -> "TIMES" 
  | LPAREN  -> "LPAREN" 
  | RPAREN  -> "RPAREN" 
  | IF  -> "IF" 
  | THEN  -> "THEN" 
  | ELSE  -> "ELSE" 
  | INT _ -> "INT" 
  | ID _ -> "ID" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | WHILE  -> (null : System.Object) 
  | DO  -> (null : System.Object) 
  | BEGIN  -> (null : System.Object) 
  | END  -> (null : System.Object) 
  | PRINT  -> (null : System.Object) 
  | SEMI  -> (null : System.Object) 
  | ASSIGN  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | TIMES  -> (null : System.Object) 
  | LPAREN  -> (null : System.Object) 
  | RPAREN  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | THEN  -> (null : System.Object) 
  | ELSE  -> (null : System.Object) 
  | INT _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | ID _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 1us; 65535us; 0us; 2us; 8us; 65535us; 14us; 6us; 15us; 7us; 16us; 8us; 17us; 9us; 20us; 10us; 21us; 11us; 27us; 12us; 32us; 13us; 6us; 65535us; 0us; 33us; 22us; 23us; 24us; 33us; 28us; 29us; 30us; 31us; 34us; 35us; 2us; 65535us; 0us; 3us; 24us; 25us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 5us; 14us; 21us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 2us; 2us; 16us; 1us; 3us; 1us; 4us; 4us; 5us; 5us; 6us; 7us; 4us; 5us; 6us; 6us; 7us; 4us; 5us; 6us; 7us; 7us; 4us; 5us; 6us; 7us; 8us; 4us; 5us; 6us; 7us; 9us; 4us; 5us; 6us; 7us; 10us; 5us; 5us; 6us; 7us; 12us; 13us; 4us; 5us; 6us; 7us; 14us; 1us; 5us; 1us; 6us; 1us; 7us; 1us; 8us; 1us; 8us; 1us; 9us; 1us; 9us; 1us; 10us; 1us; 10us; 1us; 10us; 1us; 11us; 2us; 11us; 16us; 1us; 11us; 2us; 12us; 13us; 2us; 12us; 13us; 2us; 12us; 13us; 1us; 13us; 1us; 13us; 1us; 14us; 1us; 15us; 1us; 16us; 1us; 16us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 9us; 11us; 13us; 18us; 23us; 28us; 33us; 38us; 43us; 49us; 54us; 56us; 58us; 60us; 62us; 64us; 66us; 68us; 70us; 72us; 74us; 76us; 79us; 81us; 84us; 87us; 90us; 92us; 94us; 96us; 98us; 100us; |]
let _fsyacc_action_rows = 36
let _fsyacc_actionTableElements = [|5us; 32768us; 0us; 21us; 2us; 24us; 4us; 32us; 13us; 27us; 17us; 19us; 0us; 49152us; 0us; 16385us; 1us; 16386us; 5us; 34us; 0us; 16387us; 0us; 16388us; 1us; 16389us; 10us; 16us; 1us; 16390us; 10us; 16us; 0us; 16391us; 4us; 32768us; 8us; 14us; 9us; 15us; 10us; 16us; 12us; 18us; 3us; 16393us; 8us; 14us; 9us; 15us; 10us; 16us; 4us; 32768us; 1us; 22us; 8us; 14us; 9us; 15us; 10us; 16us; 4us; 32768us; 8us; 14us; 9us; 15us; 10us; 16us; 14us; 28us; 3us; 16398us; 8us; 14us; 9us; 15us; 10us; 16us; 3us; 32768us; 11us; 17us; 16us; 5us; 17us; 4us; 3us; 32768us; 11us; 17us; 16us; 5us; 17us; 4us; 3us; 32768us; 11us; 17us; 16us; 5us; 17us; 4us; 3us; 32768us; 11us; 17us; 16us; 5us; 17us; 4us; 0us; 16392us; 1us; 32768us; 6us; 20us; 3us; 32768us; 11us; 17us; 16us; 5us; 17us; 4us; 3us; 32768us; 11us; 17us; 16us; 5us; 17us; 4us; 5us; 32768us; 0us; 21us; 2us; 24us; 4us; 32us; 13us; 27us; 17us; 19us; 0us; 16394us; 5us; 32768us; 0us; 21us; 2us; 24us; 4us; 32us; 13us; 27us; 17us; 19us; 2us; 32768us; 3us; 26us; 5us; 34us; 0us; 16395us; 3us; 32768us; 11us; 17us; 16us; 5us; 17us; 4us; 5us; 32768us; 0us; 21us; 2us; 24us; 4us; 32us; 13us; 27us; 17us; 19us; 1us; 16396us; 15us; 30us; 5us; 32768us; 0us; 21us; 2us; 24us; 4us; 32us; 13us; 27us; 17us; 19us; 0us; 16397us; 3us; 32768us; 11us; 17us; 16us; 5us; 17us; 4us; 0us; 16399us; 5us; 32768us; 0us; 21us; 2us; 24us; 4us; 32us; 13us; 27us; 17us; 19us; 0us; 16400us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 6us; 7us; 8us; 10us; 11us; 12us; 14us; 16us; 17us; 22us; 26us; 31us; 36us; 40us; 44us; 48us; 52us; 56us; 57us; 59us; 63us; 67us; 73us; 74us; 80us; 83us; 84us; 88us; 94us; 96us; 102us; 103us; 107us; 108us; 114us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 1us; 1us; 1us; 1us; 3us; 3us; 3us; 3us; 3us; 4us; 3us; 4us; 6us; 2us; 1us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 3us; 3us; 3us; 3us; 3us; 3us; 4us; 4us; 4us; 4us; 4us; 4us; 5us; 5us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 16385us; 65535us; 16387us; 16388us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16392us; 65535us; 65535us; 65535us; 65535us; 16394us; 65535us; 65535us; 16395us; 65535us; 65535us; 65535us; 65535us; 16397us; 65535us; 16399us; 65535us; 16400us; |]
let _fsyacc_reductions ()  =    [| 
# 188 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Ast.prog)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 197 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Prog)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 19 "Parser.fsy"
                                                           _1 
                   )
# 19 "Parser.fsy"
                 : Ast.prog));
# 208 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'StmtList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 21 "Parser.fsy"
                                                           Prog (List.rev _1) 
                   )
# 21 "Parser.fsy"
                 : 'Prog));
# 219 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 23 "Parser.fsy"
                                                           Val _1 
                   )
# 23 "Parser.fsy"
                 : 'Expr));
# 230 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 24 "Parser.fsy"
                                                           Int _1 
                   )
# 24 "Parser.fsy"
                 : 'Expr));
# 241 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 25 "Parser.fsy"
                                                           Plus (_1, _3) 
                   )
# 25 "Parser.fsy"
                 : 'Expr));
# 253 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 26 "Parser.fsy"
                                                           Minus (_1, _3) 
                   )
# 26 "Parser.fsy"
                 : 'Expr));
# 265 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 27 "Parser.fsy"
                                                           Times (_1, _3) 
                   )
# 27 "Parser.fsy"
                 : 'Expr));
# 277 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 28 "Parser.fsy"
                                                           _2 
                   )
# 28 "Parser.fsy"
                 : 'Expr));
# 288 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "Parser.fsy"
                                                           Assign (_1, _3) 
                   )
# 30 "Parser.fsy"
                 : 'Stmt));
# 300 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'Stmt)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 "Parser.fsy"
                                                           While (_2, _4) 
                   )
# 31 "Parser.fsy"
                 : 'Stmt));
# 312 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'StmtList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 "Parser.fsy"
                                                           Seq (List.rev _2) 
                   )
# 32 "Parser.fsy"
                 : 'Stmt));
# 323 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'Stmt)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "Parser.fsy"
                                                           IfThen (_2, _4) 
                   )
# 33 "Parser.fsy"
                 : 'Stmt));
# 335 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'Stmt)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : 'Stmt)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "Parser.fsy"
                                                           IfThenElse (_2, _4, _6) 
                   )
# 34 "Parser.fsy"
                 : 'Stmt));
# 348 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'Expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 "Parser.fsy"
                                                           Print _2 
                   )
# 35 "Parser.fsy"
                 : 'Stmt));
# 359 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Stmt)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "Parser.fsy"
                                                           [_1] 
                   )
# 38 "Parser.fsy"
                 : 'StmtList));
# 370 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'StmtList)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Stmt)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "Parser.fsy"
                                                           _3 :: _1 
                   )
# 39 "Parser.fsy"
                 : 'StmtList));
|]
# 383 "Parser.fs"
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
    numTerminals = 21;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : Ast.prog =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))