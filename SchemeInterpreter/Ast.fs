module Ast

type LispVal =
    | Atom of string
    | List of LispVal list
    | DottedList of LispVal list * LispVal
    | Number of int
    | String of string
    | Bool of bool
    | Float of double
    
type LispProgram = Prog of LispVal list