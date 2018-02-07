// Discussion how often functional programs end up being about the creation of a data structure (parsing)
// and the evaluation of it. At the end talk about the options moving forward: describing the parser,
// the evaluator or the symbol table
namespace Lisp

// Ported from http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours

module Ast =

    type Env = (string * LispVal ref) list ref

    and FuncRecord = { parms: string list; varargs: string option; body: LispVal list; closure: Env}
    
    and LispVal =
        | Atom of string
        | List of LispVal list
        | DottedList of LispVal list * LispVal
        | Number of int
        | String of string
        | Bool of bool
        | PrimitiveFunc of (LispVal list -> LispVal)
        | Func of FuncRecord
        | Port of System.IO.FileStream