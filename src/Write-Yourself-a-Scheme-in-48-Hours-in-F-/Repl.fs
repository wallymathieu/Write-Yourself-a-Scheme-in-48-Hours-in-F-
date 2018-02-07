// Overall discussion about the fact that if the code is well written, you should be able to understand it
// easily in isolation from the code it depends upon, hence starting from the top level (main) and follow
// downward toward the implementation
namespace Lisp

module Repl =

    open System
    open FParsec.Primitives
    open FParsec.CharParsers

    open Ast
    open Parser
    open Errors
    open Eval
    open SymbolTable

    // 5. print s
    let printStr (s: string) = Console.Write(s)

    // 4. print s and read a line
    let readPrompt (s: string) = printStr s ; Console.ReadLine ()

    // 6. newline
    let newLine () = Console.WriteLine()

    // 8. parse the string and evaluate the expression
    // note the catch to display an error if things go wrong
    let evalString env expr =
        try
            expr |> readExpr |> eval env
        with
        | LispException(error) -> String (showError error)

    // 7. evaluate a string given an environment, make it a human readable string
    // print it and newline it
    let evalAndPrint env = evalString env >> showVal >> printStr >> newLine

    // 9. Cycle until pred, displaying prompt and evaluating the result
    let rec until pred prompter evaluator =
        let result = prompter ()
        if not (pred result) then
            evaluator result
            until pred prompter evaluator

    // 11. Got through everything in 'primitive' and add it to the environment
    // After this step move to Ast.fs
    let primitiveBindings () =
        bindVars [ for v, f in primitives -> v, PrimitiveFunc f ] (nullEnv ())

    // 10. Calling eval to evaluate the load method
    let loadStdLib env =
        eval env (List [Atom "load"; String "stdlib.scm"]) |> ignore
        env
        //printfn "StdLib loaded ..."

    // 3. Load primitive operations and standard lib and cycle through reading input and evaluating it
    // (evalAndPrint should return an env if we wanted this to be side effect free)
    let runRepl () =
        let env = primitiveBindings () |> loadStdLib
        until (fun s -> s = "Quit" || s = "quit") (fun () -> readPrompt "Lisp>>> ") (evalAndPrint env)

    // 2. Load the primitive operations (+, -, ...), the standard library
    // and add to the environment args.[1..N] as a list of Strings
    // then load the file in args.[0] and execute it
    let runOne (filename : string) (args : list<string>) =
        let env = primitiveBindings ()
                    |> loadStdLib
                    |> bindVars [ "args", List (List.map String args) ]
        List [Atom "load"; String filename] |> eval env |> showVal |> printStr


    // 1. Either run a REPL window or execute one expression
    [<EntryPoint>]
    let main(args: string[]) =
        match Array.toList args with
        | [] -> runRepl ()
        | filename :: args -> runOne filename args
        0

