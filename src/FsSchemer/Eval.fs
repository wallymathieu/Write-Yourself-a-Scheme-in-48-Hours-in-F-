namespace Lisp

module Eval =

    open FParsec.Primitives
    open FParsec.CharParsers

    open Ast
    open Parser
    open Errors
    open SymbolTable

    open System.IO

    let readOrThrow parser input =
        match run parser input with
        | Success (v, _, _) -> v
        | Failure (msg, err, _) -> raise (LispException(ParseError(msg, err)))

    let readExpr = readOrThrow parseExpr
    let readExprList = readOrThrow (endBy parseExpr spaces)

    // 6. Gets the number out of a LispVal, it works for Lists ...
    let rec unpackNum = function
        | Number n  -> n
        | String n  -> let success, result = System.Int32.TryParse n
                       if success
                           then result
                           else throw (TypeMismatch("number", String n))
        | List [n]  -> unpackNum n
        | notNumber -> throw (TypeMismatch("number", notNumber))

    let rec unpackStr = function
        | String s -> s
        | Number n -> n.ToString()
        | Bool b   -> b.ToString()
        | List [s]  -> unpackStr s
        | noString -> throw (TypeMismatch("string", noString))

    let rec unpackBool = function
        | Bool b -> b
        | List [b]  -> unpackBool b
        | noBool -> throw (TypeMismatch("boolean", noBool))

    // 11. Unpacking arguments to support equality
    let tryUnpacker (unpack : LispVal -> 'a) (op : 'a -> 'a -> bool) arg1 arg2 =
        try op (unpack arg1) (unpack arg2) with _ -> false

    let numUnpackEq = tryUnpacker unpackNum (=)
    let strUnpackEq = tryUnpacker unpackStr (=)
    let boolUnpackEq = tryUnpacker unpackBool (=)

    // 5. fold1 doesn't exist in F# ...
    let foldl1 op = function
        | h::t -> List.fold op h t
        | [] -> throw (Default("Expected a not empty list, got an empty list"))

    // 4. unpack a number, applies the right operator and convert to a Number
    let numericBinop op parms =
        if List.length parms < 2
            then throw <| NumArgs(2, parms)
            else parms |> List.map unpackNum |> foldl1 op |> Number

    // 6. Unpack args and apply the binary operator
    let boolBinop unpacker op args =
        match args with
        | [ left; right ] -> Bool (op (unpacker left) (unpacker right))
        | _ -> throw (NumArgs(2,args))


    // 7. Example of currying ...
    let numBoolBinop = boolBinop unpackNum
    let strBoolBinop = boolBinop unpackStr
    let boolBoolBinop = boolBinop unpackBool

    // 8. Trivially how car, cdr and cons work
    let car = function
        | [List (x :: _)] -> x
        | [DottedList (x :: _, _)] -> x
        | [badArg] -> throw (TypeMismatch("pair", badArg))
        | badArgList -> throw (NumArgs(1, badArgList))

    let cdr = function
        | [List (x :: xs)] -> List xs
        | [DottedList ([xs], x)] -> x
        | [DottedList ((_ :: xs), x)] -> DottedList (xs, x)
        | [badArg] -> throw (TypeMismatch("pair", badArg))
        | badArgList -> throw (NumArgs(1, badArgList))

    let cons = function
        | [x; List xs] -> List (x :: xs)
        | [x; DottedList (xs, xlast)] -> DottedList (x :: xs, xlast)
        | [x1; x2] -> DottedList([x1], x2)
        | badArgList -> throw (NumArgs(2, badArgList))

    // 9. equivalence for list just zipping and looking at the pairs
    let rec eqvPrim e1 e2 =
        match e1, e2 with
        | (Bool b1, Bool b2) -> b1 = b2
        | (Number n1, Number n2) -> n1 = n2
        | (String s1, String s2) -> s1 = s2
        | (Atom a1, Atom a2) -> a1 = a2
        | (DottedList (xs, x), DottedList(ys, y)) -> eqvPrim (List (xs @ [x])) (List (ys @ [y]))
        | (List l1, List l2) -> l1.Length = l2.Length && List.forall2 eqvPrim l1 l2
        | _ -> false

    let eqv = function
              | [e1; e2] -> Bool (eqvPrim e1 e2)
              | badArgList -> throw (NumArgs (2, badArgList))

    // 10. Try eqv and then unpacking to numbers, strings or bools
    let equal = function
        | [arg1; arg2] ->
            let unpackEqual = numUnpackEq arg1 arg2 ||
                              strUnpackEq arg1 arg2 ||
                              boolUnpackEq arg1 arg2
            Bool (eqvPrim arg1 arg2 || unpackEqual)
        | argsList -> throw (NumArgs(2, argsList))


    // 1b2. Wrapper over any function that operate on a fileName that plays well with the rest of evaluator
    let fileIOFunction func = function
        | [String fileName] -> func (fileName)
        | [] -> throw (IOError("No file name"))
        | args -> throw (NumArgs(1, args))

    // 13. Creates & close a LispVal Port (just a wrapper for a filestream)
    let makePort fileAccess = fileIOFunction (fun fileName ->
                                File.Open(fileName,FileMode.OpenOrCreate, fileAccess) |> Port)

    let closePort = function
                    | [Port(port)] -> port.Close() ; Bool true
                    | _ -> Bool false

    // 14. Read and write from the port or console
    let rec readProc port =
        let parseReader (reader:TextReader) = reader.ReadLine() |> readExpr
        match port with
           | [] -> parseReader(System.Console.In)
           | [Port(port)] ->
                use reader = new StreamReader(port)
                parseReader (reader)
           | args -> throw (NumArgs(1, args))

    let writeProc objPort =
        let write obj (writer: TextWriter) = writer.Write(showVal obj) ; Bool true
        match objPort with
        | [obj] -> write obj (System.Console.Out)
        | [obj ; Port(port)] ->
            use writer = new StreamWriter(port)
            write obj writer
        | args -> throw (NumArgs(1, args))

    let readContents = fileIOFunction (fun fileName -> File.ReadAllText(fileName) |> String)

    // 1b1. Read the text and expression list. readExprList is part of parsing so we'll look at it later.
    let load = fileIOFunction (fun fileName -> File.ReadAllText(fileName)
                                               |> readExprList)

    let readAll fileName = load fileName |> List

    // 1c. Create a function LispVal (note conversion of parms to strings as they are params names)
    // (notice closure captures the current environment)
    let makeFunc varargs env parms body =
                Func ({parms = (List.map showVal parms); varargs = varargs; body = body; closure = env})
    let makeNormalFunc = makeFunc None
    let makeVarargs = showVal >> Some >> makeFunc

    let rec last = function
        | hd :: [] -> hd
        | hd :: tl -> last tl
        | _ -> failwith "Empty list."

    // 3. Contains all primitive operators, PrimitiveBindings in Repl.fs takes this list
    // and creates tuples in env (op name, primitiveFunc (function))
    let rec primitives =
         [
            "+",    numericBinop (+)
            "-",    numericBinop (-)
            "*",    numericBinop (*)
            "/",    numericBinop (/)
            "mod",  numericBinop (%)
            "=",    numBoolBinop (=)
            "<",    numBoolBinop (<)
            ">",    numBoolBinop (>)
            "/=",   numBoolBinop (<>)
            ">=",   numBoolBinop (>=)
            "<=",   numBoolBinop (<=)
            "&&",   boolBoolBinop (&&)
            "||",   boolBoolBinop (||)
            "string=?",     strBoolBinop (=)
            "string>?",      strBoolBinop (>)
            "string<?",      strBoolBinop (<)
            "string<=?",    strBoolBinop (<=)
            "string>=?",    strBoolBinop (>=)
            "car",  car
            "cdr",  cdr
            "cons", cons
            "eq?", eqv
            "eqv?", eqv
            "equal?", equal

            // IO primitives
            "apply", applyProc
            "open-input-file", makePort FileAccess.Read
            "open-output-file", makePort FileAccess.Write
            "close-input-port", closePort
            "close-output-port", closePort
            "read", readProc
            "write", writeProc
            "read-contents", readContents
            "read-all", readAll
         ]

    // 2. If it is a Func (user defined), check parms correctness, then bind the parameters and their values
    // in the closure env, bind the variable args and evaluate the body
    // evaluating the body means just evaluating all the expressions in it and returning the last one
    // Still, we need to describe how to bind primitive functions (see 3)
    and apply func args =
        match func with
        | PrimitiveFunc(f) -> f args
        | Func ({parms = parms; varargs = varargs; body = body; closure = closure}) ->
            let invalidNonVarargs = args.Length <> parms.Length && varargs.IsNone
            let invalidVarargs = args.Length < parms.Length && varargs.IsSome

            if invalidVarargs || invalidNonVarargs
            then
                throw (NumArgs(parms.Length, args))
            else
                let remainingArgs = args |> Seq.skip parms.Length |> Seq.toList
                let evalBody env = body |> List.map (eval env) |> last
                let rec zip xs1 xs2 acc =
                    match xs1, xs2 with
                    | x1::xs1, x2::xs2 -> zip xs1 xs2 ((x1, x2)::acc)
                    | _ -> acc
                let bindVarArgs arg env =
                    match arg with
                    | Some(argName) -> bindVars [argName, (List remainingArgs)] env
                    | None -> env
                bindVars (zip parms args []) closure
                    |> bindVarArgs varargs
                    |> evalBody
        | funcName -> throw (NotFunction("Expecting a function, getting ", showVal funcName))
    and
        // 1. Evaluate a LispVal in the context of an env, return a LispVal
        // . primitive values evaluate as themselves
        // . Atom evaluates as getting a var from the env
        // . quote returns the unevaluated self
        // . if does evalif (see 1a)
        // . load load the file (see 1b), evaluate all the expressions and return the last
        // . set! insert in the symbol table the var and the evaluated value, throws if don't exist
        // . define does the same as set! but doesn't throw and manages functions (see below)
        // . lambda and define create function slots in the symbol table (see 1c)
        // . evaluating functions,
        // . a. evaluate the function form (yes, in Lisp it can be whatever grammar form)
        // . b. evaluate all the args (applicative order, not normal in Lisp)
        // . c. call apply (see 2)
        eval env = function
        | String _ as v -> v
        | Number _ as v -> v
        | Bool _ as v -> v
        | Atom var -> getVar var env
        | List [Atom "quote"; v] -> v
        | List [Atom "if"; pred; conseq; alt] -> evalIf env pred conseq alt
        // do set! and define return the value of the variable C-like? This assumes so ...
        | List [Atom "load"; fileName] -> load [fileName] |> List.map (eval env) |> last
        | List [Atom "set!" ; Atom var ; form] -> env |> setVar var (eval env form)
        | List [Atom "define"; Atom var; form] -> define env var (eval env form)
        | List (Atom "define" :: (List (Atom var :: parms) :: body)) ->
            makeNormalFunc env parms body |> define env var
        | List (Atom "define" :: (DottedList ((Atom var :: parms), varargs) :: body)) ->
            makeVarargs varargs env parms body |> define env var
        | List (Atom "lambda" :: (List parms :: body)) -> makeNormalFunc env parms body
        | List (Atom "lambda" :: (DottedList(parms, varargs) :: body)) -> makeVarargs varargs env parms body
        | List (Atom "lambda" :: ((Atom _) as varargs :: body)) -> makeVarargs varargs env [] body
        | List (func :: args) ->
            let f = eval env func
            let argVals = List.map (eval env) args
            apply f argVals
        | badForm -> throw (BadSpecialForm("Unrecognized special form", badForm))
    and
        // 1a. If the evaluation of the pred is false evaluate alt, else evaluate cons
        evalIf env pred conseq alt =
            match eval env pred with
            | Bool(false) -> eval env alt
            | _ -> eval env conseq
    and
        // 12. apply implemented in terms of apply, hence needs to be in the and
        applyProc = function
                    | [func; List args] -> apply func args
                    | func :: args -> apply func args
                    | [] -> throw (Default("Expecting a function, got an empty list"))
