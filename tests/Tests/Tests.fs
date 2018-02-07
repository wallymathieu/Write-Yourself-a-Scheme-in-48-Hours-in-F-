module Tests


open Expecto
open Lisp
open Repl
open Parser
let eval env = evalString env >> showVal
let initEnv () = primitiveBindings () |> loadStdLib

let test tests =
    let env = initEnv ()
    tests |> List.iter (fun (expr, result) -> Expect.equal result (eval env expr) "Should match expected result")


[<Tests>]
let tests =
  testList "Main tests" [
    testCase "simpleEval" <| fun _ ->
        let tests = [
            "(+ 2 2)", "4"
            "(+ 2 (- 4 1))", "5"
            "(- (+ 4 6 3) 3 5 2)", "3"
        ]
        test tests
    testCase "errorCheck" <| fun _ ->
        let tests = [
             "(+ 2 \"two\")", "\"Invalid type: expected number, found \"two\"\""
             "(+ 2)", "\"Expected 2 args; found values 2\""
             "(what? 2)", "\"Getting an unbound variable: what?\""
             ]
        test tests
    testCase "moreEval" <| fun _ ->
        let tests = [
             "(< 2 3)", "#t"
             "(> 2 3)", "#f"
             "(>= 3 3)", "#t"
             "(string=? \"test\" \"test\")", "#t"
             "(string=? \"abcd\" \"dsft\")", "#f"
             "(if (> 2 3) \"no\" \"yes\")", "\"yes\""
             "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")", "9"
             "(cdr '(a simple test))", "(simple test)"
             "(car (cdr '(a simple test)))", "simple"
             "(car '((this is) a test))", "(this is)"
             "(cons '(this is) 'test)", "((this is) . test)"
             "(cons '(this is) '())", "((this is))"
             "(eqv? 1 3)", "#f"
             "(eqv? 3 3)", "#t"
             "(eqv? 'atom 'atom)", "#t"
             ]
        test tests
    testCase "assignement" <| fun _ ->
        let tests = [
            "(define x 3)", "3"
            "(+ x 2)", "5"
            "(+ y 2)", "\"Getting an unbound variable: y\""
            "(define y 5)", "5"
            "(+ x (- y 2))", "6"
            "(define str \"A string\")", "\"A string\""
            "(< str \"The string\")", "\"Invalid type: expected number, found \"A string\"\""
            "(string<? str \"The string\")", "#t"
             ]
        test tests

    testCase "closure" <| fun _ ->

        let tests = [
            "(define (f x y) (+ x y))", "(lambda (\"x\" \"y\") ...)"
            "(f 1 2)", "3"
            "(f 1 2 3)", "\"Expected 2 args; found values 1 2 3\""
            "(define (factorial x) (if (= x 1) 1 (* x (factorial (- x 1)))))", "(lambda (\"x\") ...)"
            "(factorial 10)", "3628800"
            "(define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))", "(lambda (\"inc\") ...)"
            "(define my-count (counter 5))", "(lambda (\"x\") ...)"
            "(my-count 3)", "8"
            "(my-count 6)", "14"
            "(my-count 5)", "19"
             ]
        test tests

    testCase "predefinedFunctions" <| fun _ ->
        let tests = [
            "(map (curry + 2) '(1 2 3 4))", "(3 4 5 6)"
            "(filter even? '(1 2 3 4))", "(2 4)"
            ]
        test tests

    testCase "varargsCountCheck" <| fun _ ->
        let tests = [
            "(define (sum x y . lst) (fold + (* x y) lst))", "(lambda (\"x\" \"y\" . lst) ...)"
            "(sum 1 2 3)", "5"
            "(sum 1 1 1)", "2"
            "(sum 1 2)", "2"
            "(sum 1)", "\"Expected 2 args; found values 1\""
             ]
        test tests
  ]
