use rsmonkey::ast::StatementNode;
use rsmonkey::evaluator::eval;
use rsmonkey::evaluator::EvalNode;
use rsmonkey::lexer::Lexer;
use rsmonkey::object::{Environment, Integer, Object};
use rsmonkey::parser::Parser;

enum TestLiteral {
    IntegerLiteral { value: i64 },
    StringLiteral { value: &'static str },
    ErrorLiteral { message: &'static str },
}

impl TestLiteral {
    fn test_literal(&self, value: &Object) {
        match self {
            TestLiteral::IntegerLiteral { value: v } => {
                if let Object::IntegerObject(io) = value {
                    assert_eq!(io.value, *v);
                } else {
                    panic!(
                        "object is not integer, got {:?}, expected {:?}",
                        value.object_type(),
                        v
                    );
                }
            }
            TestLiteral::StringLiteral { value: s } => {
                if let Object::StringObject(so) = value {
                    assert_eq!(so.value, *s);
                } else {
                    panic!("object is not string, got {:?}", value.object_type());
                }
            }
            TestLiteral::ErrorLiteral { message: m } => {
                if let Object::ErrorObject(eo) = value {
                    assert_eq!(eo.message, *m);
                } else {
                    panic!("object is not erro, got {:?}", value.object_type());
                }
            }
        }
    }
}

#[test]
fn test_eval_integer_expression() {
    struct Test {
        input: &'static str,
        expected: i64,
    }

    let integer_expression_tests = vec![
        Test {
            input: "5",
            expected: 5,
        },
        Test {
            input: "10",
            expected: 10,
        },
        Test {
            input: "-5",
            expected: -5,
        },
        Test {
            input: "-10",
            expected: -10,
        },
        Test {
            input: "5 + 5 + 5 + 5 - 10",
            expected: 10,
        },
        Test {
            input: "2 * 2 * 2 * 2 * 2",
            expected: 32,
        },
        Test {
            input: "-50 + 100 + -50",
            expected: 0,
        },
        Test {
            input: "5 * 2 + 10",
            expected: 20,
        },
        Test {
            input: "5 + 2 * 10",
            expected: 25,
        },
        Test {
            input: "20 + 2 * -10",
            expected: 0,
        },
        Test {
            input: "50 / 2 * 2 + 10",
            expected: 60,
        },
        Test {
            input: "2 * (5 + 10)",
            expected: 30,
        },
        Test {
            input: "3 * 3 * 3 + 10",
            expected: 37,
        },
        Test {
            input: "3 * (3 * 3) + 10",
            expected: 37,
        },
        Test {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            expected: 50,
        },
    ];

    for tt in integer_expression_tests {
        let evaluated = test_eval(tt.input);
        test_integer_object(evaluated, tt.expected);
    }
}

#[test]
fn test_eval_boolean_expression() {
    struct Test {
        input: &'static str,
        expected: bool,
    }

    let boolean_expresssion_tests = vec![
        Test {
            input: "true",
            expected: true,
        },
        Test {
            input: "false",
            expected: false,
        },
        Test {
            input: "1 < 2",
            expected: true,
        },
        Test {
            input: "1 > 2",
            expected: false,
        },
        Test {
            input: "1 < 1",
            expected: false,
        },
        Test {
            input: "1 > 1",
            expected: false,
        },
        Test {
            input: "1 == 1",
            expected: true,
        },
        Test {
            input: "1 != 1",
            expected: false,
        },
        Test {
            input: "1 == 2",
            expected: false,
        },
        Test {
            input: "1 != 2",
            expected: true,
        },
        Test {
            input: "true == true",
            expected: true,
        },
        Test {
            input: "false == false",
            expected: true,
        },
        Test {
            input: "true != true",
            expected: false,
        },
        Test {
            input: "true == false",
            expected: false,
        },
        Test {
            input: "true != false",
            expected: true,
        },
        Test {
            input: "false != true",
            expected: true,
        },
        Test {
            input: "(1 < 2) == true",
            expected: true,
        },
        Test {
            input: "(1 < 2) == false",
            expected: false,
        },
        Test {
            input: "(1 > 2) == true",
            expected: false,
        },
        Test {
            input: "(1 > 2) == false",
            expected: true,
        },
        Test {
            input: r##""hoge" == "hoge""##,
            expected: true,
        },
        Test {
            input: r##""hoge" == "fuga""##,
            expected: false,
        },
        Test {
            input: r##""hoge" != "hoge""##,
            expected: false,
        },
        Test {
            input: r##""hoge" != "fuga""##,
            expected: true,
        },
    ];

    for tt in boolean_expresssion_tests {
        let evaluated = test_eval(tt.input);
        test_boolean_object(evaluated, tt.expected);
    }
}

#[test]
fn test_eval_bang_operator() {
    struct Test {
        input: &'static str,
        expected: bool,
    }

    let bang_tests = vec![
        Test {
            input: "!true",
            expected: false,
        },
        Test {
            input: "!false",
            expected: true,
        },
        Test {
            input: "!5",
            expected: false,
        },
        Test {
            input: "!!true",
            expected: true,
        },
        Test {
            input: "!!false",
            expected: false,
        },
        Test {
            input: "!!5",
            expected: true,
        },
    ];

    for tt in bang_tests {
        let evaluated = test_eval(tt.input);
        test_boolean_object(evaluated, tt.expected);
    }
}

#[test]
fn test_eval_if_else_expression() {
    struct Test {
        input: &'static str,
        expected: Object,
    }

    let if_else_tests = vec![
        Test {
            input: "if (true) { 10 }",
            expected: Object::IntegerObject(Box::new(Integer { value: 10 })),
        },
        Test {
            input: "if (false) { 10 }",
            expected: Object::Null,
        },
        Test {
            input: "if (1) { 10 }",
            expected: Object::IntegerObject(Box::new(Integer { value: 10 })),
        },
        Test {
            input: "if (1 < 2) { 10 }",
            expected: Object::IntegerObject(Box::new(Integer { value: 10 })),
        },
        Test {
            input: "if (1 > 2) { 10 }",
            expected: Object::Null,
        },
        Test {
            input: "if (1 > 2) { 10 } else { 20 }",
            expected: Object::IntegerObject(Box::new(Integer { value: 20 })),
        },
        Test {
            input: "if (1 < 2) { 10 } else { 20 }",
            expected: Object::IntegerObject(Box::new(Integer { value: 10 })),
        },
    ];

    for tt in if_else_tests {
        let evaluated = test_eval(tt.input);
        assert!(evaluated == tt.expected);
    }
}

#[test]
fn test_eval_return_statement() {
    struct Test {
        input: &'static str,
        expected: i64,
    }

    let return_tests = vec![
        Test {
            input: "return 10;",
            expected: 10,
        },
        Test {
            input: "return 10; 9;",
            expected: 10,
        },
        Test {
            input: "return 2 * 5; 9;",
            expected: 10,
        },
        Test {
            input: "9; return 2 * 5; 9;",
            expected: 10,
        },
        Test {
            input: r##"if (10 > 1) {
                         if (10 > 1) {
                           return 10;
                         }
                         return 1;
                      }"##,
            expected: 10,
        },
    ];

    for tt in return_tests {
        let evaluated = test_eval(tt.input);
        test_integer_object(evaluated, tt.expected);
    }
}

/// test error handling
#[test]
fn test_error_handling() {
    struct Test {
        input: &'static str,
        expected: &'static str,
    }

    let error_tests = vec![
        Test {
            input: "5 + true;",
            expected: "type mismatch: INTEGER + BOOLEAN",
        },
        Test {
            input: "5 + true; 5;",
            expected: "type mismatch: INTEGER + BOOLEAN",
        },
        Test {
            input: "-true",
            expected: "unknown operator: -BOOLEAN",
        },
        Test {
            input: "true + false;",
            expected: "unkown operator: BOOLEAN + BOOLEAN",
        },
        Test {
            input: "5; true + false; 5",
            expected: "unkown operator: BOOLEAN + BOOLEAN",
        },
        Test {
            input: "if (10 > 1) { true + false; }",
            expected: "unkown operator: BOOLEAN + BOOLEAN",
        },
        Test {
            input: "foobar",
            expected: "identifier not found: foobar",
        },
        Test {
            input: r##""Hello" - "World""##,
            expected: "unkown operator: STRING - STRING",
        },
    ];

    for tt in error_tests {
        let evaluated = test_eval(tt.input);
        if let Object::ErrorObject(eo) = evaluated {
            assert_eq!(eo.message, tt.expected);
        } else {
            panic!("no error object returned, got {:?}", evaluated);
        }
    }
}

/// test let statement
#[test]
fn test_let_statements() {
    struct Test {
        input: &'static str,
        expected: i64,
    }

    let let_statement_test = vec![
        Test {
            input: "let a = 5; a;",
            expected: 5,
        },
        Test {
            input: "let a = 5 * 5; a;",
            expected: 25,
        },
        Test {
            input: "let a = 5; let b = a;  b;",
            expected: 5,
        },
        Test {
            input: "let a = 5; let b = a;  let c = a + b + 5; c;",
            expected: 15,
        },
    ];

    for tt in let_statement_test {
        let evaluated = test_eval(tt.input);
        test_integer_object(evaluated, tt.expected);
    }
}

/// test function object
#[test]
fn test_function_object() {
    let input = "fn(x) {x + 2;};";

    let evaluated = test_eval(input);

    let f = match evaluated {
        Object::FunctionObject(f) => f,
        _ => panic!("function object is expected, got {:?}", evaluated),
    };

    assert!(
        f.parameters.len() == 1,
        "# of parameters should be 1, got {}",
        f.parameters.len()
    );

    assert!(
        f.parameters[0].string() == "x",
        "parameter should be 'x', got '{}'",
        f.parameters[0].string()
    );

    assert!(
        f.body.string() == "(x + 2)",
        r##"body should be "(x + 2)", got {}"##,
        f.body.string()
    );
}

/// test function application
#[test]
fn test_function_application() {
    struct Test {
        input: &'static str,
        expected: i64,
    }

    let function_app_tests = vec![
        Test {
            input: "let identify = fn(x) {x;}; identify(5);",
            expected: 5,
        },
        Test {
            input: "let identify = fn(x) {return x;}; identify(5);",
            expected: 5,
        },
        Test {
            input: "let double = fn(x) {return x * 2;}; double(5);",
            expected: 10,
        },
        Test {
            input: "let add = fn(x, y) {return x + y;}; add(5, 5);",
            expected: 10,
        },
        Test {
            input: "let add = fn(x, y) {return x + y;}; add(5 + 5, add(5, 5));",
            expected: 20,
        },
        Test {
            input: "fn(x) {x;}(5)",
            expected: 5,
        },
    ];

    for tt in function_app_tests {
        let evaluated = test_eval(tt.input);
        test_integer_object(evaluated, tt.expected);
    }
}

/// test closures
#[test]
fn test_closures() {
    let input = r##"let newAdder = fn(x) {
                      fn(y) { x + y; };
                    };

                    let addTwo = newAdder(2);
                    addTwo(2);"##;

    let evaluated = test_eval(input);
    test_integer_object(evaluated, 4);
}

/// eval function
fn test_eval(input: &'static str) -> Object {
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    let mut env = Environment::new();

    eval(
        &EvalNode::EvalStatementNode(Box::new(StatementNode::ProgramStatementNode(Box::new(
            program,
        )))),
        &mut env,
    )
}

/// test string literal
#[test]
fn test_string_literal() {
    let input = r##""Hello World!""##;

    let evaluated = test_eval(input);

    if let Object::StringObject(so) = evaluated {
        assert!(
            so.value == "Hello World!",
            r##""Hwllo World!" is expected, but got {}"##,
            so.value
        );
    } else {
        panic!(
            "unexpected object: {:?} (expected String Object)",
            evaluated
        );
    }
}

/// test string concatenation
#[test]
fn test_string_concatenation() {
    let input = r##""Hello" + " " + "World!""##;

    let evaluated = test_eval(input);

    if let Object::StringObject(so) = evaluated {
        assert!(
            so.value == "Hello World!",
            r##""Hwllo World!" is expected, but got {}"##,
            so.value
        );
    } else {
        panic!(
            "unexpected object: {:?} (expected String Object)",
            evaluated
        );
    }
}

/// test builtin functions
#[test]
fn test_builtin_functions() {
    struct Test {
        input: &'static str,
        expected: TestLiteral,
    }

    let builtin_function_tests = vec![
        Test {
            input: r##"len("")"##,
            expected: TestLiteral::IntegerLiteral { value: 0 },
        },
        Test {
            input: r##"len("four")"##,
            expected: TestLiteral::IntegerLiteral { value: 4 },
        },
        Test {
            input: r##"len("hello world")"##,
            expected: TestLiteral::IntegerLiteral { value: 11 },
        },
        Test {
            input: "len(1)",
            expected: TestLiteral::ErrorLiteral {
                message: "argument to `len` not supported, got INTEGER",
            },
        },
        Test {
            input: r##"len("one", "two")"##,
            expected: TestLiteral::ErrorLiteral {
                message: "wrong number of arguments. got=2, expected=1",
            },
        },
    ];

    for tt in builtin_function_tests {
        let evaluated = test_eval(tt.input);
        tt.expected.test_literal(&evaluated);
    }
}

/// test integer object
fn test_integer_object(obj: Object, expected: i64) {
    if let Object::IntegerObject(io) = obj {
        assert!(
            io.value == expected,
            "{} is expected, but got {}",
            expected,
            io.value
        );
    } else {
        panic!("unexpected object: {:?} (expected: {})", obj, expected);
    }
}

/// test boolean object
fn test_boolean_object(obj: Object, expected: bool) {
    if let Object::BooleanObject(bo) = obj {
        assert!(
            bo.value == expected,
            "{} is expected, but got {}",
            expected,
            bo.value
        );
    } else {
        panic!("unexpected object, got {:?} (expected: {})", obj, expected);
    }
}
