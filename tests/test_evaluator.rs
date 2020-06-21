use std::rc::Rc;

use rsmonkey::ast::StatementNode;
use rsmonkey::evaluator::eval;
use rsmonkey::evaluator::EvalNode;
use rsmonkey::lexer::Lexer;
use rsmonkey::object::{Environment, Object};
use rsmonkey::parser::Parser;

enum TestLiteral {
    IntegerLiteral { value: i64 },
    BooleanLiteral { value: bool },
    StringLiteral { value: &'static str },
    ErrorLiteral { message: &'static str },
    ArrayLiteral { array: Vec<TestLiteral> },
    NullLiteral,
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
            TestLiteral::BooleanLiteral { value: v } => {
                if let Object::BooleanObject(bo) = value {
                    assert_eq!(bo.value, *v);
                } else {
                    panic!("object is not boolean, got {:?}", value.object_type());
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
                    panic!("object is not error, got {:?}", value.object_type());
                }
            }
            TestLiteral::ArrayLiteral { array } => {
                if let Object::ArrayObject(al) = value {
                    assert!(
                        al.elements.len() == array.len(),
                        "the length of array is expected {}, got {}",
                        array.len(),
                        al.elements.len()
                    );
                    for (i, a) in array.iter().enumerate() {
                        a.test_literal(&al.elements[i]);
                    }
                } else {
                    panic!("object is not array literal, got {:?}", value);
                }
            }
            TestLiteral::NullLiteral => assert_eq!(*value, Object::Null),
        }
    }
}

#[test]
fn test_eval_integer_expression() {
    struct Test {
        input: &'static str,
        expected: TestLiteral,
    }

    let integer_expression_tests = vec![
        Test {
            input: "5",
            expected: TestLiteral::IntegerLiteral { value: 5 },
        },
        Test {
            input: "10",
            expected: TestLiteral::IntegerLiteral { value: 10 },
        },
        Test {
            input: "-5",
            expected: TestLiteral::IntegerLiteral { value: -5 },
        },
        Test {
            input: "-10",
            expected: TestLiteral::IntegerLiteral { value: -10 },
        },
        Test {
            input: "5 + 5 + 5 + 5 - 10",
            expected: TestLiteral::IntegerLiteral { value: 10 },
        },
        Test {
            input: "2 * 2 * 2 * 2 * 2",
            expected: TestLiteral::IntegerLiteral { value: 32 },
        },
        Test {
            input: "-50 + 100 + -50",
            expected: TestLiteral::IntegerLiteral { value: 0 },
        },
        Test {
            input: "5 * 2 + 10",
            expected: TestLiteral::IntegerLiteral { value: 20 },
        },
        Test {
            input: "5 + 2 * 10",
            expected: TestLiteral::IntegerLiteral { value: 25 },
        },
        Test {
            input: "20 + 2 * -10",
            expected: TestLiteral::IntegerLiteral { value: 0 },
        },
        Test {
            input: "50 / 2 * 2 + 10",
            expected: TestLiteral::IntegerLiteral { value: 60 },
        },
        Test {
            input: "2 * (5 + 10)",
            expected: TestLiteral::IntegerLiteral { value: 30 },
        },
        Test {
            input: "3 * 3 * 3 + 10",
            expected: TestLiteral::IntegerLiteral { value: 37 },
        },
        Test {
            input: "3 * (3 * 3) + 10",
            expected: TestLiteral::IntegerLiteral { value: 37 },
        },
        Test {
            input: "(5 + 10 * 2 + 15 / 3) * 2 + -10",
            expected: TestLiteral::IntegerLiteral { value: 50 },
        },
    ];

    for tt in integer_expression_tests {
        let evaluated = test_eval(tt.input);
        tt.expected.test_literal(&evaluated);
    }
}

#[test]
fn test_eval_boolean_expression() {
    struct Test {
        input: &'static str,
        expected: TestLiteral,
    }

    let boolean_expresssion_tests = vec![
        Test {
            input: "true",
            expected: TestLiteral::BooleanLiteral { value: true },
        },
        Test {
            input: "false",
            expected: TestLiteral::BooleanLiteral { value: false },
        },
        Test {
            input: "1 < 2",
            expected: TestLiteral::BooleanLiteral { value: true },
        },
        Test {
            input: "1 > 2",
            expected: TestLiteral::BooleanLiteral { value: false },
        },
        Test {
            input: "1 < 1",
            expected: TestLiteral::BooleanLiteral { value: false },
        },
        Test {
            input: "1 > 1",
            expected: TestLiteral::BooleanLiteral { value: false },
        },
        Test {
            input: "1 == 1",
            expected: TestLiteral::BooleanLiteral { value: true },
        },
        Test {
            input: "1 != 1",
            expected: TestLiteral::BooleanLiteral { value: false },
        },
        Test {
            input: "1 == 2",
            expected: TestLiteral::BooleanLiteral { value: false },
        },
        Test {
            input: "1 != 2",
            expected: TestLiteral::BooleanLiteral { value: true },
        },
        Test {
            input: "true == true",
            expected: TestLiteral::BooleanLiteral { value: true },
        },
        Test {
            input: "false == false",
            expected: TestLiteral::BooleanLiteral { value: true },
        },
        Test {
            input: "true != true",
            expected: TestLiteral::BooleanLiteral { value: false },
        },
        Test {
            input: "true == false",
            expected: TestLiteral::BooleanLiteral { value: false },
        },
        Test {
            input: "true != false",
            expected: TestLiteral::BooleanLiteral { value: true },
        },
        Test {
            input: "false != true",
            expected: TestLiteral::BooleanLiteral { value: true },
        },
        Test {
            input: "(1 < 2) == true",
            expected: TestLiteral::BooleanLiteral { value: true },
        },
        Test {
            input: "(1 < 2) == false",
            expected: TestLiteral::BooleanLiteral { value: false },
        },
        Test {
            input: "(1 > 2) == true",
            expected: TestLiteral::BooleanLiteral { value: false },
        },
        Test {
            input: "(1 > 2) == false",
            expected: TestLiteral::BooleanLiteral { value: true },
        },
        Test {
            input: r##""hoge" == "hoge""##,
            expected: TestLiteral::BooleanLiteral { value: true },
        },
        Test {
            input: r##""hoge" == "fuga""##,
            expected: TestLiteral::BooleanLiteral { value: false },
        },
        Test {
            input: r##""hoge" != "hoge""##,
            expected: TestLiteral::BooleanLiteral { value: false },
        },
        Test {
            input: r##""hoge" != "fuga""##,
            expected: TestLiteral::BooleanLiteral { value: true },
        },
    ];

    for tt in boolean_expresssion_tests {
        let evaluated = test_eval(tt.input);
        tt.expected.test_literal(&evaluated);
    }
}

#[test]
fn test_eval_bang_operator() {
    struct Test {
        input: &'static str,
        expected: TestLiteral,
    }

    let bang_tests = vec![
        Test {
            input: "!true",
            expected: TestLiteral::BooleanLiteral { value: false },
        },
        Test {
            input: "!false",
            expected: TestLiteral::BooleanLiteral { value: true },
        },
        Test {
            input: "!5",
            expected: TestLiteral::BooleanLiteral { value: false },
        },
        Test {
            input: "!!true",
            expected: TestLiteral::BooleanLiteral { value: true },
        },
        Test {
            input: "!!false",
            expected: TestLiteral::BooleanLiteral { value: false },
        },
        Test {
            input: "!!5",
            expected: TestLiteral::BooleanLiteral { value: true },
        },
    ];

    for tt in bang_tests {
        let evaluated = test_eval(tt.input);
        tt.expected.test_literal(&evaluated);
    }
}

#[test]
fn test_eval_if_else_expression() {
    struct Test {
        input: &'static str,
        expected: TestLiteral,
    }

    let if_else_tests = vec![
        Test {
            input: "if (true) { 10 }",
            expected: TestLiteral::IntegerLiteral { value: 10 },
        },
        Test {
            input: "if (false) { 10 }",
            expected: TestLiteral::NullLiteral,
        },
        Test {
            input: "if (1) { 10 }",
            expected: TestLiteral::IntegerLiteral { value: 10 },
        },
        Test {
            input: "if (1 < 2) { 10 }",
            expected: TestLiteral::IntegerLiteral { value: 10 },
        },
        Test {
            input: "if (1 > 2) { 10 }",
            expected: TestLiteral::NullLiteral,
        },
        Test {
            input: "if (1 > 2) { 10 } else { 20 }",
            expected: TestLiteral::IntegerLiteral { value: 20 },
        },
        Test {
            input: "if (1 < 2) { 10 } else { 20 }",
            expected: TestLiteral::IntegerLiteral { value: 10 },
        },
    ];

    for tt in if_else_tests {
        let evaluated = test_eval(tt.input);
        tt.expected.test_literal(&evaluated);
    }
}

#[test]
fn test_eval_return_statement() {
    struct Test {
        input: &'static str,
        expected: TestLiteral,
    }

    let return_tests = vec![
        Test {
            input: "return 10;",
            expected: TestLiteral::IntegerLiteral { value: 10 },
        },
        Test {
            input: "return 10; 9;",
            expected: TestLiteral::IntegerLiteral { value: 10 },
        },
        Test {
            input: "return 2 * 5; 9;",
            expected: TestLiteral::IntegerLiteral { value: 10 },
        },
        Test {
            input: "9; return 2 * 5; 9;",
            expected: TestLiteral::IntegerLiteral { value: 10 },
        },
        Test {
            input: r##"if (10 > 1) {
                         if (10 > 1) {
                           return 10;
                         }
                         return 1;
                      }"##,
            expected: TestLiteral::IntegerLiteral { value: 10 },
        },
    ];

    for tt in return_tests {
        let evaluated = test_eval(tt.input);
        tt.expected.test_literal(&evaluated);
    }
}

/// test error handling
#[test]
fn test_error_handling() {
    struct Test {
        input: &'static str,
        expected: TestLiteral,
    }

    let error_tests = vec![
        Test {
            input: "5 + true;",
            expected: TestLiteral::ErrorLiteral {
                message: "type mismatch: INTEGER + BOOLEAN",
            },
        },
        Test {
            input: "5 + true; 5;",
            expected: TestLiteral::ErrorLiteral {
                message: "type mismatch: INTEGER + BOOLEAN",
            },
        },
        Test {
            input: "-true",
            expected: TestLiteral::ErrorLiteral {
                message: "unknown operator: -BOOLEAN",
            },
        },
        Test {
            input: "true + false;",
            expected: TestLiteral::ErrorLiteral {
                message: "unkown operator: BOOLEAN + BOOLEAN",
            },
        },
        Test {
            input: "5; true + false; 5",
            expected: TestLiteral::ErrorLiteral {
                message: "unkown operator: BOOLEAN + BOOLEAN",
            },
        },
        Test {
            input: "if (10 > 1) { true + false; }",
            expected: TestLiteral::ErrorLiteral {
                message: "unkown operator: BOOLEAN + BOOLEAN",
            },
        },
        Test {
            input: "foobar",
            expected: TestLiteral::ErrorLiteral {
                message: "identifier not found: foobar",
            },
        },
        Test {
            input: r##""Hello" - "World""##,
            expected: TestLiteral::ErrorLiteral {
                message: "unkown operator: STRING - STRING",
            },
        },
    ];

    for tt in error_tests {
        let evaluated = test_eval(tt.input);
        tt.expected.test_literal(&evaluated);
    }
}

/// test let statement
#[test]
fn test_let_statements() {
    struct Test {
        input: &'static str,
        expected: TestLiteral,
    }

    let let_statement_test = vec![
        Test {
            input: "let a = 5; a;",
            expected: TestLiteral::IntegerLiteral { value: 5 },
        },
        Test {
            input: "let a = 5 * 5; a;",
            expected: TestLiteral::IntegerLiteral { value: 25 },
        },
        Test {
            input: "let a = 5; let b = a;  b;",
            expected: TestLiteral::IntegerLiteral { value: 5 },
        },
        Test {
            input: "let a = 5; let b = a;  let c = a + b + 5; c;",
            expected: TestLiteral::IntegerLiteral { value: 15 },
        },
    ];

    for tt in let_statement_test {
        let evaluated = test_eval(tt.input);
        tt.expected.test_literal(&evaluated);
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
        expected: TestLiteral,
    }

    let function_app_tests = vec![
        Test {
            input: "let identify = fn(x) {x;}; identify(5);",
            expected: TestLiteral::IntegerLiteral { value: 5 },
        },
        Test {
            input: "let identify = fn(x) {return x;}; identify(5);",
            expected: TestLiteral::IntegerLiteral { value: 5 },
        },
        Test {
            input: "let double = fn(x) {return x * 2;}; double(5);",
            expected: TestLiteral::IntegerLiteral { value: 10 },
        },
        Test {
            input: "let add = fn(x, y) {return x + y;}; add(5, 5);",
            expected: TestLiteral::IntegerLiteral { value: 10 },
        },
        Test {
            input: "let add = fn(x, y) {return x + y;}; add(5 + 5, add(5, 5));",
            expected: TestLiteral::IntegerLiteral { value: 20 },
        },
        Test {
            input: "fn(x) {x;}(5)",
            expected: TestLiteral::IntegerLiteral { value: 5 },
        },
        Test {
            input: "fn(x) {x;}(5, 4)",
            expected: TestLiteral::ErrorLiteral {
                message: "the # of arguments is wrong, expected 1, got 2",
            },
        },
    ];

    for tt in function_app_tests {
        let evaluated = test_eval(tt.input);
        tt.expected.test_literal(&evaluated);
    }
}

/// test closures
#[test]
fn test_closures() {
    let input = r##"
      let y = 5;
      let hoge = fn(x) {x + y};
      let y = 10;
      hoge(5);
    "##;

    let evaluated = test_eval(input);
    let expected = TestLiteral::IntegerLiteral { value: 15 };

    expected.test_literal(&evaluated);

    let input = r##"
      let newAdder = fn(x) {
         fn(y) { x + y; };
      };

      let addTwo = newAdder(2);
      addTwo(2);
    "##;

    let evaluated = test_eval(input);
    let expected = TestLiteral::IntegerLiteral { value: 4 };

    expected.test_literal(&evaluated);
}

/// test hash
#[test]
fn test_hash() {
    let input = r##"let two = "two";
{
  "one": 10 - 9,
  two: 1 + 1,
  "thr" + "ee": 6 / 2,
  4: 4,
  true: 5,
  false: 6
}"##;

    let evaluated = test_eval(input);

    let ho = match &evaluated {
        Object::HashObject(h) => h,
        _ => panic!("hash object is expected, got {:?}", evaluated),
    };

    assert_eq!(
        ho.pairs.len(),
        6,
        "the number of elements is expected 6, got {}",
        ho.pairs.len(),
    );

    (TestLiteral::StringLiteral { value: "one" }).test_literal(&ho.pairs[0].0);
    (TestLiteral::IntegerLiteral { value: 1 }).test_literal(&ho.pairs[0].1);
    (TestLiteral::StringLiteral { value: "two" }).test_literal(&ho.pairs[1].0);
    (TestLiteral::IntegerLiteral { value: 2 }).test_literal(&ho.pairs[1].1);
    (TestLiteral::StringLiteral { value: "three" }).test_literal(&ho.pairs[2].0);
    (TestLiteral::IntegerLiteral { value: 3 }).test_literal(&ho.pairs[2].1);
    (TestLiteral::IntegerLiteral { value: 4 }).test_literal(&ho.pairs[3].0);
    (TestLiteral::IntegerLiteral { value: 4 }).test_literal(&ho.pairs[3].1);
    (TestLiteral::BooleanLiteral { value: true }).test_literal(&ho.pairs[4].0);
    (TestLiteral::IntegerLiteral { value: 5 }).test_literal(&ho.pairs[4].1);
    (TestLiteral::BooleanLiteral { value: false }).test_literal(&ho.pairs[5].0);
    (TestLiteral::IntegerLiteral { value: 6 }).test_literal(&ho.pairs[5].1);
}

/// test hash index expression
#[test]
fn test_hash_index_expression() {
    struct Test {
        input: &'static str,
        expected: TestLiteral,
    }

    let hash_index_tests = vec![
        Test {
            input: r##"{"foo": 5}["foo"]"##,
            expected: TestLiteral::IntegerLiteral { value: 5 },
        },
        Test {
            input: r##"{"foo": 5}["bar"]"##,
            expected: TestLiteral::NullLiteral,
        },
        Test {
            input: r##"let key = "foo"; {"foo": 5}[key]"##,
            expected: TestLiteral::IntegerLiteral { value: 5 },
        },
        Test {
            input: r##"{}["foo"]"##,
            expected: TestLiteral::NullLiteral,
        },
        Test {
            input: "{5: 5}[5]",
            expected: TestLiteral::IntegerLiteral { value: 5 },
        },
        Test {
            input: "{true: 5}[true]",
            expected: TestLiteral::IntegerLiteral { value: 5 },
        },
        Test {
            input: "{false: 5}[false]",
            expected: TestLiteral::IntegerLiteral { value: 5 },
        },
        Test {
            input: r##"{false: 5, 1: 3, "false": 2}[false]"##,
            expected: TestLiteral::IntegerLiteral { value: 5 },
        },
        Test {
            input: r##"{false: 5, 1: 3, "false": 2}["false"]"##,
            expected: TestLiteral::IntegerLiteral { value: 2 },
        },
    ];

    for tt in hash_index_tests {
        let evaluated = test_eval(tt.input);
        tt.expected.test_literal(&evaluated);
    }
}

/// test arrays
#[test]
fn test_array_literals() {
    let input = r##"[1, 2 * 2, 3 + 3]"##;

    let evaluated = test_eval(input);

    let ao = match &evaluated {
        Object::ArrayObject(a) => a,
        _ => panic!("array object is expected, got {:?}", evaluated),
    };

    assert!(
        ao.elements.len() == 3,
        "the # of elements is expected 3, got {}",
        ao.elements.len()
    );

    (TestLiteral::IntegerLiteral { value: 1 }).test_literal(&ao.elements[0]);
    (TestLiteral::IntegerLiteral { value: 4 }).test_literal(&ao.elements[1]);
    (TestLiteral::IntegerLiteral { value: 6 }).test_literal(&ao.elements[2]);
}

/// test index expressions
#[test]
fn test_array_index_expressions() {
    struct Test {
        input: &'static str,
        expected: TestLiteral,
    }

    let index_expressions_tests = vec![
        Test {
            input: "[1, 2, 3][0]",
            expected: TestLiteral::IntegerLiteral { value: 1 },
        },
        Test {
            input: "[1, 2, 3][1]",
            expected: TestLiteral::IntegerLiteral { value: 2 },
        },
        Test {
            input: "[1, 2, 3][2]",
            expected: TestLiteral::IntegerLiteral { value: 3 },
        },
        Test {
            input: "[1, 2, 3][1 + 1]",
            expected: TestLiteral::IntegerLiteral { value: 3 },
        },
        Test {
            input: "let myArray = [1, 2, 3]; myArray[2]",
            expected: TestLiteral::IntegerLiteral { value: 3 },
        },
        Test {
            input: "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
            expected: TestLiteral::IntegerLiteral { value: 6 },
        },
        Test {
            input: "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i]",
            expected: TestLiteral::IntegerLiteral { value: 2 },
        },
        Test {
            input: "[1, 2, 3][3]",
            expected: TestLiteral::NullLiteral,
        },
        Test {
            input: "[1, 2, 3][-1]",
            expected: TestLiteral::NullLiteral,
        },
    ];

    for tt in index_expressions_tests {
        let evaluated = test_eval(tt.input);
        tt.expected.test_literal(&evaluated);
    }
}

/// eval function
fn test_eval(input: &'static str) -> Object {
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program().unwrap();
    let env = Rc::new(Environment::new());

    eval(
        &EvalNode::EvalStatementNode(Box::new(StatementNode::ProgramStatementNode(Box::new(
            program,
        )))),
        env,
    )
}

/// test string literal
#[test]
fn test_string_literal() {
    let input = r##""Hello World!""##;
    let evaluated = test_eval(input);

    TestLiteral::StringLiteral {
        value: "Hello World!",
    }
    .test_literal(&evaluated);
}

/// test string concatenation
#[test]
fn test_string_concatenation() {
    let input = r##""Hello" + " " + "World!""##;
    let evaluated = test_eval(input);

    TestLiteral::StringLiteral {
        value: "Hello World!",
    }
    .test_literal(&evaluated);
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
        Test {
            input: r##"len([1, 2, 3])"##,
            expected: TestLiteral::IntegerLiteral { value: 3 },
        },
        Test {
            input: r##"len([1, 2, 3, 4, 5, 6, 7])"##,
            expected: TestLiteral::IntegerLiteral { value: 7 },
        },
        Test {
            input: r##"let myArray = [1, 2];
                       len(myArray);"##,
            expected: TestLiteral::IntegerLiteral { value: 2 },
        },
        Test {
            input: r##"let myArray = [1, 2, 3, 4, 5, 6, 7];
                       first(myArray);"##,
            expected: TestLiteral::IntegerLiteral { value: 1 },
        },
        Test {
            input: r##"first(1);"##,
            expected: TestLiteral::ErrorLiteral {
                message: "argument to `first` must be ARRAY, got INTEGER",
            },
        },
        Test {
            input: r##"let myArray = [1, 2, 3, 4, 5, 6, 7];
                       last(myArray);"##,
            expected: TestLiteral::IntegerLiteral { value: 7 },
        },
        Test {
            input: r##"last(1);"##,
            expected: TestLiteral::ErrorLiteral {
                message: "argument to `last` must be ARRAY, got INTEGER",
            },
        },
        Test {
            input: r##"rest([1, 2, 3, 4]);"##,
            expected: TestLiteral::ArrayLiteral {
                array: vec![
                    TestLiteral::IntegerLiteral { value: 2 },
                    TestLiteral::IntegerLiteral { value: 3 },
                    TestLiteral::IntegerLiteral { value: 4 },
                ],
            },
        },
        Test {
            input: r##"rest(rest([1, 2, 3, 4]));"##,
            expected: TestLiteral::ArrayLiteral {
                array: vec![
                    TestLiteral::IntegerLiteral { value: 3 },
                    TestLiteral::IntegerLiteral { value: 4 },
                ],
            },
        },
        Test {
            input: r##"rest(rest(rest([1, 2, 3, 4])));"##,
            expected: TestLiteral::ArrayLiteral {
                array: vec![TestLiteral::IntegerLiteral { value: 4 }],
            },
        },
        Test {
            input: r##"rest(rest(rest(rest([1, 2, 3, 4]))));"##,
            expected: TestLiteral::ArrayLiteral { array: vec![] },
        },
        Test {
            input: r##"rest(rest(rest(rest(rest([1, 2, 3, 4])))));"##,
            expected: TestLiteral::NullLiteral,
        },
        Test {
            input: r##"let a = [1, 2]; push(a, 3);"##,
            expected: TestLiteral::ArrayLiteral {
                array: vec![
                    TestLiteral::IntegerLiteral { value: 1 },
                    TestLiteral::IntegerLiteral { value: 2 },
                    TestLiteral::IntegerLiteral { value: 3 },
                ],
            },
        },
        Test {
            input: r##"let map = fn(arr, f) {
                         let iter = fn(arr, accumulated) {
                           if (len(arr) == 0) {
                             accumulated
                          } else {
                            iter(rest(arr), push(accumulated, f(first(arr))));
                          }
                        };
                        iter(arr, []);
                      };
                      let a = [1, 2, 3, 4];
                      let double = fn(x) { x * 2 };
                      map(a, double);"##,
            expected: TestLiteral::ArrayLiteral {
                array: vec![
                    TestLiteral::IntegerLiteral { value: 2 },
                    TestLiteral::IntegerLiteral { value: 4 },
                    TestLiteral::IntegerLiteral { value: 6 },
                    TestLiteral::IntegerLiteral { value: 8 },
                ],
            },
        },
        Test {
            input: r##"let reduce = fn(arr, initial, f) {
                         let iter = fn(arr, result) {
                           if (len(arr) == 0) {
                             result
                           } else {
                             iter(rest(arr), f(result, first(arr)));
                           }
                         };
                         iter(arr, initial);
                       };
                       let sum = fn(arr) {
                         reduce(arr, 0, fn(initial, el) { initial + el });
                       };
                       sum([1, 2, 3, 4, 5]);"##,
            expected: TestLiteral::IntegerLiteral { value: 15 },
        },
    ];

    for tt in builtin_function_tests {
        let evaluated = test_eval(tt.input);
        tt.expected.test_literal(&evaluated);
    }
}
