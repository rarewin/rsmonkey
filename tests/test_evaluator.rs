use std::cell::RefCell;
use std::rc::Rc;

use rsmonkey::evaluator::{eval, EvaluationError};
use rsmonkey::lexer::Lexer;
use rsmonkey::object::{Environment, Object};
use rsmonkey::parser::Parser;

#[derive(Debug)]
enum TestLiteral {
    IntegerLiteral { value: i64 },
    BooleanLiteral { value: bool },
    StringLiteral { value: &'static str },
    ErrorLiteral { message: &'static str },
    ArrayLiteral { array: Vec<Object> },
    HashLiteral { pairs: Vec<(Object, Object)> },
    NullLiteral,
}

impl TestLiteral {
    fn test_literal(&self, value: &Result<Object, EvaluationError>) {
        match self {
            TestLiteral::IntegerLiteral { value: v } => {
                if let Ok(Object::Integer(value)) = value {
                    assert_eq!(value, v);
                } else {
                    panic!(
                        "object is not integer, got {:?}, expected {:?}",
                        value.as_ref().unwrap().object_type(),
                        v
                    );
                }
            }
            TestLiteral::BooleanLiteral { value: v } => {
                if let Ok(Object::Boolean(b)) = value {
                    assert_eq!(b, v);
                } else {
                    panic!(
                        "object is not boolean, got {:?}",
                        value.as_ref().unwrap().object_type()
                    );
                }
            }
            TestLiteral::NullLiteral => {
                assert_eq!(value.as_ref().unwrap(), &Object::Null);
            }
            TestLiteral::ErrorLiteral { message: m } => {
                assert_eq!(format!("{}", value.as_ref().unwrap_err()), m.to_string());
            }
            TestLiteral::StringLiteral { value: s } => {
                if let Object::String(so) = value.as_ref().unwrap() {
                    assert_eq!(so, s);
                } else {
                    panic!(
                        "object is not string, got {:?}",
                        value.as_ref().unwrap().object_type()
                    );
                }
            }
            TestLiteral::ArrayLiteral { array } => {
                if let Object::Array(al) = value.as_ref().unwrap() {
                    assert!(
                        al.len() == array.len(),
                        "the length of array is expected {}, got {}",
                        array.len(),
                        al.len()
                    );
                    for (i, a) in array.iter().enumerate() {
                        assert_eq!(&al[i], a);
                    }
                } else {
                    panic!("object is not array literal, got {:?}", value);
                }
            }
            TestLiteral::HashLiteral { pairs: p } => {
                if let Ok(Object::Hash(ho)) = value {
                    assert_eq!(ho.len(), p.len());

                    for (i, h) in p.iter().enumerate() {
                        assert_eq!(&ho[i], h);
                    }
                } else {
                    panic!(
                        "object is not hash, got {:?}",
                        value.as_ref().unwrap().object_type()
                    );
                }
            }
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
                message: "unknown operator: BOOLEAN + BOOLEAN",
            },
        },
        Test {
            input: "5; true + false; 5",
            expected: TestLiteral::ErrorLiteral {
                message: "unknown operator: BOOLEAN + BOOLEAN",
            },
        },
        Test {
            input: "if (10 > 1) { true + false; }",
            expected: TestLiteral::ErrorLiteral {
                message: "unknown operator: BOOLEAN + BOOLEAN",
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
                message: "unknown operator: STRING - STRING",
            },
        },
    ];

    for tt in error_tests {
        let evaluated = dbg!(test_eval(tt.input));
        tt.expected.test_literal(&evaluated);
    }
}

/// test let statement
#[test]
fn test_eval_let_statements() {
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

    let (parameters, body) = if let Ok(Object::Function {
        parameters,
        body,
        env: _,
    }) = evaluated
    {
        (parameters, body)
    } else {
        panic!("function object is expected, got {:?}", evaluated)
    };

    assert!(
        parameters.len() == 1,
        "# of parameters should be 1, got {}",
        parameters.len()
    );

    assert_eq!(
        String::from(&parameters[0]),
        "x",
        "parameter should be 'x', got '{}'",
        String::from(&parameters[0])
    );

    assert!(
        String::from(&body) == "(x + 2)",
        r##"body should be "(x + 2)", got {}"##,
        String::from(&body)
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
        Ok(Object::Hash(h)) => h,
        _ => panic!("hash object is expected, got {:?}", evaluated),
    };

    assert_eq!(
        ho.len(),
        6,
        "the number of elements is expected 6, got {}",
        ho.len(),
    );

    let expected = TestLiteral::HashLiteral {
        pairs: vec![
            (Object::from("one"), Object::from(1)),
            (Object::from("two"), Object::from(2)),
            (Object::from("three"), Object::from(3)),
            (Object::from(4), Object::from(4)),
            (Object::from(true), Object::from(5)),
            (Object::from(false), Object::from(6)),
        ],
    };

    expected.test_literal(&evaluated);
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
        Ok(Object::Array(a)) => a,
        _ => panic!("array object is expected, got {:?}", evaluated),
    };

    assert!(
        ao.len() == 3,
        "the # of elements is expected 3, got {}",
        ao.len()
    );

    TestLiteral::ArrayLiteral {
        array: vec![Object::from(1), Object::from(4), Object::from(6)],
    }
    .test_literal(&evaluated);
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
fn test_eval(input: &'static str) -> Result<Object, EvaluationError> {
    let l = Lexer::new(input);
    let p = Parser::new(l);

    let env = Rc::new(RefCell::new(Environment::new()));
    eval(p, env)
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
                message: "first argument to `last` must be ARRAY, got INTEGER",
            },
        },
        Test {
            input: r##"rest([1, 2, 3, 4]);"##,
            expected: TestLiteral::ArrayLiteral {
                array: vec![Object::from(2), Object::from(3), Object::from(4)],
            },
        },
        Test {
            input: r##"rest(rest([1, 2, 3, 4]));"##,
            expected: TestLiteral::ArrayLiteral {
                array: vec![Object::from(3), Object::from(4)],
            },
        },
        Test {
            input: r##"rest(rest(rest([1, 2, 3, 4])));"##,
            expected: TestLiteral::ArrayLiteral {
                array: vec![Object::from(4)],
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
                array: vec![Object::from(1), Object::from(2), Object::from(3)],
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
                    Object::from(2),
                    Object::from(4),
                    Object::from(6),
                    Object::from(8),
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
