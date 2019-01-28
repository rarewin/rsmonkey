use rsmonkey::ast::{ExpressionNode, StatementNode};
use rsmonkey::evaluator::eval;
use rsmonkey::evaluator::EvalNode;
use rsmonkey::lexer::Lexer;
use rsmonkey::object::{Integer, Object};
use rsmonkey::parser::Parser;

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
            expected: Object::IntegerObject(Integer { value: 10 }),
        },
        Test {
            input: "if (false) { 10 }",
            expected: Object::Null,
        },
        Test {
            input: "if (1) { 10 }",
            expected: Object::IntegerObject(Integer { value: 10 }),
        },
        Test {
            input: "if (1 < 2) { 10 }",
            expected: Object::IntegerObject(Integer { value: 10 }),
        },
        Test {
            input: "if (1 > 2) { 10 }",
            expected: Object::Null,
        },
        Test {
            input: "if (1 > 2) { 10 } else { 20 }",
            expected: Object::IntegerObject(Integer { value: 20 }),
        },
        Test {
            input: "if (1 < 2) { 10 } else { 20 }",
            expected: Object::IntegerObject(Integer { value: 10 }),
        },
    ];

    for tt in if_else_tests {
        let evaluated = test_eval(tt.input);
        assert!(evaluated == tt.expected);
    }
}

/// eval function
fn test_eval(input: &'static str) -> Object {
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();

    eval(&EvalNode::EvalStatementNode(Box::new(
        StatementNode::ProgramStatementNode(Box::new(program)),
    )))
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
        panic!("unexpected object");
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
        panic!("unexpected object, got {:?}", obj);
    }
}
