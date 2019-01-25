use rsmonkey::ast::{ExpressionNode, StatementNode};
use rsmonkey::evaluator::eval;
use rsmonkey::evaluator::EvalNode;
use rsmonkey::lexer::Lexer;
use rsmonkey::object::Object;
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
    ];

    for tt in integer_expression_tests {
        let evaluated = test_eval(tt.input);
        test_integer_boject(evaluated, tt.expected);
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
    ];

    for tt in boolean_expresssion_tests {
        let evaluated = test_eval(tt.input);
        test_boolean_boject(evaluated, tt.expected);
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
        test_boolean_boject(evaluated, tt.expected);
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
fn test_integer_boject(obj: Object, expected: i64) {
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
fn test_boolean_boject(obj: Object, expected: bool) {
    if let Object::BooleanObject(bo) = obj {
        assert!(
            bo.value == expected,
            "{} is expected, but got {}",
            expected,
            bo.value
        );
    } else {
        panic!("unexpected object");
    }
}
