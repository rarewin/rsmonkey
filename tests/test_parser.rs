use rsmonkey::ast::*;
use rsmonkey::lexer::Lexer;
use rsmonkey::parser::{ParseError, Parser};
use rsmonkey::token::Token;

enum TestLiteral {
    IntegerLiteral(i64),
    IdentifierLiteral(&'static str),
    StringLiteral(&'static str),
    BooleanLiteral(bool),
}
#[test]
fn test_let_statements() -> Result<(), ParseError> {
    struct Test {
        input: &'static str,
        expected_identifier: &'static str,
        expected_value: TestLiteral,
    }
    let let_statement_test = vec![
        Test {
            input: "let x = 5;",
            expected_identifier: "x",
            expected_value: TestLiteral::IntegerLiteral(5),
        },
        Test {
            input: "let y = true;",
            expected_identifier: "y",
            expected_value: TestLiteral::BooleanLiteral(true),
        },
        Test {
            input: "let foobar = y;",
            expected_identifier: "foobar",
            expected_value: TestLiteral::IdentifierLiteral("y"),
        },
    ];

    for tt in let_statement_test {
        let l = Lexer::new(tt.input);
        let mut p = Parser::new(l).into_iter();

        let program = p.next().unwrap()?;

        test_let_statement(&program, tt.expected_identifier, &tt.expected_value);
        assert!(p.next().is_none());
    }

    Ok(())
}

/// test let statement.
/// get panicked if the test is failed.
///
/// # Arguments
///
/// * `s` - let statement
/// * `expected_name` - expected string for identifer of `s`
/// * `expected_value` - expected value for right value
/// ```
fn test_let_statement(s: &StatementNode, expected_name: &str, expected_value: &TestLiteral) {
    if let StatementNode::LetStatement {
        token: _,
        name,
        value,
    } = s
    {
        assert_eq!(
            Token::Ident(format!("{}", name)),
            Token::Ident(expected_name.to_string()),
            "expected identifier is '{}', but got '{:?}'.",
            expected_name,
            name,
        );
        assert_eq!(
            String::from(name),
            expected_name,
            "expected identifier is '{}', but got '{}'.",
            expected_name,
            name
        );
        test_literal_expression(&value, expected_value);
    } else {
        panic!();
    }
}

#[test]
fn test_return_statements() -> Result<(), ParseError> {
    struct Test {
        input: &'static str,
        expected_value: TestLiteral,
    }

    let return_statement_test = vec![
        Test {
            input: "return 5;",
            expected_value: TestLiteral::IntegerLiteral(5),
        },
        Test {
            input: "return 10;",
            expected_value: TestLiteral::IntegerLiteral(10),
        },
        Test {
            input: "return 993322;",
            expected_value: TestLiteral::IntegerLiteral(993322),
        },
    ];

    for tt in return_statement_test {
        let l = Lexer::new(tt.input);
        let mut p = Parser::new(l).into_iter();

        let program = p.next().unwrap()?;

        if let StatementNode::ReturnStatement {
            token: _,
            return_value,
        } = program
        {
            test_literal_expression(&return_value, &tt.expected_value);
        } else {
            panic!("return statement is expected");
        }

        assert!(p.next().is_none());
    }

    Ok(())
}

#[test]
fn test_string() {
    let v = StatementNode::LetStatement {
        token: Token::Let,
        name: Token::Ident("myVar".into()),
        value: ExpressionNode::Identifier {
            token: Token::Ident("anotherVar".into()),
        },
    };

    assert_eq!(String::from(&v), "let myVar = anotherVar;");
}

#[test]
fn test_identifier_expression() -> Result<(), ParseError> {
    let input = r##"foobar;"##;

    let l = Lexer::new(input);
    let mut p = Parser::new(l).into_iter();

    let program = p.next().unwrap()?;

    let stmt_token = match program {
        StatementNode::ExpressionStatement {
            token,
            expression: _,
        } => token,
        _ => panic!("first statement is not expressionstatement"),
    };

    assert_eq!(String::from(stmt_token), "foobar");
    assert!(p.next().is_none());

    Ok(())
}

#[test]
fn test_integer_literal_expression() -> Result<(), ParseError> {
    let input = r##"5;"##;

    let l = Lexer::new(input);
    let mut p = Parser::new(l).into_iter();

    let program = p.next().unwrap()?;

    assert_eq!(format!("{}", program), "5");
    assert!(p.next().is_none());

    Ok(())
}

/// test string literal
#[test]
fn test_string_literal_expression() -> Result<(), ParseError> {
    let input = r##""hello world""##;

    let l = Lexer::new(input);
    let mut p = Parser::new(l).into_iter();

    let program = p.next().unwrap()?;

    assert_eq!(format!("{}", program), "hello world");
    assert!(p.next().is_none());

    Ok(())
}

/// test boolean
#[test]
fn test_boolean_literal_expression() -> Result<(), ParseError> {
    struct Test {
        input: &'static str,
        value: bool,
    }

    let boolean_test = vec![
        Test {
            input: "true",
            value: true,
        },
        Test {
            input: "false",
            value: false,
        },
    ];

    for tt in boolean_test {
        let l = Lexer::new(tt.input);
        let mut p = Parser::new(l).into_iter();

        let program = p.next().unwrap()?;

        let stmt_exp = match &program {
            StatementNode::ExpressionStatement {
                token: _,
                expression,
            } => expression,
            _ => panic!("first statement is not expression statement"),
        };

        let exp_value = match &stmt_exp {
            ExpressionNode::Boolean { token: _, value } => value,
            _ => panic!("this expression statement does not have boolean expression"),
        };

        assert_eq!(
            *exp_value, tt.value,
            "{} is expected, but got {}",
            tt.value, exp_value,
        );

        assert!(p.next().is_none());
    }

    Ok(())
}

#[test]
fn test_parsing_prefix_expressions() -> Result<(), ParseError> {
    struct Test {
        input: &'static str,
        value: TestLiteral,
    }

    let prefix_test = vec![
        Test {
            input: "!5;",
            value: TestLiteral::IntegerLiteral(5),
        },
        Test {
            input: "-15;",
            value: TestLiteral::IntegerLiteral(15),
        },
        Test {
            input: "!true;",
            value: TestLiteral::BooleanLiteral(true),
        },
        Test {
            input: "!false;",
            value: TestLiteral::BooleanLiteral(false),
        },
    ];

    for tt in prefix_test {
        let l = Lexer::new(tt.input);
        let mut p = Parser::new(l).into_iter();

        let program = p.next().unwrap()?;

        let stmt_exp = match &program {
            StatementNode::ExpressionStatement {
                token: _,
                expression,
            } => expression,
            _ => panic!("first statement is not expression statement"),
        };

        let exp = match &stmt_exp {
            ExpressionNode::PrefixExpression { token: _, right } => right,
            _ => panic!("this expression statement does not have prefix expression"),
        };

        test_literal_expression(&exp, &tt.value);

        assert!(p.next().is_none());
    }

    Ok(())
}

#[test]
fn test_parsing_infix_expressions() -> Result<(), ParseError> {
    struct Test {
        input: &'static str,
        left_value: TestLiteral,
        operator: &'static str,
        right_value: TestLiteral,
    }

    let infix_test = vec![
        Test {
            input: "5 + 5;",
            left_value: TestLiteral::IntegerLiteral(5),
            operator: "+",
            right_value: TestLiteral::IntegerLiteral(5),
        },
        Test {
            input: "5 - 5;",
            left_value: TestLiteral::IntegerLiteral(5),
            operator: "-",
            right_value: TestLiteral::IntegerLiteral(5),
        },
        Test {
            input: "5 * 4;",
            left_value: TestLiteral::IntegerLiteral(5),
            operator: "*",
            right_value: TestLiteral::IntegerLiteral(4),
        },
        Test {
            input: "8 / 4;",
            left_value: TestLiteral::IntegerLiteral(8),
            operator: "/",
            right_value: TestLiteral::IntegerLiteral(4),
        },
        Test {
            input: "2 > 8;",
            left_value: TestLiteral::IntegerLiteral(2),
            operator: ">",
            right_value: TestLiteral::IntegerLiteral(8),
        },
        Test {
            input: "8 < 2;",
            left_value: TestLiteral::IntegerLiteral(8),
            operator: "<",
            right_value: TestLiteral::IntegerLiteral(2),
        },
        Test {
            input: "2 == 2;",
            left_value: TestLiteral::IntegerLiteral(2),
            operator: "==",
            right_value: TestLiteral::IntegerLiteral(2),
        },
        Test {
            input: "3 != 2;",
            left_value: TestLiteral::IntegerLiteral(3),
            operator: "!=",
            right_value: TestLiteral::IntegerLiteral(2),
        },
        Test {
            input: "true == true;",
            left_value: TestLiteral::BooleanLiteral(true),
            operator: "==",
            right_value: TestLiteral::BooleanLiteral(true),
        },
        Test {
            input: "true != false;",
            left_value: TestLiteral::BooleanLiteral(true),
            operator: "!=",
            right_value: TestLiteral::BooleanLiteral(false),
        },
        Test {
            input: "false == false;",
            left_value: TestLiteral::BooleanLiteral(false),
            operator: "==",
            right_value: TestLiteral::BooleanLiteral(false),
        },
    ];

    for tt in infix_test {
        let l = Lexer::new(tt.input);
        let mut p = Parser::new(l).into_iter();

        let program = p.next().unwrap()?;

        let stmt_exp = match &program {
            StatementNode::ExpressionStatement {
                token: _,
                expression,
            } => expression,
            _ => panic!("first statement is not expression statement"),
        };

        test_infix_expression(&stmt_exp, &tt.left_value, tt.operator, &tt.right_value);
        assert!(p.next().is_none());
    }

    Ok(())
}

#[test]
fn test_operator_precedence_parsing() -> Result<(), ParseError> {
    struct Test {
        input: &'static str,
        expected: &'static str,
    }

    let operator_precedence_test = vec![
        Test {
            input: "-a * b;",
            expected: "((-a) * b)",
        },
        Test {
            input: "!-a",
            expected: "(!(-a))",
        },
        Test {
            input: "a + b + c",
            expected: "((a + b) + c)",
        },
        Test {
            input: "a + b - c",
            expected: "((a + b) - c)",
        },
        Test {
            input: "a * b * c",
            expected: "((a * b) * c)",
        },
        Test {
            input: "a * b / c",
            expected: "((a * b) / c)",
        },
        Test {
            input: "a + b * c + d / e - f",
            expected: "(((a + (b * c)) + (d / e)) - f)",
        },
        Test {
            input: "3 + 4; -5 * 5;",
            expected: "(3 + 4)((-5) * 5)",
        },
        Test {
            input: "5 > 4 == 3 < 4",
            expected: "((5 > 4) == (3 < 4))",
        },
        Test {
            input: "5 < 4 != 3 > 4",
            expected: "((5 < 4) != (3 > 4))",
        },
        Test {
            input: "3 + 4 * 5 == 3 * 1 + 4 * 5",
            expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        },
        Test {
            input: "true",
            expected: "true",
        },
        Test {
            input: "false",
            expected: "false",
        },
        Test {
            input: "3 > 5 == false",
            expected: "((3 > 5) == false)",
        },
        Test {
            input: "3 < 5 == true",
            expected: "((3 < 5) == true)",
        },
        Test {
            input: "1 + (2 + 3) + 4;",
            expected: "((1 + (2 + 3)) + 4)",
        },
        Test {
            input: "(5 + 5) * 2;",
            expected: "((5 + 5) * 2)",
        },
        Test {
            input: "2 / (5 + 5);",
            expected: "(2 / (5 + 5))",
        },
        Test {
            input: "-(5 + 5);",
            expected: "(-(5 + 5))",
        },
        Test {
            input: "!(true == true);",
            expected: "(!(true == true))",
        },
        Test {
            input: "a + add(b * c) + d;",
            expected: "((a + add((b * c))) + d)",
        },
        Test {
            input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));",
            expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
        },
        Test {
            input: "add(a + b + c * d / f + g);",
            expected: "add((((a + b) + ((c * d) / f)) + g))",
        },
        Test {
            input: "a * [1, 2, 3, 4][b * c] * d",
            expected: "((a * ([1, 2, 3, 4][(b * c)])) * d)",
        },
        Test {
            input: "add(a * b[2], b[1], 2 * [1, 2][1])",
            expected: "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
        },
    ];

    for tt in operator_precedence_test {
        let l = Lexer::new(tt.input);
        let p = Parser::new(l).into_iter();

        let program_str = p.map(|c| String::from(&c.unwrap())).collect::<String>();

        assert_eq!(
            program_str, tt.expected,
            r##"expected "{}", got "{}""##,
            tt.expected, program_str,
        );
    }

    Ok(())
}

#[test]
fn test_if_expression() -> Result<(), ParseError> {
    let input = "if (x < y) { x };";

    let l = Lexer::new(input);
    let mut p = Parser::new(l).into_iter();

    let program = p.next().unwrap()?;

    let exps = match &program {
        StatementNode::ExpressionStatement {
            token: _,
            expression,
        } => expression,
        _ => panic!("expression stateme is expected"),
    };

    let (condition, consequence, alternative) = match &exps {
        ExpressionNode::IfExpression {
            token: _,
            condition,
            consequence,
            alternative,
        } => (condition, consequence, alternative),
        _ => panic!("if expression is expected"),
    };

    test_infix_expression(
        condition,
        &TestLiteral::IdentifierLiteral("x"),
        "<",
        &TestLiteral::IdentifierLiteral("y"),
    );

    let cs_stmt = match consequence.as_ref().unwrap().as_ref() {
        StatementNode::BlockStatement {
            token: _,
            statements,
        } => statements,
        _ => panic!("consequence does not have block statement"),
    };

    assert_eq!(
        cs_stmt.len(),
        1,
        "consequence does not have the expected number of statements.",
    );

    let conex = match &cs_stmt[0] {
        StatementNode::ExpressionStatement {
            token: _,
            expression,
        } => expression,
        _ => panic!("expression statement is expected"),
    };

    test_literal_expression(&conex, &TestLiteral::IdentifierLiteral("x"));

    match alternative {
        None => {}
        _ => panic!("alternative has unexpected statement node"),
    };

    assert!(p.next().is_none());

    Ok(())
}

#[test]
fn test_if_else_expression() -> Result<(), ParseError> {
    let input = "if (x < y) { x; } else { y };";

    let l = Lexer::new(input);
    let mut p = Parser::new(l).into_iter();

    let stmt = p.next().unwrap()?;

    let exps = match &stmt {
        StatementNode::ExpressionStatement {
            token: _,
            expression,
        } => expression,
        _ => panic!("expression stateme is expected"),
    };

    let (condition, consequence, alternative) = match &exps {
        ExpressionNode::IfExpression {
            token: _,
            condition,
            consequence,
            alternative,
        } => (condition, consequence, alternative),
        _ => panic!("if expression is expected"),
    };

    test_infix_expression(
        condition,
        &TestLiteral::IdentifierLiteral("x"),
        "<",
        &TestLiteral::IdentifierLiteral("y"),
    );

    let cs_stmt = match consequence.as_ref().unwrap().as_ref() {
        StatementNode::BlockStatement {
            token: _,
            statements,
        } => statements,
        _ => panic!("consequence does not have block statement"),
    };

    assert!(
        cs_stmt.len() == 1,
        "consequence does not have the expected number of statements. {}",
        cs_stmt.len()
    );

    let conex = match &cs_stmt[0] {
        StatementNode::ExpressionStatement {
            token: _,
            expression,
        } => expression,
        _ => panic!("expression statement is expected"),
    };

    test_literal_expression(&conex, &TestLiteral::IdentifierLiteral("x"));

    let als_stmt = match alternative.as_ref().unwrap().as_ref() {
        StatementNode::BlockStatement {
            token: _,
            statements,
        } => statements,
        _ => panic!("alternative does not have block statement"),
    };

    assert!(
        als_stmt.len() == 1,
        "consequence does not have the expected number of statements. {}",
        als_stmt.len()
    );

    let alsex = match &als_stmt[0] {
        StatementNode::ExpressionStatement {
            token: _,
            expression,
        } => expression,
        _ => panic!("expression statement is expected"),
    };

    test_literal_expression(&alsex, &TestLiteral::IdentifierLiteral("y"));

    assert_eq!(String::from(&stmt), "if (x < y) x else y");
    assert!(p.next().is_none());

    Ok(())
}

#[test]
fn test_function_literal_parsing() -> Result<(), ParseError> {
    let input = "fn(x, y) { x + y; }";

    let l = Lexer::new(input);
    let mut p = Parser::new(l).into_iter();

    let program = p.next().unwrap()?;

    let exps = match &program {
        StatementNode::ExpressionStatement {
            token: _,
            expression,
        } => expression,
        _ => panic!("expression stateme is expected"),
    };

    let (fl_params, fl_body) = match &exps {
        ExpressionNode::FunctionLiteral {
            token: _,
            parameters,
            body,
        } => (parameters, body),
        _ => panic!("function literalis expected"),
    };

    assert!(
        fl_params.len() == 2,
        "parameters should have {}, but got {}",
        2,
        fl_params.len(),
    );

    test_literal_expression(&fl_params[0], &TestLiteral::IdentifierLiteral("x"));
    test_literal_expression(&fl_params[1], &TestLiteral::IdentifierLiteral("y"));

    let body_stmt = match &**(fl_body.as_ref().unwrap()) {
        StatementNode::BlockStatement {
            token: _,
            statements,
        } => statements,
        _ => panic!("block statement is expected here"),
    };

    assert!(
        body_stmt.len() == 1,
        "block statement does not have the expected number of statements. got {}",
        body_stmt.len(),
    );

    let body_stmt_exp = match &body_stmt[0] {
        StatementNode::ExpressionStatement {
            token: _,
            expression,
        } => expression,
        _ => panic!("expression statement is expected here"),
    };

    test_infix_expression(
        &body_stmt_exp,
        &TestLiteral::IdentifierLiteral("x"),
        "+",
        &TestLiteral::IdentifierLiteral("y"),
    );

    assert!(p.next().is_none());

    Ok(())
}

#[test]
fn test_function_parameter_parsing() -> Result<(), ParseError> {
    struct Test {
        input: &'static str,
        expected: Vec<&'static str>,
    }

    let func_test = vec![
        Test {
            input: "fn() {};",
            expected: vec![],
        },
        Test {
            input: "fn(x) {};",
            expected: vec!["x"],
        },
        Test {
            input: "fn(x, y, z) {};",
            expected: vec!["x", "y", "z"],
        },
    ];

    for tt in func_test {
        let l = Lexer::new(tt.input);
        let mut p = Parser::new(l).into_iter();

        let program = p.next().unwrap()?;

        let exps = match &program {
            StatementNode::ExpressionStatement {
                token: _,
                expression,
            } => expression,
            _ => panic!("expression stateme is expected"),
        };

        let fl_params = match &exps {
            ExpressionNode::FunctionLiteral {
                token: _,
                parameters,
                body: _,
            } => parameters,
            _ => panic!("function literalis expected"),
        };

        assert!(
            fl_params.len() == tt.expected.len(),
            "parameters should have {}, but got {}",
            tt.expected.len(),
            fl_params.len(),
        );

        for i in 0..(tt.expected.len()) {
            test_literal_expression(
                &fl_params[i],
                &TestLiteral::IdentifierLiteral(tt.expected[i]),
            );
        }
        assert!(p.next().is_none());
    }

    Ok(())
}

#[test]
fn test_call_expression_parsing() -> Result<(), ParseError> {
    let input = "add(1, 2 * 3, 4 + 5);";

    let l = Lexer::new(input);
    let mut p = Parser::new(l).into_iter();

    let program = p.next().unwrap()?;

    let exps = match &program {
        StatementNode::ExpressionStatement {
            token: _,
            expression,
        } => expression,
        _ => panic!("expression stateme is expected"),
    };

    let arguments = match &exps {
        ExpressionNode::CallExpression {
            token: _,
            function: _,
            arguments,
        } => arguments,
        _ => panic!("call expression is expected"),
    };

    assert!(
        arguments.len() == 3,
        "# of arguments should be {}, but got {}",
        3,
        arguments.len()
    );

    test_literal_expression(&arguments[0], &TestLiteral::IntegerLiteral(1));
    test_infix_expression(
        &arguments[1],
        &TestLiteral::IntegerLiteral(2),
        "*",
        &TestLiteral::IntegerLiteral(3),
    );
    test_infix_expression(
        &arguments[2],
        &TestLiteral::IntegerLiteral(4),
        "+",
        &TestLiteral::IntegerLiteral(5),
    );

    assert!(p.next().is_none());

    Ok(())
}

/// test for array literal
#[test]
fn test_parsing_array_literal() -> Result<(), ParseError> {
    let input = "[1, 2 * 2, 3 + 3];";

    let l = Lexer::new(input);
    let mut p = Parser::new(l).into_iter();

    let program = p.next().unwrap()?;

    let exps = match &program {
        StatementNode::ExpressionStatement {
            token: _,
            expression,
        } => expression,
        _ => panic!("expression stateme is expected"),
    };

    let elements = match &exps {
        ExpressionNode::ArrayLiteral { token: _, elements } => elements,
        _ => panic!("array literal node is expectd"),
    };

    assert!(
        elements.len() == 3,
        "the # of elements of the array literal should be 3, but {}",
        elements.len()
    );

    test_integer_literal(&elements[0], 1);
    test_infix_expression(
        &elements[1],
        &TestLiteral::IntegerLiteral(2),
        "*",
        &TestLiteral::IntegerLiteral(2),
    );
    test_infix_expression(
        &elements[2],
        &TestLiteral::IntegerLiteral(3),
        "+",
        &TestLiteral::IntegerLiteral(3),
    );

    assert!(p.next().is_none());

    Ok(())
}

/// test for index expressions
#[test]
fn test_parsing_index_expressions() -> Result<(), ParseError> {
    let input = "myArray[1 + 1]";

    let l = Lexer::new(input);
    let mut p = Parser::new(l).into_iter();

    let program = p.next().unwrap()?;

    let exps = match &program {
        StatementNode::ExpressionStatement {
            token: _,
            expression,
        } => expression,
        _ => panic!("expression stateme is expected"),
    };

    let (left, index) = match &exps {
        ExpressionNode::IndexExpression {
            token: _,
            left,
            index,
        } => (left, index),
        _ => panic!("index expression node is expectd"),
    };

    test_literal_expression(&left, &TestLiteral::IdentifierLiteral("myArray"));
    test_infix_expression(
        &index,
        &TestLiteral::IntegerLiteral(1),
        "+",
        &TestLiteral::IntegerLiteral(1),
    );
    assert!(p.next().is_none());

    Ok(())
}

/// test hash
#[test]
fn test_parsing_hash_literals_string_keys() -> Result<(), ParseError> {
    let input = r##"{"one": 1, "two": 2, "three": 3}"##;

    let l = Lexer::new(input);
    let mut p = Parser::new(l).into_iter();

    let program = p.next().unwrap()?;

    let exps = match &program {
        StatementNode::ExpressionStatement {
            token: _,
            expression,
        } => expression,
        _ => panic!("expression stateme is expected"),
    };

    let pairs = match &exps {
        ExpressionNode::HashLiteral { token: _, pairs } => pairs,
        _ => panic!("hash literal is expected"),
    };

    assert!(
        pairs.len() == 3,
        "the # of pairs of the hash literal should be 3, but {}",
        pairs.len()
    );

    test_literal_expression(&pairs[0].0, &TestLiteral::StringLiteral("one"));
    test_literal_expression(&pairs[1].0, &TestLiteral::StringLiteral("two"));
    test_literal_expression(&pairs[2].0, &TestLiteral::StringLiteral("three"));
    test_literal_expression(&pairs[0].1, &TestLiteral::IntegerLiteral(1));
    test_literal_expression(&pairs[1].1, &TestLiteral::IntegerLiteral(2));
    test_literal_expression(&pairs[2].1, &TestLiteral::IntegerLiteral(3));

    assert!(p.next().is_none());

    Ok(())
}

#[test]
fn test_parsing_empty_hash_literal() -> Result<(), ParseError> {
    let input = "{}";

    let l = Lexer::new(input);
    let mut p = Parser::new(l).into_iter();

    let program = p.next().unwrap()?;

    let exps = match &program {
        StatementNode::ExpressionStatement {
            token: _,
            expression,
        } => expression,
        _ => panic!("hash literal stateme is expected"),
    };

    let pairs = match &exps {
        ExpressionNode::HashLiteral { token: _, pairs } => pairs,
        _ => panic!("hash literal is expected"),
    };

    assert!(
        pairs.len() == 0,
        "the # of elements of {{}} should be 0, got {}",
        pairs.len()
    );

    assert!(p.next().is_none());

    Ok(())
}

#[test]
fn test_parsing_hash_literals_with_expressions() -> Result<(), ParseError> {
    let input = r##"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"##;

    let l = Lexer::new(input);
    let mut p = Parser::new(l).into_iter();

    let program = p.next().unwrap()?;

    let exps = match &program {
        StatementNode::ExpressionStatement {
            token: _,
            expression,
        } => expression,
        _ => panic!("hash literal stateme is expected"),
    };

    let pairs = match &exps {
        ExpressionNode::HashLiteral { token: _, pairs } => pairs,
        _ => panic!("hash literal is expected"),
    };

    assert!(
        pairs.len() == 3,
        "the # of elements of {{}} should be 3, got {}",
        pairs.len()
    );

    test_literal_expression(&pairs[0].0, &TestLiteral::StringLiteral("one"));
    test_infix_expression(
        &pairs[0].1,
        &TestLiteral::IntegerLiteral(0),
        "+",
        &TestLiteral::IntegerLiteral(1),
    );

    test_literal_expression(&pairs[1].0, &TestLiteral::StringLiteral("two"));
    test_infix_expression(
        &pairs[1].1,
        &TestLiteral::IntegerLiteral(10),
        "-",
        &TestLiteral::IntegerLiteral(8),
    );

    test_literal_expression(&pairs[2].0, &TestLiteral::StringLiteral("three"));
    test_infix_expression(
        &pairs[2].1,
        &TestLiteral::IntegerLiteral(15),
        "/",
        &TestLiteral::IntegerLiteral(5),
    );
    assert!(p.next().is_none());

    Ok(())
}

/// test integer literal
///
/// # Arguments
///
/// * `en` - `ExpressionNode::IntegerLiteralNode` to be tested
/// * `value` - the value that `en` should have
fn test_integer_literal(en: &ExpressionNode, value: i64) {
    let il_token = match &en {
        ExpressionNode::IntegerLiteral { token } => token,
        _ => panic!("unexpected node"),
    };

    assert_eq!(il_token, &Token::Int(value),);
}

/// test identifier literal
///
/// # Arguments
///
/// * `en` - `ExpressionNode::IdentifierNode` to be tested
/// * `value` - the value that `en` should have
fn test_identifier_literal(en: &ExpressionNode, value: &'static str) {
    let id_token = match &en {
        ExpressionNode::Identifier { token } => token,
        _ => panic!("unexpected node"),
    };

    assert_eq!(id_token, &Token::Ident(value.to_string()),);
}

/// test string literal
///
/// # Arguments
///
/// * `en` - `ExpressionNode::StringNode` to be tested
/// * `value` - the value that `en` should have
fn test_string_literal(en: &ExpressionNode, value: &'static str) {
    let id_token = match &en {
        ExpressionNode::StringLiteral { token } => token,
        _ => panic!("unexpected node, {:?}", en),
    };

    assert_eq!(id_token, &Token::String(value.to_string()),);
}

/// test boolean literal
fn test_boolean_literal(en: &ExpressionNode, value: bool) {
    let (bl_token, bl_value) = match &en {
        ExpressionNode::Boolean { token, value } => (token, value),
        _ => panic!("boolean expression node is expected"),
    };

    assert_eq!(
        bl_value, &value,
        "{:?} is expected, but got {:?}",
        value, bl_value,
    );

    assert_eq!(
        String::from(bl_token),
        format!("{:?}", value),
        r##"{:?} is expected, but got {}"##,
        format!("{:?}", value),
        bl_token
    );
}

/// test expression's literal
fn test_literal_expression(en: &ExpressionNode, literal: &TestLiteral) {
    match literal {
        TestLiteral::IntegerLiteral(i) => test_integer_literal(&en, *i),
        TestLiteral::IdentifierLiteral(id) => test_identifier_literal(&en, id),
        TestLiteral::StringLiteral(s) => test_string_literal(&en, s),
        TestLiteral::BooleanLiteral(b) => test_boolean_literal(&en, *b),
    }
}

/// test infix expression
fn test_infix_expression(
    en: &ExpressionNode,
    ex_left: &TestLiteral,
    ex_operator: &'static str,
    ex_right: &TestLiteral,
) {
    let (token, left, right) = match &en {
        ExpressionNode::InfixExpression { token, left, right } => (token, left, right),
        _ => panic!("not infix expression {:?}", en),
    };

    test_literal_expression(left, &ex_left);

    assert_eq!(
        format!("{}", token),
        ex_operator,
        "unexpected operator {} (expected {})",
        token,
        ex_operator,
    );

    test_literal_expression(right, &ex_right);
}
