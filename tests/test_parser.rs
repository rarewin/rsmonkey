use rsmonkey::ast::*;
use rsmonkey::lexer::Lexer;
use rsmonkey::parser::Parser;
use rsmonkey::token::{Token, TokenType};

enum TestLiteral {
    IntegerLiteral(i64),
    IdentifierLiteral(&'static str),
    StringLiteral(&'static str),
    BooleanLiteral(bool),
}

#[test]
fn test_let_statements() {
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
        let l = Lexer::new(tt.input.to_string());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(p);

        assert!(
            program.statements.len() == 1,
            "length of program.statements should be {}, but got {}",
            1,
            program.statements.len()
        );

        let stmt = match &program.statements[0] {
            StatementNode::LetStatementNode(s) => s,
            _ => panic!("let statement is expected"),
        };

        test_let_statement(stmt, tt.expected_identifier, &tt.expected_value);
    }
}

/// test let statement.
/// get panicked if the test is failed.
///
/// # Arguments
///
/// * `s` - let statement
/// * `name` - expected string for identifer of `s`
/// * `value` - expected value for right value
/// ```
fn test_let_statement(s: &LetStatement, name: &str, value: &TestLiteral) {
    assert_eq!(
        s.name.value, name,
        "expected identifier is '{}', but got '{}'.",
        name, s.name.value,
    );
    assert_eq!(
        s.name.token.get_literal(),
        name,
        "expected identifier is '{}', but got '{}'.",
        name,
        s.name.token.get_literal()
    );
    test_literal_expression(&s.value, value);
}

#[test]
fn test_return_statements() {
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
        let l = Lexer::new(tt.input.to_string());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(p);

        assert!(
            program.statements.len() == 1,
            "length of program.statements should be {}, but got {}",
            1,
            program.statements.len()
        );

        let stmt = match &program.statements[0] {
            StatementNode::ReturnStatementNode(rs) => rs,
            _ => panic!("return statement is expected"),
        };

        test_literal_expression(&stmt.return_value, &tt.expected_value);
    }
}

#[test]
fn test_string() {
    let v = vec![StatementNode::LetStatementNode(Box::new(LetStatement {
        token: Token::new(TokenType::Let, "let"),
        name: Identifier {
            token: Token::new(TokenType::Ident, "myVar"),
            value: "myVar".to_string(),
        },
        value: ExpressionNode::IdentifierNode(Box::new(Identifier {
            token: Token::new(TokenType::Ident, "anotherVar"),
            value: "myVar".to_string(),
        })),
    }))];

    let program = Program { statements: { v } };

    assert_eq!(program.string(), "let myVar = anotherVar;");
}

#[test]
fn test_identifier_expression() {
    let input = r##"foobar;"##;

    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(p);

    assert_eq!(
        program.statements.len(),
        1,
        "program does not have enough statements. got {}",
        program.statements.len()
    );

    let stmt = match &program.statements[0] {
        StatementNode::ExpressionStatementNode(es) => es,
        _ => panic!("first statement is not expressionstatement"),
    };

    assert_eq!(stmt.token.get_literal(), "foobar");
}

#[test]
fn test_integer_literal_expression() {
    let input = r##"5;"##;

    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(p);

    assert_eq!(
        program.statements.len(),
        1,
        "program does not have enough statements. got {}",
        program.statements.len()
    );

    assert!(program.statements[0].get_literal() == "5");
}

/// test string literal
#[test]
fn test_string_literal_expression() {
    let input = r##""hello world""##;

    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(p);

    assert_eq!(
        program.statements.len(),
        1,
        "program does not have enough statements. got {}",
        program.statements.len()
    );

    assert!(program.statements[0].get_literal() == "hello world");
}

/// test boolean
#[test]
fn test_boolean_literal_expression() {
    struct Test {
        input: &'static str,
        value: bool,
    };

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
        let l = Lexer::new(tt.input.to_string());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(p);

        assert_eq!(
            program.statements.len(),
            1,
            "program does not have enough statements. got {}",
            program.statements.len()
        );

        let stmt = match &program.statements[0] {
            StatementNode::ExpressionStatementNode(es) => es,
            _ => panic!("first statement is not expression statement"),
        };

        let exp = match &stmt.expression {
            ExpressionNode::BooleanExpressionNode(bn) => bn,
            _ => panic!("this expression statement does not have boolean expression"),
        };

        assert_eq!(
            exp.value, tt.value,
            "{} is expected, but got {}",
            tt.value, exp.value,
        );
    }
}

#[test]
fn test_parsing_prefix_expressions() {
    struct Test {
        input: &'static str,
        operator: &'static str,
        value: TestLiteral,
    };

    let prefix_test = vec![
        Test {
            input: "!5;",
            operator: "!",
            value: TestLiteral::IntegerLiteral(5),
        },
        Test {
            input: "-15;",
            operator: "-",
            value: TestLiteral::IntegerLiteral(15),
        },
        Test {
            input: "!true;",
            operator: "!",
            value: TestLiteral::BooleanLiteral(true),
        },
        Test {
            input: "!false;",
            operator: "!",
            value: TestLiteral::BooleanLiteral(false),
        },
    ];

    for tt in prefix_test {
        let l = Lexer::new(tt.input.to_string());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(p);

        assert_eq!(
            program.statements.len(),
            1,
            "program does not have the expected number of statements. {}",
            program.statements.len()
        );

        let stmt = match &program.statements[0] {
            StatementNode::ExpressionStatementNode(es) => es,
            _ => panic!("first statement is not expression statement"),
        };

        let exp = match &stmt.expression {
            ExpressionNode::PrefixExpressionNode(pe) => pe,
            _ => panic!("this expression statement does not have prefix expression"),
        };

        assert_eq!(
            tt.operator, exp.operator,
            "unexpected operator {} (expected {})",
            exp.operator, tt.operator
        );

        test_literal_expression(&exp.right, &tt.value);
    }
}

#[test]
fn test_parsing_infix_expressions() {
    struct Test {
        input: &'static str,
        left_value: TestLiteral,
        operator: &'static str,
        right_value: TestLiteral,
    };

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
        let l = Lexer::new(tt.input.to_string());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(p);

        let stmt = match &program.statements[0] {
            StatementNode::ExpressionStatementNode(es) => es,
            _ => panic!("first statement is not expression statement"),
        };

        test_infix_expression(
            &stmt.expression,
            &tt.left_value,
            tt.operator,
            &tt.right_value,
        );
    }
}

#[test]
fn test_operator_precedence_parsing() {
    struct Test {
        input: &'static str,
        expected: &'static str,
    };

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
        let l = Lexer::new(tt.input.to_string());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(p);

        assert_eq!(
            program.string(),
            tt.expected,
            r##"expected "{}", got "{}""##,
            tt.expected,
            program.string(),
        );
    }
}

#[test]
fn test_if_expression() {
    let input = "if (x < y) { x };";

    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(p);

    assert!(
        program.statements.len() == 1,
        "program does not have the expected number of statements. {}",
        program.statements.len()
    );

    let exps = match &program.statements[0] {
        StatementNode::ExpressionStatementNode(es) => es,
        _ => panic!("expression stateme is expected"),
    };

    let exp = match &exps.expression {
        ExpressionNode::IfExpressionNode(ie) => ie,
        _ => panic!("if expression is expected"),
    };

    test_infix_expression(
        &exp.condition,
        &TestLiteral::IdentifierLiteral("x"),
        "<",
        &TestLiteral::IdentifierLiteral("y"),
    );

    let cs = match &exp.consequence {
        Some(StatementNode::BlockStatementNode(bs)) => bs,
        _ => panic!("consequence does not have block statement"),
    };

    assert!(
        cs.statements.len() == 1,
        "consequence does not have the expected number of statements. {}",
        cs.statements.len()
    );

    let conex = match &cs.statements[0] {
        StatementNode::ExpressionStatementNode(es) => es,
        _ => panic!("expression statement is expected"),
    };

    test_literal_expression(&conex.expression, &TestLiteral::IdentifierLiteral("x"));

    match &exp.alternative {
        None => {}
        _ => panic!("alternative has unexpected statement node"),
    };
}

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x; } else { y };";

    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(p);

    assert!(
        program.statements.len() == 1,
        "program does not have the expected number of statements. {}",
        program.statements.len()
    );

    let stmt = &program.statements[0];

    let exps = match &stmt {
        StatementNode::ExpressionStatementNode(es) => es,
        _ => panic!("expression stateme is expected"),
    };

    let exp = match &exps.expression {
        ExpressionNode::IfExpressionNode(ie) => ie,
        _ => panic!("if expression is expected"),
    };

    test_infix_expression(
        &exp.condition,
        &TestLiteral::IdentifierLiteral("x"),
        "<",
        &TestLiteral::IdentifierLiteral("y"),
    );

    let cs = match &exp.consequence {
        Some(StatementNode::BlockStatementNode(bs)) => bs,
        _ => panic!("consequence does not have block statement"),
    };

    assert!(
        cs.statements.len() == 1,
        "consequence does not have the expected number of statements. {}",
        cs.statements.len()
    );

    let conex = match &cs.statements[0] {
        StatementNode::ExpressionStatementNode(es) => es,
        _ => panic!("expression statement is expected"),
    };

    test_literal_expression(&conex.expression, &TestLiteral::IdentifierLiteral("x"));

    let als = match &exp.alternative {
        Some(StatementNode::BlockStatementNode(bs)) => bs,
        _ => panic!("alternative does not have block statement"),
    };

    assert!(
        als.statements.len() == 1,
        "consequence does not have the expected number of statements. {}",
        als.statements.len()
    );

    let alsex = match &als.statements[0] {
        StatementNode::ExpressionStatementNode(es) => es,
        _ => panic!("expression statement is expected"),
    };

    test_literal_expression(&alsex.expression, &TestLiteral::IdentifierLiteral("y"));

    assert_eq!(stmt.string(), "if (x < y) x else y");
}

#[test]
fn test_function_literal_parsing() {
    let input = "fn(x, y) { x + y; }";

    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(p);

    assert!(
        program.statements.len() == 1,
        "program does not have the expected number of statements. {}",
        program.statements.len()
    );

    let exps = match &program.statements[0] {
        StatementNode::ExpressionStatementNode(exps) => exps,
        _ => panic!("expression stateme is expected"),
    };

    let fl = match &exps.expression {
        ExpressionNode::FunctionLiteralNode(f) => f,
        _ => panic!("function literalis expected"),
    };

    assert!(
        fl.parameters.len() == 2,
        "parameters should have {}, but got {}",
        2,
        fl.parameters.len(),
    );

    test_literal_expression(&fl.parameters[0], &TestLiteral::IdentifierLiteral("x"));
    test_literal_expression(&fl.parameters[1], &TestLiteral::IdentifierLiteral("y"));

    let body = match &fl.body {
        Some(StatementNode::BlockStatementNode(b)) => b,
        _ => panic!("block statement is expected here"),
    };

    assert!(
        body.statements.len() == 1,
        "block statement does not have the expected number of statements. got {}",
        body.statements.len(),
    );

    let body_stmt = match &body.statements[0] {
        StatementNode::ExpressionStatementNode(e) => e,
        _ => panic!("expression statement is expected here"),
    };

    test_infix_expression(
        &body_stmt.expression,
        &TestLiteral::IdentifierLiteral("x"),
        "+",
        &TestLiteral::IdentifierLiteral("y"),
    );
}

#[test]
fn test_function_parameter_parsing() {
    struct Test {
        input: &'static str,
        expected: Vec<&'static str>,
    };

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
        let l = Lexer::new(tt.input.to_string());
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(p);

        assert!(
            program.statements.len() == 1,
            "program does not have the expected number of statements. {}",
            program.statements.len()
        );

        let exps = match &program.statements[0] {
            StatementNode::ExpressionStatementNode(exps) => exps,
            _ => panic!("expression stateme is expected"),
        };

        let fl = match &exps.expression {
            ExpressionNode::FunctionLiteralNode(f) => f,
            _ => panic!("function literalis expected"),
        };

        assert!(
            fl.parameters.len() == tt.expected.len(),
            "parameters should have {}, but got {}",
            tt.expected.len(),
            fl.parameters.len(),
        );

        for i in 0..(tt.expected.len()) {
            test_literal_expression(
                &fl.parameters[i],
                &TestLiteral::IdentifierLiteral(tt.expected[i]),
            );
        }
    }
}

#[test]
fn test_call_expression_arsing() {
    let input = "add(1, 2 * 3, 4 + 5);";

    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(p);

    assert!(
        program.statements.len() == 1,
        "program does not have the expected number of statements. {}",
        program.statements.len()
    );

    let exps = match &program.statements[0] {
        StatementNode::ExpressionStatementNode(exps) => exps,
        _ => panic!("expression stateme is expected"),
    };

    let fc = match &exps.expression {
        ExpressionNode::CallExpressionNode(fc) => fc,
        _ => panic!("call expression is expected"),
    };

    assert!(
        fc.arguments.len() == 3,
        "# of arguments should be {}, but got {}",
        3,
        fc.arguments.len()
    );

    test_literal_expression(&fc.arguments[0], &TestLiteral::IntegerLiteral(1));
    test_infix_expression(
        &fc.arguments[1],
        &TestLiteral::IntegerLiteral(2),
        "*",
        &TestLiteral::IntegerLiteral(3),
    );
    test_infix_expression(
        &fc.arguments[2],
        &TestLiteral::IntegerLiteral(4),
        "+",
        &TestLiteral::IntegerLiteral(5),
    );
}

/// test for array literal
#[test]
fn test_parsing_array_literal() {
    let input = "[1, 2 * 2, 3 + 3];";

    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(p);

    assert!(
        program.statements.len() == 1,
        "program does not have the expected number of statements. {}",
        program.statements.len()
    );

    let exps = match &program.statements[0] {
        StatementNode::ExpressionStatementNode(e) => e,
        _ => panic!("expression stateme is expected"),
    };

    let al = match &exps.expression {
        ExpressionNode::ArrayLiteralNode(a) => a,
        _ => panic!("array literal node is expectd"),
    };

    assert!(
        al.elements.len() == 3,
        "the # of elements of the array literal should be 3, but {}",
        al.elements.len()
    );

    test_integer_literal(&al.elements[0], 1);
    test_infix_expression(
        &al.elements[1],
        &TestLiteral::IntegerLiteral(2),
        "*",
        &TestLiteral::IntegerLiteral(2),
    );
    test_infix_expression(
        &al.elements[2],
        &TestLiteral::IntegerLiteral(3),
        "+",
        &TestLiteral::IntegerLiteral(3),
    );
}

/// test for index expressions
#[test]
fn test_parsing_index_expressions() {
    let input = "myArray[1 + 1]";

    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(p);

    assert!(
        program.statements.len() == 1,
        "program does not have the expected number of statements. {}",
        program.statements.len()
    );

    let exps = match &program.statements[0] {
        StatementNode::ExpressionStatementNode(e) => e,
        _ => panic!("expression stateme is expected"),
    };

    let il = match &exps.expression {
        ExpressionNode::IndexExpressionNode(a) => a,
        _ => panic!("index expression node is expectd"),
    };

    test_literal_expression(&il.left, &TestLiteral::IdentifierLiteral("myArray"));
    test_infix_expression(
        &il.index,
        &TestLiteral::IntegerLiteral(1),
        "+",
        &TestLiteral::IntegerLiteral(1),
    );
}

/// test hash
#[test]
fn test_parsing_hash_literals_string_keys() {
    let input = r##"{"one": 1, "two": 2, "three": 3}"##;

    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(p);

    assert!(
        program.statements.len() == 1,
        "program does not have the expected number of statements. {}",
        program.statements.len()
    );

    let exps = match &program.statements[0] {
        StatementNode::ExpressionStatementNode(e) => e,
        _ => panic!("expression stateme is expected"),
    };

    let hl = match &exps.expression {
        ExpressionNode::HashLiteralNode(h) => h,
        _ => panic!("hash literal is expected"),
    };

    assert!(
        hl.pairs.len() == 3,
        "the # of pairs of the hash literal should be 3, but {}",
        hl.pairs.len()
    );

    test_literal_expression(&hl.pairs[0].0, &TestLiteral::StringLiteral("one"));
    test_literal_expression(&hl.pairs[1].0, &TestLiteral::StringLiteral("two"));
    test_literal_expression(&hl.pairs[2].0, &TestLiteral::StringLiteral("three"));
    test_literal_expression(&hl.pairs[0].1, &TestLiteral::IntegerLiteral(1));
    test_literal_expression(&hl.pairs[1].1, &TestLiteral::IntegerLiteral(2));
    test_literal_expression(&hl.pairs[2].1, &TestLiteral::IntegerLiteral(3));
}

#[test]
fn test_parsing_empty_hash_literal() {
    let input = "{}";

    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(p);

    assert!(
        program.statements.len() == 1,
        "program does not have the expected number of statements. {}",
        program.statements.len()
    );

    let exps = match &program.statements[0] {
        StatementNode::ExpressionStatementNode(e) => e,
        _ => panic!("hash literal stateme is expected"),
    };

    let hl = match &exps.expression {
        ExpressionNode::HashLiteralNode(h) => h,
        _ => panic!("hash literal is expected"),
    };

    assert!(
        hl.pairs.len() == 0,
        "the # of elements of {{}} should be 0, got {}",
        hl.pairs.len()
    );
}

#[test]
fn test_parsing_hash_literals_with_expressions() {
    let input = r##"{"one": 0 + 1, "two": 10 - 8, "three": 15 / 5}"##;

    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(p);

    assert!(
        program.statements.len() == 1,
        "program does not have the expected number of statements. {}",
        program.statements.len()
    );

    let exps = match &program.statements[0] {
        StatementNode::ExpressionStatementNode(e) => e,
        _ => panic!("hash literal stateme is expected"),
    };

    let hl = match &exps.expression {
        ExpressionNode::HashLiteralNode(h) => h,
        _ => panic!("hash literal is expected"),
    };

    assert!(
        hl.pairs.len() == 3,
        "the # of elements of {{}} should be 3, got {}",
        hl.pairs.len()
    );

    test_literal_expression(&hl.pairs[0].0, &TestLiteral::StringLiteral("one"));
    test_infix_expression(
        &hl.pairs[0].1,
        &TestLiteral::IntegerLiteral(0),
        "+",
        &TestLiteral::IntegerLiteral(1),
    );

    test_literal_expression(&hl.pairs[1].0, &TestLiteral::StringLiteral("two"));
    test_infix_expression(
        &hl.pairs[1].1,
        &TestLiteral::IntegerLiteral(10),
        "-",
        &TestLiteral::IntegerLiteral(8),
    );

    test_literal_expression(&hl.pairs[2].0, &TestLiteral::StringLiteral("three"));
    test_infix_expression(
        &hl.pairs[2].1,
        &TestLiteral::IntegerLiteral(15),
        "/",
        &TestLiteral::IntegerLiteral(5),
    );
}

fn check_parser_errors(p: Parser) {
    let errors = p.errors();

    if errors.is_empty() {
        return;
    }

    for e in errors {
        println!("parser error: {}", e);
    }

    panic!("parser has {} errors", errors.len());
}

/// test integer literal
///
/// # Arguments
///
/// * `en` - `ExpressionNode::IntegerLiteralNode` to be tested
/// * `value` - the value that `en` should have
fn test_integer_literal(en: &ExpressionNode, value: i64) {
    let il = match &en {
        ExpressionNode::IntegerLiteralNode(iln) => iln,
        _ => panic!("unexpected node"),
    };

    assert_eq!(
        il.value, value,
        "integer value is expected as {} but got {}",
        value, il.value
    );

    assert_eq!(
        il.token.get_literal(),
        format!("{}", value),
        "get_literal() is expected as {} bug got {}",
        format!("{}", value),
        il.token.get_literal(),
    );
}

/// test identifier literal
///
/// # Arguments
///
/// * `en` - `ExpressionNode::IdentifierNode` to be tested
/// * `value` - the value that `en` should have
fn test_identifier_literal(en: &ExpressionNode, value: &'static str) {
    let id = match &en {
        ExpressionNode::IdentifierNode(idn) => idn,
        _ => panic!("unexpected node"),
    };

    assert_eq!(
        id.value, value,
        r##"value is expected as "{}" but got "{}""##,
        value, id.value,
    );

    assert_eq!(
        id.token.get_literal(),
        value,
        r##"get_literal() is expected as "{}" but got "{}""##,
        id.token.get_literal(),
        id.value,
    );
}

/// test string literal
///
/// # Arguments
///
/// * `en` - `ExpressionNode::StringNode` to be tested
/// * `value` - the value that `en` should have
fn test_string_literal(en: &ExpressionNode, value: &'static str) {
    let id = match &en {
        ExpressionNode::StringLiteralNode(idn) => idn,
        _ => panic!("unexpected node, {:?}", en),
    };

    assert_eq!(
        id.value, value,
        r##"value is expected as "{}" but got "{}""##,
        value, id.value,
    );

    assert_eq!(
        id.token.get_literal(),
        value,
        r##"get_literal() is expected as "{}" but got "{}""##,
        id.token.get_literal(),
        id.value,
    );
}

/// test boolean literal
fn test_boolean_literal(en: &ExpressionNode, value: bool) {
    let bl = match &en {
        ExpressionNode::BooleanExpressionNode(be) => be,
        _ => panic!("boolean expression node is expected"),
    };

    assert_eq!(
        bl.value, value,
        "{:?} is expected, but got {:?}",
        value, bl.value,
    );

    assert_eq!(
        bl.token.get_literal(),
        format!("{:?}", value),
        r##"{:?} is expected, but got {:?}"##,
        format!("{:?}", value),
        bl.token.get_literal(),
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
    let ifn = match &en {
        ExpressionNode::InfixExpressionNode(ie) => ie,
        _ => panic!("not infix expression {:?}", en),
    };

    test_literal_expression(&ifn.left, &ex_left);

    assert_eq!(
        ifn.operator, ex_operator,
        "unexpected operator {} (expected {})",
        ifn.operator, ex_operator,
    );

    test_literal_expression(&ifn.right, &ex_right);
}
