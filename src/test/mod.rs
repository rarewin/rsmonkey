use rsmonkey::ast::*;
use rsmonkey::lexer::Lexer;
use rsmonkey::parser::Parser;
use rsmonkey::token::{Token, TokenType};

enum TestLiteral {
    IntegerLiteral(i64),
    StringLiteral(&'static str),
    BooleanLiteral(bool),
}

#[test]
fn test_next_token() {
    let input = r##"=+(){},; let five = 5;
    let ten = 10;

    let add = fn(x, y) {
      x + y;
    };

    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
      return true;
    } else {
      return false;
    }

    10 == 10;
    10 != 9;"##;

    let tests = [
        Token::new(TokenType::Assign, "="),
        Token::new(TokenType::Plus, "+"),
        Token::new(TokenType::LParen, "("),
        Token::new(TokenType::RParen, ")"),
        Token::new(TokenType::LBrace, "{"),
        Token::new(TokenType::RBrace, "}"),
        Token::new(TokenType::Comma, ","),
        Token::new(TokenType::Semicolon, ";"),
        //
        Token::new(TokenType::Let, "let"),
        Token::new(TokenType::Ident, "five"),
        Token::new(TokenType::Assign, "="),
        Token::new(TokenType::Int, "5"),
        Token::new(TokenType::Semicolon, ";"),
        //
        Token::new(TokenType::Let, "let"),
        Token::new(TokenType::Ident, "ten"),
        Token::new(TokenType::Assign, "="),
        Token::new(TokenType::Int, "10"),
        Token::new(TokenType::Semicolon, ";"),
        //
        Token::new(TokenType::Let, "let"),
        Token::new(TokenType::Ident, "add"),
        Token::new(TokenType::Assign, "="),
        Token::new(TokenType::Function, "fn"),
        Token::new(TokenType::LParen, "("),
        Token::new(TokenType::Ident, "x"),
        Token::new(TokenType::Comma, ","),
        Token::new(TokenType::Ident, "y"),
        Token::new(TokenType::RParen, ")"),
        Token::new(TokenType::LBrace, "{"),
        Token::new(TokenType::Ident, "x"),
        Token::new(TokenType::Plus, "+"),
        Token::new(TokenType::Ident, "y"),
        Token::new(TokenType::Semicolon, ";"),
        Token::new(TokenType::RBrace, "}"),
        Token::new(TokenType::Semicolon, ";"),
        //
        Token::new(TokenType::Let, "let"),
        Token::new(TokenType::Ident, "result"),
        Token::new(TokenType::Assign, "="),
        Token::new(TokenType::Ident, "add"),
        Token::new(TokenType::LParen, "("),
        Token::new(TokenType::Ident, "five"),
        Token::new(TokenType::Comma, ","),
        Token::new(TokenType::Ident, "ten"),
        Token::new(TokenType::RParen, ")"),
        Token::new(TokenType::Semicolon, ";"),
        //
        Token::new(TokenType::Bang, "!"),
        Token::new(TokenType::Minus, "-"),
        Token::new(TokenType::Slash, "/"),
        Token::new(TokenType::Asterisk, "*"),
        Token::new(TokenType::Int, "5"),
        Token::new(TokenType::Semicolon, ";"),
        //
        Token::new(TokenType::Int, "5"),
        Token::new(TokenType::LT, "<"),
        Token::new(TokenType::Int, "10"),
        Token::new(TokenType::GT, ">"),
        Token::new(TokenType::Int, "5"),
        Token::new(TokenType::Semicolon, ";"),
        //
        Token::new(TokenType::If, "if"),
        Token::new(TokenType::LParen, "("),
        Token::new(TokenType::Int, "5"),
        Token::new(TokenType::LT, "<"),
        Token::new(TokenType::Int, "10"),
        Token::new(TokenType::RParen, ")"),
        Token::new(TokenType::LBrace, "{"),
        Token::new(TokenType::Return, "return"),
        Token::new(TokenType::True, "true"),
        Token::new(TokenType::Semicolon, ";"),
        Token::new(TokenType::RBrace, "}"),
        Token::new(TokenType::Else, "else"),
        Token::new(TokenType::LBrace, "{"),
        Token::new(TokenType::Return, "return"),
        Token::new(TokenType::False, "false"),
        Token::new(TokenType::Semicolon, ";"),
        Token::new(TokenType::RBrace, "}"),
        //
        Token::new(TokenType::Int, "10"),
        Token::new(TokenType::Eq, "=="),
        Token::new(TokenType::Int, "10"),
        Token::new(TokenType::Semicolon, ";"),
        //
        Token::new(TokenType::Int, "10"),
        Token::new(TokenType::NotEq, "!="),
        Token::new(TokenType::Int, "9"),
        Token::new(TokenType::Semicolon, ";"),
        //
        Token::new(TokenType::EoF, "EOF"),
    ];

    let mut l = Lexer::new(input.to_string());

    for tp in tests.iter() {
        let tok = l.next_token();
        assert_eq!(&tok, tp);
    }
}

#[test]
fn test_let_statements() {
    let input = r##"let x = 5;
                    let y = 10;
                    let foobar = 838383;"##;

    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(p);

    assert!(
        program.statements.len() == 3,
        "length of program.statements should be {}, but got {}",
        3,
        program.statements.len()
    );

    let tests = ["x", "y", "foobar"];

    for i in 0..tests.len() {
        let stmt = match &program.statements[i] {
            StatementNode::LetStatementNode(s) => s,
            _ => panic!(),
        };
        let tt = tests[i];

        test_let_statement(stmt, tt);
    }
}

/// test let statement.
/// get panicked if the test is failed.
///
/// # Arguments
///
/// * `s` - let statement
/// * `name` - expected string for identifer of `s`
///
/// # Examples
///
/// ```
/// extern crate rsmonkey;
/// use rsmonkey::ast::LetStatement;
/// use rsmonkey::token::{TokenType, Token};
///
/// let stmt = LetStatement {
///    token: Token {
///       token_type: TokenType::Ident,
///       literal: "0".to_string(),
///    },
///    name: Identifier {
///       token: Token {
///          token_type: TokenType::Ident,
///          literal: "0".to_string(),
///        },
///        value: "0".to_string(),
/// };
/// ```
fn test_let_statement(s: &LetStatement, name: &str) {
    assert_eq!(
        s.name.value, name,
        "expected identifier is '{}', but got '{}'.",
        name, s.name.value,
    );
    assert_eq!(
        s.name.token_literal(),
        name,
        "expected identifier is '{}', but got '{}'.",
        name,
        s.name.token_literal()
    );
}

#[test]
fn test_return_statements() {
    let input = r##"
    return 5;
    return 10;
    return 993322;"##;

    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let program = p.parse_program();
    check_parser_errors(p);

    assert!(
        program.statements.len() == 3,
        "length of program.statements should be {}, but got {}",
        3,
        program.statements.len()
    );

    for i in 0..program.statements.len() {
        match &program.statements[i] {
            StatementNode::ReturnStatementNode(_) => {}
            _ => panic!(),
        }
    }
}

#[test]
fn test_string() {
    let v = vec![StatementNode::LetStatementNode(Box::new(LetStatement {
        token: Token {
            token_type: TokenType::Let,
            literal: "let".to_string(),
        },
        name: Identifier {
            token: Token {
                token_type: TokenType::Ident,
                literal: "myVar".to_string(),
            },
            value: "myVar".to_string(),
        },
        value: ExpressionNode::IdentifierNode(Box::new(Identifier {
            token: Token {
                token_type: TokenType::Ident,
                literal: "anotherVar".to_string(),
            },
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

    assert_eq!(stmt.token.token_literal(), "foobar");
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
        &TestLiteral::StringLiteral("x"),
        "<",
        &TestLiteral::StringLiteral("y"),
    );

    let cs = match &exp.consequence {
        StatementNode::BlockStatementNode(bs) => bs,
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

    test_literal_expression(&conex.expression, &TestLiteral::StringLiteral("x"));

    match &exp.alternative {
        StatementNode::Null => {}
        _ => panic!("alternative has unexpected statement node"),
    };
}

#[test]
fn test_if_else_expression() {
    let input = "if (x < y) { x } else { y };";

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
        &TestLiteral::StringLiteral("x"),
        "<",
        &TestLiteral::StringLiteral("y"),
    );

    let cs = match &exp.consequence {
        StatementNode::BlockStatementNode(bs) => bs,
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

    test_literal_expression(&conex.expression, &TestLiteral::StringLiteral("x"));

    let als = match &exp.alternative {
        StatementNode::BlockStatementNode(bs) => bs,
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

    test_literal_expression(&alsex.expression, &TestLiteral::StringLiteral("y"));
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

    test_literal_expression(&fl.parameters[0], &TestLiteral::StringLiteral("x"));
    test_literal_expression(&fl.parameters[1], &TestLiteral::StringLiteral("y"));

    let body = match &fl.body {
        StatementNode::BlockStatementNode(b) => b,
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
        &TestLiteral::StringLiteral("x"),
        "+",
        &TestLiteral::StringLiteral("y"),
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
                &TestLiteral::StringLiteral(tt.expected[i]),
            );
        }
    }
}

fn check_parser_errors(p: Parser) {
    let errors = p.errors();

    if errors.len() == 0 {
        return;
    }

    println!("parser has {} errors", errors.len());

    for e in errors {
        println!("parser error: {}", e);
    }
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
        il.token_literal(),
        format!("{}", value),
        "token_literal() is expected as {} bug got {}",
        format!("{}", value),
        il.token_literal(),
    );
}

/// test string literal
///
/// # Arguments
///
/// * `en` - `ExpressionNode::IdentifierNode` to be tested
/// * `value` - the value that `en` should have
fn test_string_literal(en: &ExpressionNode, value: &'static str) {
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
        id.token_literal(),
        value,
        r##"token_literal() is expected as "{}" but got "{}""##,
        id.token_literal(),
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
        bl.token.token_literal(),
        format!("{:?}", value),
        r##"{:?} is expected, but got {:?}"##,
        format!("{:?}", value),
        bl.token.token_literal(),
    );
}

/// test expression's literal
fn test_literal_expression(en: &ExpressionNode, literal: &TestLiteral) {
    match literal {
        TestLiteral::IntegerLiteral(i) => test_integer_literal(&en, *i),
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
