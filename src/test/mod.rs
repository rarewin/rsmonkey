use rsmonkey::ast::*;
use rsmonkey::lexer::Lexer;
use rsmonkey::parser::Parser;
use rsmonkey::token::{Token, TokenType};

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

#[test]
fn test_parsing_prefix_expressions() {
    struct Test {
        input: String,
        operator: String,
        integer_value: i64,
    };

    let prefix_test = vec![
        Test {
            input: "!5;".to_string(),
            operator: "!".to_string(),
            integer_value: 5,
        },
        Test {
            input: "-15;".to_string(),
            operator: "-".to_string(),
            integer_value: 15,
        },
    ];

    for tt in prefix_test {
        let l = Lexer::new(tt.input);
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
            ExpressionNode::PrefixExpressionNode(pe) => pe,
            _ => panic!("this expression statement does not have prefix expression"),
        };

        assert_eq!(
            tt.operator, exp.operator,
            "unexpected operator {} (expected {})",
            exp.operator, tt.operator
        );

        test_integer_literal(&exp.right, tt.integer_value);
    }
}

#[test]
fn test_parsing_infix_expressions() {
    struct Test {
        input: String,
        left_value: i64,
        operator: String,
        right_value: i64,
    };

    let infix_test = vec![
        Test {
            input: "5 + 5;".to_string(),
            left_value: 5,
            operator: "+".to_string(),
            right_value: 5,
        },
        Test {
            input: "5 - 5;".to_string(),
            left_value: 5,
            operator: "-".to_string(),
            right_value: 5,
        },
        Test {
            input: "5 * 4;".to_string(),
            left_value: 5,
            operator: "*".to_string(),
            right_value: 4,
        },
        Test {
            input: "8 / 4;".to_string(),
            left_value: 8,
            operator: "/".to_string(),
            right_value: 4,
        },
        Test {
            input: "2 > 8;".to_string(),
            left_value: 2,
            operator: ">".to_string(),
            right_value: 8,
        },
        Test {
            input: "8 < 2;".to_string(),
            left_value: 8,
            operator: "<".to_string(),
            right_value: 2,
        },
        Test {
            input: "2 == 2;".to_string(),
            left_value: 2,
            operator: "==".to_string(),
            right_value: 2,
        },
        Test {
            input: "3 != 2;".to_string(),
            left_value: 3,
            operator: "!=".to_string(),
            right_value: 2,
        },
    ];

    for tt in infix_test {
        let l = Lexer::new(tt.input);
        let mut p = Parser::new(l);

        let program = p.parse_program();
        check_parser_errors(p);

        let stmt = match &program.statements[0] {
            StatementNode::ExpressionStatementNode(es) => es,
            _ => panic!("first statement is not expression statement"),
        };

        let exp = match &stmt.expression {
            ExpressionNode::InfixExpressionNode(ie) => ie,
            _ => panic!("this expression statement does not have infix expression"),
        };

        test_integer_literal(&exp.left, tt.left_value);

        assert_eq!(
            tt.operator, exp.operator,
            "unexpected operator {} (expected {})",
            exp.operator, tt.operator
        );

        test_integer_literal(&exp.right, tt.right_value);
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
