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
            Node::LetStatementNode(s) => s,
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
            Node::ReturnStatementNode(_) => {}
            _ => panic!(),
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
