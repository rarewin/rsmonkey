use rsmonkey::lexer::Lexer;
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
    10 != 9;

    "foobar"
    "foo bar""##;

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
        Token::new(TokenType::StringToken, "foobar"),
        Token::new(TokenType::StringToken, "foo bar"),
        //
        Token::new(TokenType::EoF, "EOF"),
    ];

    let mut l = Lexer::new(input.to_string());

    for tp in tests.iter() {
        let tok = l.next_token();
        assert_eq!(&tok, tp);
    }
}
