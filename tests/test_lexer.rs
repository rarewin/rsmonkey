use rsmonkey::lexer::Lexer;
use rsmonkey::token::Token;

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
    "foo bar"

    [1, 2];

    {"foo": "bar"}"##;

    let tests = [
        Token::Assign,
        Token::Plus,
        Token::LParen,
        Token::RParen,
        Token::LBrace,
        Token::RBrace,
        Token::Comma,
        Token::Semicolon,
        //
        Token::Let,
        Token::Ident("five".into()),
        Token::Assign,
        Token::Int(5),
        Token::Semicolon,
        //
        Token::Let,
        Token::Ident("ten".into()),
        Token::Assign,
        Token::Int(10),
        Token::Semicolon,
        //
        Token::Let,
        Token::Ident("add".into()),
        Token::Assign,
        Token::Function,
        Token::LParen,
        Token::Ident("x".into()),
        Token::Comma,
        Token::Ident("y".into()),
        Token::RParen,
        Token::LBrace,
        Token::Ident("x".into()),
        Token::Plus,
        Token::Ident("y".into()),
        Token::Semicolon,
        Token::RBrace,
        Token::Semicolon,
        //
        Token::Let,
        Token::Ident("result".into()),
        Token::Assign,
        Token::Ident("add".into()),
        Token::LParen,
        Token::Ident("five".into()),
        Token::Comma,
        Token::Ident("ten".into()),
        Token::RParen,
        Token::Semicolon,
        //
        Token::Bang,
        Token::Minus,
        Token::Slash,
        Token::Asterisk,
        Token::Int(5),
        Token::Semicolon,
        //
        Token::Int(5),
        Token::Lt,
        Token::Int(10),
        Token::Gt,
        Token::Int(5),
        Token::Semicolon,
        //
        Token::If,
        Token::LParen,
        Token::Int(5),
        Token::Lt,
        Token::Int(10),
        Token::RParen,
        Token::LBrace,
        Token::Return,
        Token::True,
        Token::Semicolon,
        Token::RBrace,
        Token::Else,
        Token::LBrace,
        Token::Return,
        Token::False,
        Token::Semicolon,
        Token::RBrace,
        //
        Token::Int(10),
        Token::Eq,
        Token::Int(10),
        Token::Semicolon,
        //
        Token::Int(10),
        Token::NotEq,
        Token::Int(9),
        Token::Semicolon,
        Token::String("foobar".into()),
        Token::String("foo bar".into()),
        //
        Token::LBracket,
        Token::Int(1),
        Token::Comma,
        Token::Int(2),
        Token::RBracket,
        Token::Semicolon,
        //
        Token::LBrace,
        Token::String("foo".into()),
        Token::Colon,
        Token::String("bar".into()),
        Token::RBrace,
    ];

    let mut l = Lexer::new(input).into_iter();

    for tp in tests.iter() {
        let tok = l.next().unwrap();
        assert_eq!(&tok, tp, "{:?}", tok);
    }

    assert_eq!(l.next(), None);
}

#[test]
fn test_empty_input() {
    let input = "";

    let mut l = Lexer::new(input).into_iter();

    assert!(l.next().is_none());
}
