#[derive(Debug)]
struct Token {
    token_type: TokenType,
    literal: String,
}

#[derive(Debug, PartialEq)]
enum TokenType {
    Ileegal,
    EoF,

    Ident,
    Int,

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    LT,
    GT,

    Eq,
    NotEq,

    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

macro_rules! new_token {
    ($tt:expr, $l:expr) => {
        Token {
            token_type: $tt,
            literal: $l.to_string(),
        }
    };
}

#[test]
fn test_next_token() {
    let input = r##"=+(){},;"##;

    let tests = [
        new_token!(TokenType::Assign, "="),
        new_token!(TokenType::Plus, "+"),
        new_token!(TokenType::LParen, "("),
        new_token!(TokenType::RParen, ")"),
        new_token!(TokenType::LBrace, "{"),
        new_token!(TokenType::RBrace, "}"),
        new_token!(TokenType::Comma, ","),
        new_token!(TokenType::Semicolon, ";"),
        new_token!(TokenType::EoF, "EOF"),
    ];

    let mut l = Lexer::new(input.to_string());

    for tp in tests.iter() {
        let tok = l.next_token();
        assert_eq!(tok.token_type, tp.token_type);
        assert_eq!(tok.literal, tok.literal);
    }
}

struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input: input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        l.read_char();

        return l;
    }

    pub fn read_char(&mut self) {
        self.ch = if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.as_bytes()[self.read_position] as char
        };

        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        let token = match self.ch {
            '=' => new_token!(TokenType::Assign, "="),
            '+' => new_token!(TokenType::Plus, "+"),
            '(' => new_token!(TokenType::LParen, "("),
            ')' => new_token!(TokenType::RParen, ")"),
            '{' => new_token!(TokenType::LBrace, "{"),
            '}' => new_token!(TokenType::RBrace, "}"),
            ',' => new_token!(TokenType::Comma, ","),
            ';' => new_token!(TokenType::Semicolon, ";"),
            '\0' => new_token!(TokenType::EoF, "EOF"),
            _ => new_token!(TokenType::Ileegal, "illegal"),
        };
        self.read_char();

        return token;
    }
}

fn main() {
    println!("Hello, world!");
}
