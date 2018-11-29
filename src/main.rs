#[derive(Debug)]
struct Token {
    token_type: TokenType,
    literal: String,
}

#[derive(Debug, PartialEq)]
enum TokenType {
    Illegal,
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
        new_token!(TokenType::Assign, "="),
        new_token!(TokenType::Plus, "+"),
        new_token!(TokenType::LParen, "("),
        new_token!(TokenType::RParen, ")"),
        new_token!(TokenType::LBrace, "{"),
        new_token!(TokenType::RBrace, "}"),
        new_token!(TokenType::Comma, ","),
        new_token!(TokenType::Semicolon, ";"),
        //
        new_token!(TokenType::Let, "let"),
        new_token!(TokenType::Ident, "five"),
        new_token!(TokenType::Assign, "="),
        new_token!(TokenType::Int, "5"),
        new_token!(TokenType::Semicolon, ";"),
        //
        new_token!(TokenType::Let, "let"),
        new_token!(TokenType::Ident, "ten"),
        new_token!(TokenType::Assign, "="),
        new_token!(TokenType::Int, "10"),
        new_token!(TokenType::Semicolon, ";"),
        //
        new_token!(TokenType::Let, "let"),
        new_token!(TokenType::Ident, "add"),
        new_token!(TokenType::Assign, "="),
        new_token!(TokenType::Function, "fn"),
        new_token!(TokenType::LParen, "("),
        new_token!(TokenType::Ident, "x"),
        new_token!(TokenType::Comma, ","),
        new_token!(TokenType::Ident, "y"),
        new_token!(TokenType::RParen, ")"),
        new_token!(TokenType::LBrace, "{"),
        new_token!(TokenType::Ident, "x"),
        new_token!(TokenType::Plus, "+"),
        new_token!(TokenType::Ident, "y"),
        new_token!(TokenType::Semicolon, ";"),
        new_token!(TokenType::RBrace, "}"),
        new_token!(TokenType::Semicolon, ";"),
        //
        new_token!(TokenType::Let, "let"),
        new_token!(TokenType::Ident, "result"),
        new_token!(TokenType::Assign, "="),
        new_token!(TokenType::Ident, "add"),
        new_token!(TokenType::LParen, "("),
        new_token!(TokenType::Ident, "five"),
        new_token!(TokenType::Comma, ","),
        new_token!(TokenType::Ident, "ten"),
        new_token!(TokenType::RParen, ")"),
        new_token!(TokenType::Semicolon, ";"),
        //
        new_token!(TokenType::Bang, "!"),
        new_token!(TokenType::Minus, "-"),
        new_token!(TokenType::Slash, "/"),
        new_token!(TokenType::Asterisk, "*"),
        new_token!(TokenType::Int, "5"),
        new_token!(TokenType::Semicolon, ";"),
        //
        new_token!(TokenType::Int, "5"),
        new_token!(TokenType::LT, "<"),
        new_token!(TokenType::Int, "10"),
        new_token!(TokenType::GT, ">"),
        new_token!(TokenType::Int, "5"),
        new_token!(TokenType::Semicolon, ";"),
        //
        new_token!(TokenType::If, "if"),
        new_token!(TokenType::LParen, "("),
        new_token!(TokenType::Int, "5"),
        new_token!(TokenType::LT, "<"),
        new_token!(TokenType::Int, "10"),
        new_token!(TokenType::RParen, ")"),
        new_token!(TokenType::LBrace, "{"),
        new_token!(TokenType::Return, "return"),
        new_token!(TokenType::True, "true"),
        new_token!(TokenType::Semicolon, ";"),
        new_token!(TokenType::RBrace, "}"),
        new_token!(TokenType::Else, "else"),
        new_token!(TokenType::LBrace, "{"),
        new_token!(TokenType::Return, "return"),
        new_token!(TokenType::False, "false"),
        new_token!(TokenType::Semicolon, ";"),
        new_token!(TokenType::RBrace, "}"),
        //
        new_token!(TokenType::Int, "10"),
        new_token!(TokenType::Eq, "=="),
        new_token!(TokenType::Int, "10"),
        new_token!(TokenType::Semicolon, ";"),
        //
        new_token!(TokenType::Int, "10"),
        new_token!(TokenType::NotEq, "!="),
        new_token!(TokenType::Int, "9"),
        new_token!(TokenType::Semicolon, ";"),
        //
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
        self.skip_whitespace();
        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    new_token!(TokenType::Eq, "==")
                } else {
                    new_token!(TokenType::Assign, "=")
                }
            }
            '+' => new_token!(TokenType::Plus, "+"),
            '-' => new_token!(TokenType::Minus, "-"),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    new_token!(TokenType::NotEq, "!=")
                } else {
                    new_token!(TokenType::Bang, "!")
                }
            }
            '/' => new_token!(TokenType::Slash, "/"),
            '*' => new_token!(TokenType::Asterisk, "*"),
            '<' => new_token!(TokenType::LT, "<"),
            '>' => new_token!(TokenType::GT, ">"),
            '(' => new_token!(TokenType::LParen, "("),
            ')' => new_token!(TokenType::RParen, ")"),
            '{' => new_token!(TokenType::LBrace, "{"),
            '}' => new_token!(TokenType::RBrace, "}"),
            ',' => new_token!(TokenType::Comma, ","),
            ';' => new_token!(TokenType::Semicolon, ";"),
            '\0' => new_token!(TokenType::EoF, "EOF"),
            _ => {
                if is_letter(self.ch) {
                    let p = self.position;
                    while is_letter(self.ch) {
                        self.read_char();
                    }
                    let v = &self.input[p..self.position].to_string();
                    return new_token!(lookup_ident(&v), v);
                } else if is_digit(self.ch) {
                    let p = self.position;
                    while is_digit(self.ch) {
                        self.read_char();
                    }
                    return new_token!(TokenType::Int, &self.input[p..self.position].to_string());
                } else {
                    return new_token!(TokenType::Illegal, "illegal");
                }
            }
        };
        self.read_char();

        return token;
    }

    pub fn peek_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            return '\0';
        } else {
            return self.input.as_bytes()[self.read_position] as char;
        }
    }

    pub fn skip_whitespace(&mut self) {
        while self.ch == ' ' || self.ch == '\t' || self.ch == '\n' || self.ch == '\r' {
            self.read_char();
        }
    }
}

/// Return true if ch is letter.
fn is_letter(ch: char) -> bool {
    return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_';
}

/// Retutn true if ch is digit.
fn is_digit(ch: char) -> bool {
    return '0' <= ch && ch <= '9';
}

fn lookup_ident(s: &str) -> TokenType {
    let t = s.to_string();

    if t == "let" {
        TokenType::Let
    } else if t == "fn" {
        TokenType::Function
    } else if t == "if" {
        TokenType::If
    } else if t == "else" {
        TokenType::Else
    } else if t == "return" {
        TokenType::Return
    } else if t == "true" {
        TokenType::True
    } else if t == "false" {
        TokenType::False
    } else {
        TokenType::Ident
    }
}

fn main() {
    println!("Hello, world!");
}
