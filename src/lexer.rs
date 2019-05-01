use crate::token::{Token, TokenType};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: usize,
    read_position: usize,
    ch: char,
}

impl Lexer {
    /// create new lexer
    pub fn new(input: String) -> Lexer {
        let ch = input.as_bytes()[0] as char;
        Lexer {
            input,
            position: 0,
            read_position: 1,
            ch,
        }
    }

    /// get next token
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::Eq, "==")
                } else {
                    Token::new(TokenType::Assign, "=")
                }
            }
            '+' => Token::new(TokenType::Plus, "+"),
            '-' => Token::new(TokenType::Minus, "-"),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new(TokenType::NotEq, "!=")
                } else {
                    Token::new(TokenType::Bang, "!")
                }
            }
            '/' => Token::new(TokenType::Slash, "/"),
            '*' => Token::new(TokenType::Asterisk, "*"),
            '<' => Token::new(TokenType::LT, "<"),
            '>' => Token::new(TokenType::GT, ">"),
            '(' => Token::new(TokenType::LParen, "("),
            ')' => Token::new(TokenType::RParen, ")"),
            '{' => Token::new(TokenType::LBrace, "{"),
            '}' => Token::new(TokenType::RBrace, "}"),
            '[' => Token::new(TokenType::LBracket, "["),
            ']' => Token::new(TokenType::RBracket, "]"),
            ',' => Token::new(TokenType::Comma, ","),
            ';' => Token::new(TokenType::Semicolon, ";"),
            '\0' => Token::new(TokenType::EoF, "EOF"),
            '"' => {
                let p = self.read_position;
                self.read_char();
                while self.ch != '"' && self.read_position < self.input.len() {
                    self.read_char();
                }
                Token::new(TokenType::StringToken, &self.input[p..self.position])
            }
            'a'...'z' | 'A'...'Z' | '_' => {
                let p = self.position;
                while self.ch.is_ascii_alphabetic() || self.ch == '_' {
                    self.read_char();
                }
                let v = &self.input[p..self.position];
                return Token::new(lookup_ident(v), v); // don't need to read char more
            }
            '0'...'9' => {
                let p = self.position;
                while self.ch.is_ascii_digit() {
                    self.read_char();
                }
                return Token::new(TokenType::Int, &self.input[p..self.position]); // don't need to read char more
            }
            _ => Token::new(TokenType::Illegal, "illegal"),
        };
        self.read_char();

        token
    }

    fn read_char(&mut self) {
        self.ch = self.peek_char();

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn peek_char(&mut self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input.as_bytes()[self.read_position] as char
        }
    }

    /// skip white spaces
    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }
}

/// lookup identifier
fn lookup_ident(s: &str) -> TokenType {
    match s {
        "let" => TokenType::Let,
        "fn" => TokenType::Function,
        "if" => TokenType::If,
        "else" => TokenType::Else,
        "return" => TokenType::Return,
        "true" => TokenType::True,
        "false" => TokenType::False,
        _ => TokenType::Ident,
    }
}
