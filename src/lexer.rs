use std::cell::Cell;

use crate::token::{Token, TokenType};

#[derive(Debug)]
pub struct Lexer {
    input: String,
    position: Cell<usize>,
    read_position: Cell<usize>,
    ch: Cell<char>,
}

impl Lexer {
    /// create new lexer
    pub fn new(input: String) -> Lexer {
        let ch = input.as_bytes()[0] as char;
        Lexer {
            input,
            position: Cell::new(0),
            read_position: Cell::new(1),
            ch: Cell::new(ch),
        }
    }

    /// get next token
    pub fn next_token(&self) -> Token {
        self.skip_whitespace();
        let token = match self.ch.get() {
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
                let p = self.read_position.get();
                self.read_char();
                while self.ch.get() != '"' && self.read_position.get() < self.input.len() {
                    self.read_char();
                }
                Token::new(TokenType::StringToken, &self.input[p..self.position.get()])
            }
            'a'...'z' | 'A'...'Z' | '_' => {
                let p = self.position.get();
                while self.ch.get().is_ascii_alphabetic() || self.ch.get() == '_' {
                    self.read_char();
                }
                let v = &self.input[p..self.position.get()];
                return Token::new(lookup_ident(v), v); // don't need to read char more
            }
            '0'...'9' => {
                let p = self.position.get();
                while self.ch.get().is_ascii_digit() {
                    self.read_char();
                }
                return Token::new(TokenType::Int, &self.input[p..self.position.get()]); // don't need to read char more
            }
            _ => Token::new(TokenType::Illegal, "illegal"),
        };
        self.read_char();

        token
    }

    /// read one character
    fn read_char(&self) {
        self.ch.set(self.peek_char());
        self.position.set(self.read_position.get());
        self.read_position.set(self.read_position.get() + 1);
    }

    /// peek the next character
    fn peek_char(&self) -> char {
        if self.read_position.get() >= self.input.len() {
            '\0'
        } else {
            self.input.as_bytes()[self.read_position.get()] as char
        }
    }

    /// skip white spaces
    fn skip_whitespace(&self) {
        while self.ch.get().is_ascii_whitespace() {
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
