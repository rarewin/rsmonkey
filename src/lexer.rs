use crate::token::{Token, TokenType};

#[derive(Debug)]
pub struct Lexer {
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
            ',' => Token::new(TokenType::Comma, ","),
            ';' => Token::new(TokenType::Semicolon, ";"),
            '\0' => Token::new(TokenType::EoF, "EOF"),
            _ => {
                if is_letter(self.ch) {
                    let p = self.position;
                    while is_letter(self.ch) {
                        self.read_char();
                    }
                    let v = &self.input[p..self.position].to_string();
                    return Token::new(lookup_ident(&v), v);
                } else if is_digit(self.ch) {
                    let p = self.position;
                    while is_digit(self.ch) {
                        self.read_char();
                    }
                    return Token::new(TokenType::Int, &self.input[p..self.position].to_string());
                } else {
                    return Token::new(TokenType::Illegal, "illegal");
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