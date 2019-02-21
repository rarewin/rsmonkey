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
            '[' => Token::new(TokenType::LBracket, "["),
            ']' => Token::new(TokenType::RBracket, "]"),
            ',' => Token::new(TokenType::Comma, ","),
            ';' => Token::new(TokenType::Semicolon, ";"),
            '\0' => Token::new(TokenType::EoF, "EOF"),
            '"' => Token::new(TokenType::StringToken, &self.read_string()),
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

    pub fn read_string(&mut self) -> String {
        let mut ret = String::new();
        self.read_char();
        while self.ch != '"' && self.read_position < self.input.len() {
            ret.push(self.ch);
            self.read_char();
        }
        return ret;
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
