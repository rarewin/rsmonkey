use crate::token::{Token, TokenType};

#[derive(Debug)]
pub struct Lexer {
    input: Vec<char>,
}

impl Lexer {
    /// create new lexer
    pub fn new(input: String) -> Lexer {
        let mut input = input.chars().collect::<Vec<char>>();
        input.reverse();
        Lexer { input }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    /// get next token
    fn next(&mut self) -> Option<Self::Item> {
        let mut ch = self.input.pop()?;

        while ch.is_ascii_whitespace() {
            ch = self.input.pop()?;
        }

        match ch {
            '=' => {
                let ch = self.input.pop()?;
                if ch == '=' {
                    Some(Token::new(TokenType::Eq, "=="))
                } else {
                    self.input.push(ch);
                    Some(Token::new(TokenType::Assign, "="))
                }
            }
            '+' => Some(Token::new(TokenType::Plus, "+")),
            '-' => Some(Token::new(TokenType::Minus, "-")),
            '!' => {
                let ch = self.input.pop()?;
                if ch == '=' {
                    Some(Token::new(TokenType::NotEq, "!="))
                } else {
                    self.input.push(ch);
                    Some(Token::new(TokenType::Bang, "!"))
                }
            }
            '/' => Some(Token::new(TokenType::Slash, "/")),
            '*' => Some(Token::new(TokenType::Asterisk, "*")),
            '<' => Some(Token::new(TokenType::LT, "<")),
            '>' => Some(Token::new(TokenType::GT, ">")),
            '(' => Some(Token::new(TokenType::LParen, "(")),
            ')' => Some(Token::new(TokenType::RParen, ")")),
            '{' => Some(Token::new(TokenType::LBrace, "{")),
            '}' => Some(Token::new(TokenType::RBrace, "}")),
            '[' => Some(Token::new(TokenType::LBracket, "[")),
            ']' => Some(Token::new(TokenType::RBracket, "]")),
            ',' => Some(Token::new(TokenType::Comma, ",")),
            ';' => Some(Token::new(TokenType::Semicolon, ";")),
            ':' => Some(Token::new(TokenType::Colon, ":")),
            '\0' => Some(Token::new(TokenType::EoF, "EOF")),
            '"' => {
                let string = self
                    .input
                    .iter()
                    .rev()
                    .take_while(|c| **c != '"')
                    .collect::<String>();
                for _ in 0..(string.len() + 1) {
                    self.input.pop();
                }
                Some(Token::new(TokenType::StringToken, &string))
            }
            'a'..='z' | 'A'..='Z' | '_' => {
                self.input.push(ch);
                let v = self
                    .input
                    .iter()
                    .rev()
                    .take_while(|c| c.is_ascii_alphabetic() || **c == '_')
                    .collect::<String>();
                for _ in 0..v.len() {
                    self.input.pop();
                }
                Some(Token::new(lookup_ident(&v), &v))
            }
            '0'..='9' => {
                self.input.push(ch);
                let fig = self
                    .input
                    .iter()
                    .rev()
                    .take_while(|c| c.is_ascii_digit())
                    .collect::<String>();
                for _ in 0..fig.len() {
                    self.input.pop();
                }
                Some(Token::new(TokenType::Int, &fig))
            }
            _ => None,
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
