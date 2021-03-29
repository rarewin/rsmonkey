use crate::token::Token;

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
                    Some(Token::Eq)
                } else {
                    self.input.push(ch);
                    Some(Token::Assign)
                }
            }
            '+' => Some(Token::Plus),
            '-' => Some(Token::Minus),
            '!' => {
                let ch = self.input.pop()?;
                if ch == '=' {
                    Some(Token::NotEq)
                } else {
                    self.input.push(ch);
                    Some(Token::Bang)
                }
            }
            '/' => Some(Token::Slash),
            '*' => Some(Token::Asterisk),
            '<' => Some(Token::Lt),
            '>' => Some(Token::Gt),
            '(' => Some(Token::LParen),
            ')' => Some(Token::RParen),
            '{' => Some(Token::LBrace),
            '}' => Some(Token::RBrace),
            '[' => Some(Token::LBracket),
            ']' => Some(Token::RBracket),
            ',' => Some(Token::Comma),
            ';' => Some(Token::Semicolon),
            ':' => Some(Token::Colon),
            '\0' => Some(Token::EoF),
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
                Some(Token::StringToken(string))
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
                Some(lookup_ident(&v))
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
                Some(Token::Int(fig.parse().unwrap())) // @todo  change unwrap()
            }
            _ => None,
        }
    }
}

/// lookup identifier
fn lookup_ident(s: &str) -> Token {
    match s {
        "let" => Token::Let,
        "fn" => Token::Function,
        "if" => Token::If,
        "else" => Token::Else,
        "return" => Token::Return,
        "true" => Token::True,
        "false" => Token::False,
        _ => Token::Ident(s.into()),
    }
}
