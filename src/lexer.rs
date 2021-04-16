use crate::token::Token;

#[derive(Debug)]
pub struct Lexer {
    tokens: Vec<Token>,
}

impl Lexer {
    /// create new lexer
    pub fn new(input: &str) -> Lexer {
        let mut input = input.chars().peekable();
        let mut tokens = Vec::new();

        while let Some(ch) = input.next() {
            if ch.is_ascii_whitespace() {
                continue;
            }

            match ch {
                '+' => tokens.push(Token::Plus),
                '-' => tokens.push(Token::Minus),
                '*' => tokens.push(Token::Asterisk),
                '<' => tokens.push(Token::Lt),
                '>' => tokens.push(Token::Gt),
                '(' => tokens.push(Token::LParen),
                ')' => tokens.push(Token::RParen),
                '{' => tokens.push(Token::LBrace),
                '}' => tokens.push(Token::RBrace),
                '[' => tokens.push(Token::LBracket),
                ']' => tokens.push(Token::RBracket),
                ',' => tokens.push(Token::Comma),
                ';' => tokens.push(Token::Semicolon),
                ':' => tokens.push(Token::Colon),
                '/' => tokens.push(Token::Slash),
                '\0' => tokens.push(Token::EoF),
                '=' => {
                    if let Some(&'=') = input.peek() {
                        tokens.push(Token::Eq);
                        input.next(); // consume '='
                    } else {
                        tokens.push(Token::Assign);
                    }
                }
                '!' => {
                    if let Some(&'=') = input.peek() {
                        tokens.push(Token::NotEq);
                        input.next(); // consume '='
                    } else {
                        tokens.push(Token::Bang);
                    }
                }
                'a'..='z' | 'A'..='Z' | '_' => {
                    let mut v = String::from(ch);
                    while let Some(next) = input.peek() {
                        if next.is_ascii_alphabetic() || *next == '_' {
                            v.push(*next);
                            input.next();
                        } else {
                            break;
                        }
                    }
                    tokens.push(lookup_ident(&v));
                }
                '0'..='9' => {
                    let mut fig = String::from(ch);
                    while let Some(next) = input.peek() {
                        if next.is_ascii_digit() {
                            fig.push(*next);
                            input.next();
                        } else {
                            break;
                        }
                    }
                    tokens.push(Token::Int(fig.parse().unwrap())); // @todo  change unwrap()
                }
                '"' => {
                    let mut string = String::new();
                    #[allow(clippy::while_let_on_iterator)]
                    while let Some(next) = input.next() {
                        if next == '"' {
                            break;
                        } else {
                            string.push(next);
                        }
                    }
                    tokens.push(Token::String(string));
                }
                _ => unimplemented!("{:?}", ch),
            }
        }

        Lexer { tokens }
    }
}

impl IntoIterator for Lexer {
    type Item = Token;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.tokens.into_iter()
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
