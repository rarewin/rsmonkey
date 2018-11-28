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

#[test]
fn test_next_token() {
    let input = "=+(){},;";

    let tests = [
        Token {
            token_type: TokenType::Assign,
            literal: "=".to_string(),
        },
        Token {
            token_type: TokenType::Plus,
            literal: "+".to_string(),
        },
        Token {
            token_type: TokenType::LParen,
            literal: "(".to_string(),
        },
        Token {
            token_type: TokenType::RParen,
            literal: ")".to_string(),
        },
        Token {
            token_type: TokenType::LBrace,
            literal: "{".to_string(),
        },
        Token {
            token_type: TokenType::RBrace,
            literal: "}".to_string(),
        },
        Token {
            token_type: TokenType::Comma,
            literal: ",".to_string(),
        },
        Token {
            token_type: TokenType::Semicolon,
            literal: ";".to_string(),
        },
        Token {
            token_type: TokenType::EoF,
            literal: "EOF".to_string(),
        },
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
            '=' => Token {
                token_type: TokenType::Assign,
                literal: "=".to_string(),
            },
            '+' => Token {
                token_type: TokenType::Plus,
                literal: "+".to_string(),
            },
            '(' => Token {
                token_type: TokenType::LParen,
                literal: "(".to_string(),
            },
            ')' => Token {
                token_type: TokenType::RParen,
                literal: ")".to_string(),
            },
            '{' => Token {
                token_type: TokenType::LBrace,
                literal: "{".to_string(),
            },
            '}' => Token {
                token_type: TokenType::RBrace,
                literal: "}".to_string(),
            },
            ',' => Token {
                token_type: TokenType::Comma,
                literal: ",".to_string(),
            },
            ';' => Token {
                token_type: TokenType::Semicolon,
                literal: ";".to_string(),
            },
            '\0' => Token {
                token_type: TokenType::EoF,
                literal: "EoF".to_string(),
            },
            _ => Token {
                token_type: TokenType::Ileegal,
                literal: "illegal".to_string(),
            },
        };
        self.read_char();

        return token;
    }
}

fn main() {
    println!("Hello, world!");
}
