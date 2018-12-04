use crate::ast::Program;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

/// struct for parser
pub struct Parser {
    /// lexer
    lexer: Lexer,
    /// current token
    cur_token: Token,
    /// peek token
    peek_token: Token,
}

impl Parser {
    /// constructor
    pub fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            lexer: l,
            cur_token: Token::new(TokenType::Illegal, ""),
            peek_token: Token::new(TokenType::Illegal, ""),
        };

        p.next_token();
        p.next_token();

        p
    }

    /// update current position
    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    /// parser Program
    pub fn parse_program(&mut self) -> Program {
        Program::new()
    }
}
