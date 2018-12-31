use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

/// struct for parser
#[derive(Debug)]
pub struct Parser {
    /// lexer
    lexer: Lexer,
    /// current token
    cur_token: Token,
    /// peek token
    peek_token: Token,
    /// error string
    errors: Vec<Box<String>>,
}

impl Parser {
    /// constructor
    pub fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            lexer: l,
            cur_token: Token::new(TokenType::Illegal, ""),
            peek_token: Token::new(TokenType::Illegal, ""),
            errors: Vec::<Box<String>>::new(),
        };

        p.next_token();
        p.next_token();

        p
    }

    /// error string
    pub fn errors(&self) -> &Vec<Box<String>> {
        return &self.errors;
    }

    /// update current position
    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    /// check if a current token is `tt` or not
    pub fn cur_token_is(&mut self, tt: TokenType) -> bool {
        self.cur_token.token_type == tt
    }

    /// check if a peek token is `tt` or not
    pub fn peek_token_is(&mut self, tt: TokenType) -> bool {
        self.peek_token.token_type == tt
    }

    /// expect a specific token
    pub fn expect_peek(&mut self, tt: TokenType) -> bool {
        if self.peek_token_is(tt) {
            self.next_token();
            true
        } else {
            self.peek_error(tt);
            false
        }
    }

    /// parser Program
    pub fn parse_program(&mut self) -> Program {
        let mut program = Program::new();

        while self.cur_token.get_token_type() != TokenType::EoF {
            match self.parse_statement() {
                StatementNode::LetStatementNode(s) => {
                    program.statements.push(StatementNode::LetStatementNode(s))
                }
                StatementNode::ReturnStatementNode(s) => program
                    .statements
                    .push(StatementNode::ReturnStatementNode(s)),
                _ => {}
            }
            self.next_token();
        }

        program
    }

    /// parse statement
    pub fn parse_statement(&mut self) -> StatementNode {
        match self.cur_token.get_token_type() {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => StatementNode::Null,
        }
    }

    /// parse let statement
    pub fn parse_let_statement(&mut self) -> StatementNode {
        let mut stmt = LetStatement {
            token: self.cur_token.clone(),
            name: Identifier {
                token: Token {
                    token_type: TokenType::Ident,
                    literal: "dummy".to_string(),
                },
                value: "dummy".to_string(),
            },
        };

        if !self.expect_peek(TokenType::Ident) {
            return StatementNode::Null;
        }

        stmt.name = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.token_literal(),
        };

        if !self.expect_peek(TokenType::Assign) {
            return StatementNode::Null;
        }

        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        StatementNode::LetStatementNode(stmt)
    }

    /// parse return statement
    pub fn parse_return_statement(&mut self) -> StatementNode {
        let stmt = ReturnStatement {
            token: self.cur_token.clone(),
        };

        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        StatementNode::ReturnStatementNode(stmt)
    }

    pub fn peek_error(&mut self, t: TokenType) {
        self.errors.push(Box::new(format!(
            "expected next token to be {:?}, got {:?} instead",
            t, self.peek_token.token_type
        )));
    }
}
