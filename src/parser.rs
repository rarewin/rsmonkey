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
    errors: Vec<String>,
}

impl Parser {
    /// constructor
    pub fn new(l: Lexer) -> Parser {
        let mut p = Parser {
            lexer: l,
            cur_token: Token::new(TokenType::Illegal, ""),
            peek_token: Token::new(TokenType::Illegal, ""),
            errors: Vec::<String>::new(),
        };

        p.next_token();
        p.next_token();

        p
    }

    /// error string
    pub fn errors(&self) -> &Vec<String> {
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
            program.statements.push(self.parse_statement());
            self.next_token();
        }

        program
    }

    /// parse statement
    pub fn parse_statement(&mut self) -> StatementNode {
        match self.cur_token.get_token_type() {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
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
            value: ExpressionNode::Null,
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

        return StatementNode::LetStatementNode(Box::new(stmt));
    }

    /// parse return statement
    pub fn parse_return_statement(&mut self) -> StatementNode {
        let stmt = ReturnStatement {
            token: self.cur_token.clone(),
            return_value: ExpressionNode::Null,
        };

        while !self.cur_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        return StatementNode::ReturnStatementNode(Box::new(stmt));
    }

    /// parse expression statement
    pub fn parse_expression_statement(&mut self) -> StatementNode {
        let stmt = ExpressionStatement {
            token: self.cur_token.clone(),
            expression: self.parse_expression(OperationPrecedence::Lowest),
        };

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        return StatementNode::ExpressionStatementNode(Box::new(stmt));
    }

    /// parse expression
    pub fn parse_expression(&mut self, precedence: OperationPrecedence) -> ExpressionNode {
        let mut ex = self.prefix_parse(self.cur_token.token_type.clone());

        if let ExpressionNode::Null = ex {
            return ex;
        }

        while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
            self.next_token();
            ex = self.infix_parse(self.cur_token.token_type, ex);
        }

        return ex;
    }

    /// parse identifier
    pub fn parse_identifier(&mut self) -> ExpressionNode {
        let ident = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        return ExpressionNode::IdentifierNode(Box::new(ident));
    }

    /// parse integer literal
    pub fn parse_integer_literal(&mut self) -> ExpressionNode {
        let lit = IntegerLiteral {
            token: self.cur_token.clone(),
            value: self
                .cur_token
                .literal
                .parse()
                .expect("failed to parse as i64"),
        };

        return ExpressionNode::IntegerLiteralNode(Box::new(lit));
    }

    /// parse prefix expression
    pub fn parse_prefix_expression(&mut self) -> ExpressionNode {
        let mut pe = PrefixExpression {
            token: self.cur_token.clone(),
            operator: self.cur_token.token_literal(),
            right: ExpressionNode::Null,
        };

        self.next_token();

        pe.right = self.parse_expression(OperationPrecedence::Prefix);

        return ExpressionNode::PrefixExpressionNode(Box::new(pe));
    }

    /// parse infix expression
    pub fn parse_infix_expression(&mut self, left: ExpressionNode) -> ExpressionNode {
        let mut ie = InfixExpression {
            token: self.cur_token.clone(),
            left: left,
            operator: self.cur_token.token_literal(),
            right: ExpressionNode::Null,
        };

        let precedence = self.cur_precedence();
        self.next_token();
        ie.right = self.parse_expression(precedence);

        return ExpressionNode::InfixExpressionNode(Box::new(ie));
    }

    /// parse prefix
    fn prefix_parse(&mut self, tt: TokenType) -> ExpressionNode {
        match tt {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::Bang => self.parse_prefix_expression(),
            TokenType::Minus => self.parse_prefix_expression(),
            _ => ExpressionNode::Null, // panic!("unsupported by prefix_parser: {:?}", tt),
        }
    }

    /// parse infix
    fn infix_parse(&mut self, tt: TokenType, left: ExpressionNode) -> ExpressionNode {
        match tt {
            TokenType::Plus => self.parse_infix_expression(left),
            TokenType::Minus => self.parse_infix_expression(left),
            TokenType::Asterisk => self.parse_infix_expression(left),
            TokenType::Slash => self.parse_infix_expression(left),
            TokenType::GT => self.parse_infix_expression(left),
            TokenType::LT => self.parse_infix_expression(left),
            TokenType::Eq => self.parse_infix_expression(left),
            TokenType::NotEq => self.parse_infix_expression(left),
            _ => panic!("unsupported by infix_parser: {:?}", tt),
        }
    }

    /// set error caused by peek token
    pub fn peek_error(&mut self, t: TokenType) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            t, self.peek_token.token_type
        ));
    }

    /// get precedence of the peek token
    pub fn peek_precedence(&self) -> OperationPrecedence {
        return get_precedence(&self.peek_token.token_type);
    }

    /// get precedence of the current token
    pub fn cur_precedence(&self) -> OperationPrecedence {
        return get_precedence(&self.cur_token.token_type);
    }
}

/// get precedence of the operation `tt`
fn get_precedence(tt: &TokenType) -> OperationPrecedence {
    match tt {
        TokenType::Eq => OperationPrecedence::Equals,
        TokenType::NotEq => OperationPrecedence::Equals,
        TokenType::LT => OperationPrecedence::LessGreater,
        TokenType::GT => OperationPrecedence::LessGreater,
        TokenType::Plus => OperationPrecedence::Sum,
        TokenType::Minus => OperationPrecedence::Sum,
        TokenType::Slash => OperationPrecedence::Product,
        TokenType::Asterisk => OperationPrecedence::Product,
        _ => OperationPrecedence::Lowest,
    }
}
