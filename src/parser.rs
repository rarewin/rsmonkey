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
            let stmt = self.parse_statement();

            if let StatementNode::Null = stmt {
            } else {
                program.statements.push(stmt);
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
            _ => self.parse_expression_statement(),
        }
    }

    /// parse let statement
    pub fn parse_let_statement(&mut self) -> StatementNode {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::Ident) {
            return StatementNode::Null;
        }

        let name = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.token_literal(),
        };

        if !self.expect_peek(TokenType::Assign) {
            return StatementNode::Null;
        }

        self.next_token();

        let value = self.parse_expression(OperationPrecedence::Lowest);

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        return StatementNode::LetStatementNode(Box::new(LetStatement { token, name, value }));
    }

    /// parse return statement
    pub fn parse_return_statement(&mut self) -> StatementNode {
        let token = self.cur_token.clone();

        self.next_token();

        let return_value = self.parse_expression(OperationPrecedence::Lowest);

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        return StatementNode::ReturnStatementNode(Box::new(ReturnStatement {
            token,
            return_value,
        }));
    }

    /// parse expression statement
    pub fn parse_expression_statement(&mut self) -> StatementNode {
        let stmt = ExpressionStatement {
            token: self.cur_token.clone(),
            expression: self.parse_expression(OperationPrecedence::Lowest),
        };

        if let ExpressionNode::Null = stmt.expression {
            return StatementNode::Null;
        }

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

    /// parse string literal
    pub fn parse_string_literal(&mut self) -> ExpressionNode {
        let sl = StringLiteral {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        };

        return ExpressionNode::StringLiteralNode(Box::new(sl));
    }

    /// parse function literal
    pub fn parse_function_literal(&mut self) -> ExpressionNode {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::LParen) {
            return ExpressionNode::Null;
        }

        let parameters = match self.parse_function_parameters() {
            Some(p) => p,
            _ => return ExpressionNode::Null,
        };

        if !self.expect_peek(TokenType::LBrace) {
            return ExpressionNode::Null;
        }

        let body = self.parse_block_statement();

        return ExpressionNode::FunctionLiteralNode(Box::new(FunctionLiteral {
            token,
            parameters,
            body,
        }));
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

    /// parse boolean expression
    pub fn parse_boolean_expression(&mut self) -> ExpressionNode {
        let be = Boolean {
            token: self.cur_token.clone(),
            value: self.cur_token_is(TokenType::True),
        };

        return ExpressionNode::BooleanExpressionNode(Box::new(be));
    }

    /// parse grouped expression
    pub fn parse_grouped_expression(&mut self) -> ExpressionNode {
        self.next_token();

        let exp = self.parse_expression(OperationPrecedence::Lowest);

        if !self.expect_peek(TokenType::RParen) {
            return ExpressionNode::Null;
        }

        return exp;
    }

    /// parse if expression
    pub fn parse_if_expression(&mut self) -> ExpressionNode {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::LParen) {
            return ExpressionNode::Null;
        }

        self.next_token();

        let condition = self.parse_expression(OperationPrecedence::Lowest);

        if !self.expect_peek(TokenType::RParen) {
            return ExpressionNode::Null;
        }

        if !self.expect_peek(TokenType::LBrace) {
            return ExpressionNode::Null;
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token_is(TokenType::Else) {
            self.next_token();

            if !self.expect_peek(TokenType::LBrace) {
                return ExpressionNode::Null;
            }
            self.parse_block_statement()
        } else {
            StatementNode::Null
        };

        return ExpressionNode::IfExpressionNode(Box::new(IfExpression {
            token,
            condition,
            consequence,
            alternative,
        }));
    }

    /// parse block statement
    pub fn parse_block_statement(&mut self) -> StatementNode {
        let mut block = BlockStatement {
            token: self.cur_token.clone(),
            statements: Vec::<StatementNode>::new(),
        };

        self.next_token();

        while !self.cur_token_is(TokenType::RBrace) && !self.cur_token_is(TokenType::EoF) {
            let stmt = self.parse_statement();

            if let StatementNode::Null = stmt {
                return StatementNode::Null;
            }

            block.statements.push(stmt);
            self.next_token();
        }

        return StatementNode::BlockStatementNode(Box::new(block));
    }

    /// parse function parameters
    pub fn parse_function_parameters(&mut self) -> Option<Vec<ExpressionNode>> {
        let mut params = Vec::<ExpressionNode>::new();

        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return Some(params);
        }

        self.next_token();

        params.push(ExpressionNode::IdentifierNode(Box::new(Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        })));

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();

            params.push(ExpressionNode::IdentifierNode(Box::new(Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            })));
        }

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        return Some(params);
    }

    /// parse call expression
    pub fn parse_call_expression(&mut self, left: ExpressionNode) -> ExpressionNode {
        let token = self.cur_token.clone();
        let function = left;

        let arguments = match self.parse_call_arguments() {
            Some(a) => a,
            _ => return ExpressionNode::Null,
        };

        return ExpressionNode::CallExpressionNode(Box::new(CallExpression {
            token,
            function,
            arguments,
        }));
    }

    /// parse function call arguments
    pub fn parse_call_arguments(&mut self) -> Option<Vec<ExpressionNode>> {
        let mut arguments = Vec::<ExpressionNode>::new();

        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return Some(arguments);
        };

        self.next_token();

        arguments.push(self.parse_expression(OperationPrecedence::Lowest));

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();
            arguments.push(self.parse_expression(OperationPrecedence::Lowest));
        }

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        return Some(arguments);
    }

    /// parse infix expression
    pub fn parse_infix_expression(&mut self, left: ExpressionNode) -> ExpressionNode {
        let token = self.cur_token.clone();
        let operator = self.cur_token.token_literal();
        let precedence = self.cur_precedence();

        self.next_token();

        let right = self.parse_expression(precedence);

        return ExpressionNode::InfixExpressionNode(Box::new(InfixExpression {
            token,
            left,
            operator,
            right,
        }));
    }

    /// parse prefix
    fn prefix_parse(&mut self, tt: TokenType) -> ExpressionNode {
        match tt {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::StringToken => self.parse_string_literal(),
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expression(),
            TokenType::True | TokenType::False => self.parse_boolean_expression(),
            TokenType::LParen => self.parse_grouped_expression(),
            TokenType::If => self.parse_if_expression(),
            TokenType::Function => self.parse_function_literal(),
            _ => {
                self.errors
                    .push(format!("unsupported by prefix_parser: {:?}", tt));
                ExpressionNode::Null
            }
        }
    }

    /// parse infix
    fn infix_parse(&mut self, tt: TokenType, left: ExpressionNode) -> ExpressionNode {
        match tt {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Asterisk
            | TokenType::Slash
            | TokenType::GT
            | TokenType::LT
            | TokenType::Eq
            | TokenType::NotEq => self.parse_infix_expression(left),
            TokenType::LParen => self.parse_call_expression(left),
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
        TokenType::LParen => OperationPrecedence::Call,
        TokenType::Eq | TokenType::NotEq => OperationPrecedence::Equals,
        TokenType::LT | TokenType::GT => OperationPrecedence::LessGreater,
        TokenType::Plus | TokenType::Minus => OperationPrecedence::Sum,
        TokenType::Slash | TokenType::Asterisk => OperationPrecedence::Product,
        _ => OperationPrecedence::Lowest,
    }
}
