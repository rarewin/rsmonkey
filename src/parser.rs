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
        &self.errors
    }

    /// update current position
    pub fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    /// check if a current token is `tt` or not
    pub fn cur_token_is(&mut self, tt: TokenType) -> bool {
        self.cur_token.get_token_type() == tt
    }

    /// check if a peek token is `tt` or not
    pub fn peek_token_is(&mut self, tt: TokenType) -> bool {
        self.peek_token.get_token_type() == tt
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
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        program
    }

    /// parse statement
    pub fn parse_statement(&mut self) -> Option<StatementNode> {
        match self.cur_token.get_token_type() {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    /// parse let statement
    pub fn parse_let_statement(&mut self) -> Option<StatementNode> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let name = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.get_literal(),
        };

        if !self.expect_peek(TokenType::Assign) {
            return None;
        }

        self.next_token();

        let value = self.parse_expression(OperationPrecedence::Lowest)?;

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(StatementNode::LetStatementNode(Box::new(LetStatement {
            token,
            name,
            value,
        })))
    }

    /// parse return statement
    pub fn parse_return_statement(&mut self) -> Option<StatementNode> {
        let token = self.cur_token.clone();

        self.next_token();

        let return_value = self.parse_expression(OperationPrecedence::Lowest)?;

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(StatementNode::ReturnStatementNode(Box::new(
            ReturnStatement {
                token,
                return_value,
            },
        )))
    }

    /// parse expression statement
    pub fn parse_expression_statement(&mut self) -> Option<StatementNode> {
        let token = self.cur_token.clone();

        let expression = self.parse_expression(OperationPrecedence::Lowest)?;

        if self.peek_token_is(TokenType::Semicolon) {
            self.next_token();
        }

        Some(StatementNode::ExpressionStatementNode(Box::new(
            ExpressionStatement { token, expression },
        )))
    }

    /// parse expression
    pub fn parse_expression(&mut self, precedence: OperationPrecedence) -> Option<ExpressionNode> {
        let mut ex = self.prefix_parse(self.cur_token.get_token_type())?;

        while !self.peek_token_is(TokenType::Semicolon) && precedence < self.peek_precedence() {
            self.next_token();

            ex = match self.infix_parse(self.cur_token.get_token_type(), ex.clone()) {
                Some(e) => e,
                None => break,
            }
        }

        Some(ex)
    }

    /// parse identifier
    pub fn parse_identifier(&mut self) -> Option<ExpressionNode> {
        let ident = Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.get_literal(),
        };

        Some(ExpressionNode::IdentifierNode(Box::new(ident)))
    }

    /// parse integer literal
    pub fn parse_integer_literal(&mut self) -> Option<ExpressionNode> {
        let lit = IntegerLiteral {
            token: self.cur_token.clone(),
            value: self
                .cur_token
                .get_literal()
                .parse()
                .expect("failed to parse as i64"),
        };

        Some(ExpressionNode::IntegerLiteralNode(Box::new(lit)))
    }

    /// parse string literal
    pub fn parse_string_literal(&mut self) -> Option<ExpressionNode> {
        let sl = StringLiteral {
            token: self.cur_token.clone(),
            value: self.cur_token.get_literal(),
        };

        Some(ExpressionNode::StringLiteralNode(Box::new(sl)))
    }

    /// parse function literal
    pub fn parse_function_literal(&mut self) -> Option<ExpressionNode> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::LParen) {
            return None;
        }

        let parameters = self.parse_function_parameters()?;

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        let body = self.parse_block_statement();

        Some(ExpressionNode::FunctionLiteralNode(Box::new(
            FunctionLiteral {
                token,
                parameters,
                body,
            },
        )))
    }

    /// parse prefix expression
    pub fn parse_prefix_expression(&mut self) -> Option<ExpressionNode> {
        let token = self.cur_token.clone();
        let operator = self.cur_token.get_literal();

        self.next_token();

        let right = self.parse_expression(OperationPrecedence::Prefix)?;

        Some(ExpressionNode::PrefixExpressionNode(Box::new(
            PrefixExpression {
                token,
                operator,
                right,
            },
        )))
    }

    /// parse boolean expression
    pub fn parse_boolean_expression(&mut self) -> Option<ExpressionNode> {
        let be = Boolean {
            token: self.cur_token.clone(),
            value: self.cur_token_is(TokenType::True),
        };

        Some(ExpressionNode::BooleanExpressionNode(Box::new(be)))
    }

    /// parse grouped expression
    pub fn parse_grouped_expression(&mut self) -> Option<ExpressionNode> {
        self.next_token();

        let exp = self.parse_expression(OperationPrecedence::Lowest)?;

        if !self.expect_peek(TokenType::RParen) {
            None
        } else {
            Some(exp)
        }
    }

    /// parse if expression
    pub fn parse_if_expression(&mut self) -> Option<ExpressionNode> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenType::LParen) {
            return None;
        }

        self.next_token();

        let condition = self.parse_expression(OperationPrecedence::Lowest)?;

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.peek_token_is(TokenType::Else) {
            self.next_token();
            if !self.expect_peek(TokenType::LBrace) {
                return None;
            }
            self.parse_block_statement()
        } else {
            None
        };

        Some(ExpressionNode::IfExpressionNode(Box::new(IfExpression {
            token,
            condition,
            consequence,
            alternative,
        })))
    }

    /// parse block statement
    pub fn parse_block_statement(&mut self) -> Option<StatementNode> {
        let mut block = BlockStatement {
            token: self.cur_token.clone(),
            statements: Vec::<StatementNode>::new(),
        };

        self.next_token();

        while !self.cur_token_is(TokenType::RBrace) && !self.cur_token_is(TokenType::EoF) {
            let stmt = self.parse_statement()?;
            block.statements.push(stmt);
            self.next_token();
        }

        Some(StatementNode::BlockStatementNode(Box::new(block)))
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
            value: self.cur_token.get_literal(),
        })));

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();

            params.push(ExpressionNode::IdentifierNode(Box::new(Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.get_literal(),
            })));
        }

        if !self.expect_peek(TokenType::RParen) {
            return None;
        }

        Some(params)
    }

    /// parse call expression
    pub fn parse_call_expression(&mut self, left: ExpressionNode) -> Option<ExpressionNode> {
        let token = self.cur_token.clone();
        let function = left;
        let arguments = self.parse_call_arguments()?;

        Some(ExpressionNode::CallExpressionNode(Box::new(
            CallExpression {
                token,
                function,
                arguments,
            },
        )))
    }

    /// parse index expression
    pub fn parse_index_expression(&mut self, left: ExpressionNode) -> Option<ExpressionNode> {
        let token = self.cur_token.clone();
        self.next_token();

        let index = self.parse_expression(OperationPrecedence::Lowest)?;

        if !self.expect_peek(TokenType::RBracket) {
            None
        } else {
            Some(ExpressionNode::IndexExpressionNode(Box::new(
                IndexExpression { token, left, index },
            )))
        }
    }

    /// parse function call arguments
    pub fn parse_call_arguments(&mut self) -> Option<Vec<ExpressionNode>> {
        let mut arguments = Vec::<ExpressionNode>::new();

        if self.peek_token_is(TokenType::RParen) {
            self.next_token();
            return Some(arguments);
        }

        self.next_token();

        let arg = self.parse_expression(OperationPrecedence::Lowest)?;
        arguments.push(arg);

        while self.peek_token_is(TokenType::Comma) {
            self.next_token();
            self.next_token();

            let arg = self.parse_expression(OperationPrecedence::Lowest)?;
            arguments.push(arg);
        }

        if !self.expect_peek(TokenType::RParen) {
            None
        } else {
            Some(arguments)
        }
    }

    /// parse infix expression
    pub fn parse_infix_expression(&mut self, left: ExpressionNode) -> Option<ExpressionNode> {
        let token = self.cur_token.clone();
        let operator = self.cur_token.get_literal();
        let precedence = self.cur_precedence();

        self.next_token();

        let right = self.parse_expression(precedence)?;

        Some(ExpressionNode::InfixExpressionNode(Box::new(
            InfixExpression {
                token,
                left,
                operator,
                right,
            },
        )))
    }

    /// parse expression list
    fn parse_expression_list(&mut self, end: TokenType) -> Vec<ExpressionNode> {
        let mut list = Vec::<ExpressionNode>::new();

        if self.peek_token_is(end) {
            self.next_token();
            return list;
        }

        self.next_token();

        if let Some(elm) = self.parse_expression(OperationPrecedence::Lowest) {
            list.push(elm);

            while self.peek_token_is(TokenType::Comma) {
                self.next_token();
                self.next_token();

                if let Some(elm) = self.parse_expression(OperationPrecedence::Lowest) {
                    list.push(elm);
                } else {
                    list.clear();
                    break;
                }
            }

            if !self.expect_peek(TokenType::RBracket) {
                list.clear();
            }
        }

        list
    }

    /// parse hash literal
    fn parse_hash_literal(&mut self) -> Vec<(ExpressionNode, ExpressionNode)> {
        let mut hash = Vec::<(ExpressionNode, ExpressionNode)>::new();

        while !self.peek_token_is(TokenType::RBrace) {
            self.next_token();

            let key = match self.parse_expression(OperationPrecedence::Lowest) {
                Some(k) => k,
                None => {
                    hash.clear();
                    break;
                }
            };

            if !self.expect_peek(TokenType::Colon) {
                hash.clear();
                break;
            }
            self.next_token();

            let value = match self.parse_expression(OperationPrecedence::Lowest) {
                Some(v) => v,
                None => {
                    hash.clear();
                    break;
                }
            };

            let p = (key, value);
            hash.push(p);

            if self.peek_token_is(TokenType::Comma) {
                self.next_token();
            }
        }

        if !self.expect_peek(TokenType::RBrace) {
            hash.clear();
        }

        hash
    }

    /// parse prefix
    fn prefix_parse(&mut self, tt: TokenType) -> Option<ExpressionNode> {
        match tt {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::StringToken => self.parse_string_literal(),
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expression(),
            TokenType::True | TokenType::False => self.parse_boolean_expression(),
            TokenType::LParen => self.parse_grouped_expression(),
            TokenType::LBracket => Some(ExpressionNode::ArrayLiteralNode(Box::new(ArrayLiteral {
                token: self.cur_token.clone(),
                elements: self.parse_expression_list(TokenType::RBracket),
            }))),
            TokenType::LBrace => Some(ExpressionNode::HashLiteralNode(Box::new(HashLiteral {
                token: self.cur_token.clone(),
                pairs: self.parse_hash_literal(),
            }))),
            TokenType::If => self.parse_if_expression(),
            TokenType::Function => self.parse_function_literal(),
            _ => {
                self.errors
                    .push(format!("unsupported by prefix_parser: {:?}", tt));
                None
            }
        }
    }

    /// parse infix
    fn infix_parse(&mut self, tt: TokenType, left: ExpressionNode) -> Option<ExpressionNode> {
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
            TokenType::LBracket => self.parse_index_expression(left),
            _ => panic!("unsupported by infix_parser: {:?}", tt),
        }
    }

    /// set error caused by peek token
    pub fn peek_error(&mut self, t: TokenType) {
        self.errors.push(format!(
            "expected next token to be {:?}, got {:?} instead",
            t,
            self.peek_token.get_token_type()
        ));
    }

    /// get precedence of the peek token
    pub fn peek_precedence(&self) -> OperationPrecedence {
        get_precedence(self.peek_token.get_token_type())
    }

    /// get precedence of the current token
    pub fn cur_precedence(&self) -> OperationPrecedence {
        get_precedence(self.cur_token.get_token_type())
    }
}

/// get precedence of the operation `tt`
fn get_precedence(tt: TokenType) -> OperationPrecedence {
    match tt {
        TokenType::LParen => OperationPrecedence::Call,
        TokenType::Eq | TokenType::NotEq => OperationPrecedence::Equals,
        TokenType::LT | TokenType::GT => OperationPrecedence::LessGreater,
        TokenType::Plus | TokenType::Minus => OperationPrecedence::Sum,
        TokenType::Slash | TokenType::Asterisk => OperationPrecedence::Product,
        TokenType::LBracket => OperationPrecedence::Index,
        _ => OperationPrecedence::Lowest,
    }
}
