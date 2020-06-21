use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

use anyhow::Result;

/// struct for parser
#[derive(Debug)]
pub struct Parser {
    /// tokens
    tokens: Vec<Token>,
    /// error string
    errors: Vec<String>,
}

impl Parser {
    /// constructor
    pub fn new(l: Lexer) -> Parser {
        let mut tokens = l.collect::<Vec<Token>>();
        tokens.reverse();
        Parser {
            tokens,
            errors: Vec::new(),
        }
    }

    /// parser Program
    pub fn parse_program(&mut self) -> Result<Program> {
        let mut program = Program::new();

        while self.tokens.last() != None {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            } else {
                panic!("{:#?}", self.tokens.pop());
            }
        }

        Ok(program)
    }

    /// parse statement
    pub fn parse_statement(&mut self) -> Option<StatementNode> {
        let token = self.tokens.pop()?;

        match &token.token_type {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => {
                self.tokens.push(token);
                self.parse_expression_statement()
            }
        }
    }

    /// parse let statement
    pub fn parse_let_statement(&mut self) -> Option<StatementNode> {
        if self.tokens.last()?.token_type != TokenType::Ident {
            return None;
        }

        let token = self.tokens.pop()?;

        let name = Identifier {
            token: token.clone(),
            value: token.get_literal(),
        };

        if self.tokens.last()?.token_type != TokenType::Assign {
            return None;
        }
        self.tokens.pop();

        let value = self.parse_expression(OperationPrecedence::Lowest)?;

        if !self.tokens.is_empty() && self.tokens.last()?.token_type == TokenType::Semicolon {
            self.tokens.pop();
        }

        Some(StatementNode::LetStatementNode(Box::new(LetStatement {
            token,
            name,
            value,
        })))
    }

    /// parse return statement
    pub fn parse_return_statement(&mut self) -> Option<StatementNode> {
        let token = self.tokens.last()?.clone();
        let return_value = self.parse_expression(OperationPrecedence::Lowest)?;

        if !self.tokens.is_empty() && self.tokens.last()?.token_type == TokenType::Semicolon {
            self.tokens.pop();
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
        let token = self.tokens.last()?.clone();
        let expression = self.parse_expression(OperationPrecedence::Lowest)?;

        if !self.tokens.is_empty() && self.tokens.last()?.token_type == TokenType::Semicolon {
            self.tokens.pop();
        }

        Some(StatementNode::ExpressionStatementNode(Box::new(
            ExpressionStatement { token, expression },
        )))
    }

    /// parse expression
    pub fn parse_expression(&mut self, precedence: OperationPrecedence) -> Option<ExpressionNode> {
        let mut ex = self.prefix_parse(self.tokens.last()?.token_type)?;

        while !self.tokens.is_empty()
            && self.tokens.last()?.token_type != TokenType::Semicolon
            && precedence < get_precedence(self.tokens.last()?.token_type)
        {
            let tt = self.tokens.last()?.token_type;
            ex = match self.infix_parse(tt, ex.clone()) {
                Some(e) => e,
                None => break,
            };
        }

        Some(ex)
    }

    /// parse identifier
    pub fn parse_identifier(&mut self) -> Option<ExpressionNode> {
        let token = self.tokens.pop()?;

        Some(ExpressionNode::IdentifierNode(Box::new(Identifier {
            token: token.clone(),
            value: token.get_literal(),
        })))
    }

    /// parse integer literal
    pub fn parse_integer_literal(&mut self) -> Option<ExpressionNode> {
        let token = self.tokens.pop()?;

        Some(ExpressionNode::IntegerLiteralNode(Box::new(
            IntegerLiteral {
                token: token.clone(),
                value: token.get_literal().parse().expect("failed to parse as i64"),
            },
        )))
    }

    /// parse string literal
    pub fn parse_string_literal(&mut self) -> Option<ExpressionNode> {
        let token = self.tokens.pop()?;

        Some(ExpressionNode::StringLiteralNode(Box::new(StringLiteral {
            token: token.clone(),
            value: token.get_literal(),
        })))
    }

    /// parse function literal
    pub fn parse_function_literal(&mut self) -> Option<ExpressionNode> {
        let token = self.tokens.pop()?;

        if self.tokens.last()?.token_type != TokenType::LParen {
            return None;
        }

        let parameters = self.parse_function_parameters()?;

        if self.tokens.last()?.token_type != TokenType::LBrace {
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
        let token = self.tokens.pop()?;
        let operator = token.get_literal();

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
        let token = self.tokens.pop()?;

        Some(ExpressionNode::BooleanExpressionNode(Box::new(Boolean {
            token: token.clone(),
            value: token.token_type == TokenType::True,
        })))
    }

    /// parse grouped expression
    pub fn parse_grouped_expression(&mut self) -> Option<ExpressionNode> {
        self.tokens.pop(); // drop LParen

        let exp = self.parse_expression(OperationPrecedence::Lowest)?;

        if self.tokens.pop()?.token_type != TokenType::RParen {
            None
        } else {
            Some(exp)
        }
    }

    /// parse if expression
    pub fn parse_if_expression(&mut self) -> Option<ExpressionNode> {
        let token = self.tokens.pop()?; // drop 'if'

        if self.tokens.last()?.token_type != TokenType::LParen {
            return None;
        }

        let condition = self.parse_expression(OperationPrecedence::Lowest)?;

        // RParen has been dropped at grouped_expression

        if self.tokens.last()?.token_type != TokenType::LBrace {
            return None;
        }

        let consequence = self.parse_block_statement();

        let alternative = if self.tokens.last()?.token_type == TokenType::Else {
            self.tokens.pop();
            if self.tokens.last()?.token_type != TokenType::LBrace {
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
            token: self.tokens.pop()?,
            statements: Vec::new(),
        };

        while self.tokens.last()?.token_type != TokenType::RBrace
            && self.tokens.last()?.token_type != TokenType::EoF
        {
            let stmt = self.parse_statement()?;
            block.statements.push(stmt);
        }

        if self.tokens.pop()?.token_type != TokenType::RBrace {
            None
        } else {
            Some(StatementNode::BlockStatementNode(Box::new(block)))
        }
    }

    /// parse function parameters
    pub fn parse_function_parameters(&mut self) -> Option<Vec<ExpressionNode>> {
        let mut params = Vec::<ExpressionNode>::new();

        self.tokens.pop(); // drop LParen

        if self.tokens.last()?.token_type == TokenType::RParen {
            self.tokens.pop();
            return Some(params);
        }

        let token = self.tokens.pop()?;

        params.push(ExpressionNode::IdentifierNode(Box::new(Identifier {
            token: token.clone(),
            value: token.get_literal(),
        })));

        while self.tokens.last()?.token_type == TokenType::Comma {
            self.tokens.pop();

            let t = self.tokens.pop()?;

            params.push(ExpressionNode::IdentifierNode(Box::new(Identifier {
                token: t.clone(),
                value: t.get_literal(),
            })));
        }

        if self.tokens.pop()?.token_type != TokenType::RParen {
            None
        } else {
            Some(params)
        }
    }

    /// parse call expression
    pub fn parse_call_expression(&mut self, left: ExpressionNode) -> Option<ExpressionNode> {
        let token = self.tokens.pop()?;
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
        let token = self.tokens.pop()?;

        let index = self.parse_expression(OperationPrecedence::Lowest)?;

        if self.tokens.pop()?.token_type != TokenType::RBracket {
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

        if self.tokens.last()?.token_type == TokenType::RParen {
            self.tokens.pop();
            return Some(arguments);
        }

        let arg = self.parse_expression(OperationPrecedence::Lowest)?;
        arguments.push(arg);

        while self.tokens.last()?.token_type == TokenType::Comma {
            self.tokens.pop();
            let arg = self.parse_expression(OperationPrecedence::Lowest)?;
            arguments.push(arg);
        }

        if self.tokens.pop()?.token_type == TokenType::RParen {
            Some(arguments)
        } else {
            None
        }
    }

    /// parse infix expression
    pub fn parse_infix_expression(&mut self, left: ExpressionNode) -> Option<ExpressionNode> {
        let token = self.tokens.last()?.clone();
        let operator = self.tokens.last()?.get_literal();
        let precedence = get_precedence(token.token_type);

        self.tokens.pop();

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
    fn parse_expression_list(&mut self, end: TokenType) -> Option<Vec<ExpressionNode>> {
        let mut list = Vec::<ExpressionNode>::new();

        if self.tokens.last()?.token_type == end {
            self.tokens.pop();
            return Some(list);
        }

        if let Some(elm) = self.parse_expression(OperationPrecedence::Lowest) {
            list.push(elm);

            while self.tokens.last()?.token_type == TokenType::Comma {
                self.tokens.pop();

                if let Some(elm) = self.parse_expression(OperationPrecedence::Lowest) {
                    list.push(elm);
                } else {
                    list.clear();
                    break;
                }
            }

            if self.tokens.pop()?.token_type != TokenType::RBracket {
                list.clear();
            }
        }

        Some(list)
    }

    /// parse hash literal
    fn parse_hash_literal(&mut self) -> Option<Vec<(ExpressionNode, ExpressionNode)>> {
        let mut hash = Vec::<(ExpressionNode, ExpressionNode)>::new();

        while self.tokens.last()?.token_type != TokenType::RBrace {
            let key = match self.parse_expression(OperationPrecedence::Lowest) {
                Some(k) => k,
                None => {
                    hash.clear();
                    break;
                }
            };

            if self.tokens.pop()?.token_type != TokenType::Colon {
                hash.clear();
                break;
            }

            let value = match self.parse_expression(OperationPrecedence::Lowest) {
                Some(v) => v,
                None => {
                    hash.clear();
                    break;
                }
            };

            let p = (key, value);
            hash.push(p);

            if self.tokens.last()?.token_type == TokenType::Comma {
                self.tokens.pop();
            }
        }

        self.tokens.pop(); // drop RBrace

        Some(hash)
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
                token: self.tokens.pop()?,
                elements: self.parse_expression_list(TokenType::RBracket)?,
            }))),
            TokenType::LBrace => Some(ExpressionNode::HashLiteralNode(Box::new(HashLiteral {
                token: self.tokens.pop()?,
                pairs: self.parse_hash_literal()?,
            }))),
            TokenType::If => self.parse_if_expression(),
            TokenType::Function => self.parse_function_literal(),
            _ => {
                //self.errors
                //    .push(format!("unsupported by prefix_parser: {:?}", tt));
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
