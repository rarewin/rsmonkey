use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

use thiserror::Error;

/// error types
#[derive(Debug, Error)]
pub enum ParseError {
    #[error(r##"unexpected token "{found:?}", "{expected:?}" was expected"##)]
    UnexpectedToken {
        found: TokenType,
        expected: TokenType,
    },
    #[error("unexpected EOF")]
    UnexpectedEof,
    #[error(r#""unexpected token "{0:?}" for infix_parse""#)]
    UnexpectedTokenForInfixParser(TokenType),
    #[error(r#""unexpected token "{0:?}" for prefix_parse""#)]
    UnexpectedTokenForPrefixParser(TokenType),
    #[error("unknown error")]
    Unknown,

    #[error(transparent)]
    InvalidStringForInteger(#[from] std::num::ParseIntError),
}

/// struct for parser
#[derive(Debug)]
pub struct Parser {
    /// tokens
    tokens: Vec<Token>,
    /// error string
    errors: Vec<String>,
}

impl Iterator for Parser {
    type Item = Result<StatementNode, ParseError>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.peek_token().is_ok() {
            Some(self.parse_statement())
        } else {
            None
        }
    }
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

    /// parse statement
    fn parse_statement(&mut self) -> Result<StatementNode, ParseError> {
        let token = self.peek_token()?;

        match token.get_token_type() {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    /// parse let statement
    fn parse_let_statement(&mut self) -> Result<StatementNode, ParseError> {
        self.consume_expect_token(TokenType::Let)?;
        let token = self.consume_expect_token(TokenType::Ident)?;

        let name = Identifier {
            token: token.clone(),
            value: token.get_literal(),
        };

        self.consume_expect_token(TokenType::Assign)?;

        let value = self.parse_expression(OperationPrecedence::Lowest)?;

        if !self.tokens.is_empty() && self.peek_token()?.get_token_type() == TokenType::Semicolon {
            self.tokens.pop();
        }

        Ok(StatementNode::LetStatementNode(Box::new(LetStatement {
            token,
            name,
            value,
        })))
    }

    /// parse return statement
    fn parse_return_statement(&mut self) -> Result<StatementNode, ParseError> {
        self.consume_expect_token(TokenType::Return)?;
        let token = self.peek_token()?.clone();
        let return_value = self.parse_expression(OperationPrecedence::Lowest)?;

        if !self.tokens.is_empty() && self.peek_token()?.get_token_type() == TokenType::Semicolon {
            self.tokens.pop();
        }

        Ok(StatementNode::ReturnStatementNode(Box::new(
            ReturnStatement {
                token,
                return_value,
            },
        )))
    }

    /// parse expression statement
    fn parse_expression_statement(&mut self) -> Result<StatementNode, ParseError> {
        let token = self.peek_token()?.clone();
        let expression = self.parse_expression(OperationPrecedence::Lowest)?;

        if !self.tokens.is_empty() && self.peek_token()?.get_token_type() == TokenType::Semicolon {
            self.pop_token()?;
        }

        Ok(StatementNode::ExpressionStatementNode(Box::new(
            ExpressionStatement { token, expression },
        )))
    }

    /// parse expression
    fn parse_expression(
        &mut self,
        precedence: OperationPrecedence,
    ) -> Result<ExpressionNode, ParseError> {
        let tt = self.peek_token()?.get_token_type();
        let mut ex = self.prefix_parse(tt)?;

        while !self.tokens.is_empty()
            && self.expect_token(TokenType::Semicolon).is_err()
            && precedence < get_precedence(self.peek_token()?.get_token_type())
        {
            let tt = self.peek_token()?.get_token_type();
            ex = self.infix_parse(tt, ex.clone())?;
        }

        Ok(ex)
    }

    /// parse identifier
    fn parse_identifier(&mut self) -> Result<ExpressionNode, ParseError> {
        let token = self.consume_expect_token(TokenType::Ident)?;

        Ok(ExpressionNode::IdentifierNode(Box::new(Identifier {
            token: token.clone(),
            value: token.get_literal(),
        })))
    }

    /// parse integer literal
    fn parse_integer_literal(&mut self) -> Result<ExpressionNode, ParseError> {
        let token = self.consume_expect_token(TokenType::Int)?;

        Ok(ExpressionNode::IntegerLiteralNode(Box::new(
            IntegerLiteral {
                token: token.clone(),
                value: token.get_literal().parse()?,
            },
        )))
    }

    /// parse string literal
    fn parse_string_literal(&mut self) -> Result<ExpressionNode, ParseError> {
        let token = self.consume_expect_token(TokenType::StringToken)?;

        Ok(ExpressionNode::StringLiteralNode(Box::new(StringLiteral {
            token: token.clone(),
            value: token.get_literal(),
        })))
    }

    /// parse function literal
    fn parse_function_literal(&mut self) -> Result<ExpressionNode, ParseError> {
        let token = self.consume_expect_token(TokenType::Function)?;

        self.expect_token(TokenType::LParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_token(TokenType::LBrace)?;

        let body = Some(self.parse_block_statement()?);

        Ok(ExpressionNode::FunctionLiteralNode(Box::new(
            FunctionLiteral {
                token,
                parameters,
                body,
            },
        )))
    }

    /// parse prefix expression
    fn parse_prefix_expression(&mut self) -> Result<ExpressionNode, ParseError> {
        let token = self.pop_token()?;
        let operator = token.get_literal();
        let right = self.parse_expression(OperationPrecedence::Prefix)?;

        Ok(ExpressionNode::PrefixExpressionNode(Box::new(
            PrefixExpression {
                token,
                operator,
                right,
            },
        )))
    }

    /// parse boolean expression
    fn parse_boolean_expression(&mut self) -> Result<ExpressionNode, ParseError> {
        let token = self.pop_token()?;

        Ok(ExpressionNode::BooleanExpressionNode(Box::new(Boolean {
            token: token.clone(),
            value: token.get_token_type() == TokenType::True,
        })))
    }

    /// parse grouped expression
    fn parse_grouped_expression(&mut self) -> Result<ExpressionNode, ParseError> {
        self.pop_token()?; // drop LParen

        let exp = self.parse_expression(OperationPrecedence::Lowest)?;

        self.consume_expect_token(TokenType::RParen)?;

        Ok(exp)
    }

    /// parse if expression
    fn parse_if_expression(&mut self) -> Result<ExpressionNode, ParseError> {
        let token = self.consume_expect_token(TokenType::If)?;
        self.expect_token(TokenType::LParen)?;

        let condition = self.parse_expression(OperationPrecedence::Lowest)?;

        self.expect_token(TokenType::LBrace)?;

        let consequence = Some(self.parse_block_statement()?);

        // RParen has been dropped at grouped_expression

        let alternative = if self.expect_token(TokenType::Else).is_ok() {
            self.pop_token()?;
            self.expect_token(TokenType::LBrace)?;
            Some(self.parse_block_statement()?)
        } else {
            None
        };

        Ok(ExpressionNode::IfExpressionNode(Box::new(IfExpression {
            token,
            condition,
            consequence,
            alternative,
        })))
    }

    /// parse block statement
    fn parse_block_statement(&mut self) -> Result<StatementNode, ParseError> {
        let mut block = BlockStatement {
            token: self.pop_token()?,
            statements: Vec::new(),
        };

        while self.expect_token(TokenType::RBrace).is_err() {
            block.statements.push(self.parse_statement()?);
        }

        self.consume_expect_token(TokenType::RBrace)?;

        Ok(StatementNode::BlockStatementNode(Box::new(block)))
    }

    /// parse function parameters
    fn parse_function_parameters(&mut self) -> Result<Vec<ExpressionNode>, ParseError> {
        let mut params = Vec::<ExpressionNode>::new();

        self.pop_token()?; // drop LParen

        if self.expect_token(TokenType::RParen).is_ok() {
            self.tokens.pop();
            return Ok(params);
        }

        let token = self.pop_token()?;

        params.push(ExpressionNode::IdentifierNode(Box::new(Identifier {
            token: token.clone(),
            value: token.get_literal(),
        })));

        while self.consume_expect_token(TokenType::Comma).is_ok() {
            let t = self.pop_token()?;

            params.push(ExpressionNode::IdentifierNode(Box::new(Identifier {
                token: t.clone(),
                value: t.get_literal(),
            })));
        }

        self.consume_expect_token(TokenType::RParen)?;

        Ok(params)
    }

    /// parse call expression
    fn parse_call_expression(
        &mut self,
        left: ExpressionNode,
    ) -> Result<ExpressionNode, ParseError> {
        let token = self.pop_token()?;
        let function = left;
        let arguments = self.parse_call_arguments()?;

        Ok(ExpressionNode::CallExpressionNode(Box::new(
            CallExpression {
                token,
                function,
                arguments,
            },
        )))
    }

    /// parse index expression
    fn parse_index_expression(
        &mut self,
        left: ExpressionNode,
    ) -> Result<ExpressionNode, ParseError> {
        let token = self.pop_token()?;
        let index = self.parse_expression(OperationPrecedence::Lowest)?;

        self.consume_expect_token(TokenType::RBracket)?;

        Ok(ExpressionNode::IndexExpressionNode(Box::new(
            IndexExpression { token, left, index },
        )))
    }

    /// parse function call arguments
    fn parse_call_arguments(&mut self) -> Result<Vec<ExpressionNode>, ParseError> {
        let mut arguments = Vec::<ExpressionNode>::new();

        if self.expect_token(TokenType::RParen).is_ok() {
            self.tokens.pop();
            return Ok(arguments);
        }

        let arg = self.parse_expression(OperationPrecedence::Lowest)?;
        arguments.push(arg);

        while !self.tokens.is_empty()
            && self.tokens.last().unwrap().get_token_type() == TokenType::Comma
        {
            self.tokens.pop();
            let arg = self.parse_expression(OperationPrecedence::Lowest)?;
            arguments.push(arg);
        }

        if self.expect_token(TokenType::RParen).is_ok() {
            self.tokens.pop();
            Ok(arguments)
        } else {
            Err(ParseError::Unknown)
        }
    }

    /// parse infix expression
    fn parse_infix_expression(
        &mut self,
        left: ExpressionNode,
    ) -> Result<ExpressionNode, ParseError> {
        let token = self.pop_token()?;
        let operator = token.get_literal();
        let precedence = get_precedence(token.get_token_type());

        let right = self.parse_expression(precedence)?;

        Ok(ExpressionNode::InfixExpressionNode(Box::new(
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

        if self.consume_expect_token(end).is_ok() {
            return list;
        }

        if let Ok(elm) = self.parse_expression(OperationPrecedence::Lowest) {
            list.push(elm);

            while self.expect_token(TokenType::Comma).is_ok() {
                self.tokens.pop();

                if let Ok(elm) = self.parse_expression(OperationPrecedence::Lowest) {
                    list.push(elm);
                } else {
                    list.clear();
                    break;
                }
            }

            if self.expect_token(TokenType::RBracket).is_err() {
                list.clear();
            }

            self.tokens.pop();
        }

        list
    }

    /// parse hash literal
    fn parse_hash_literal(&mut self) -> Result<Vec<(ExpressionNode, ExpressionNode)>, ParseError> {
        let mut hash = Vec::<(ExpressionNode, ExpressionNode)>::new();

        while self.consume_expect_token(TokenType::RBrace).is_err() {
            let key = match self.parse_expression(OperationPrecedence::Lowest) {
                Ok(k) => k,
                Err(_) => {
                    // @todo
                    hash.clear();
                    break;
                }
            };

            if self.expect_token(TokenType::Colon).is_err() {
                hash.clear();
                break;
            }

            self.pop_token()?;

            let value = match self.parse_expression(OperationPrecedence::Lowest) {
                Ok(v) => v,
                Err(_) => {
                    // @todo
                    hash.clear();
                    break;
                }
            };

            let p = (key, value);
            hash.push(p);

            if self.expect_token(TokenType::Comma).is_ok() {
                self.pop_token()?;
            }
        }

        Ok(hash)
    }

    /// parse prefix
    fn prefix_parse(&mut self, tt: TokenType) -> Result<ExpressionNode, ParseError> {
        match tt {
            TokenType::Ident => self.parse_identifier(),
            TokenType::Int => self.parse_integer_literal(),
            TokenType::StringToken => self.parse_string_literal(),
            TokenType::Bang | TokenType::Minus => self.parse_prefix_expression(),
            TokenType::True | TokenType::False => self.parse_boolean_expression(),
            TokenType::LParen => self.parse_grouped_expression(),
            TokenType::LBracket => Ok(ExpressionNode::ArrayLiteralNode(Box::new(ArrayLiteral {
                token: self.pop_token()?,
                elements: self.parse_expression_list(TokenType::RBracket),
            }))),
            TokenType::LBrace => Ok(ExpressionNode::HashLiteralNode(Box::new(HashLiteral {
                token: self.pop_token()?,
                pairs: self.parse_hash_literal()?,
            }))),
            TokenType::If => self.parse_if_expression(),
            TokenType::Function => self.parse_function_literal(),
            _ => Err(ParseError::UnexpectedTokenForPrefixParser(tt)),
        }
    }

    /// parse infix
    fn infix_parse(
        &mut self,
        tt: TokenType,
        left: ExpressionNode,
    ) -> Result<ExpressionNode, ParseError> {
        match tt {
            TokenType::Plus
            | TokenType::Minus
            | TokenType::Asterisk
            | TokenType::Slash
            | TokenType::Gt
            | TokenType::Lt
            | TokenType::Eq
            | TokenType::NotEq => self.parse_infix_expression(left),
            TokenType::LParen => self.parse_call_expression(left),
            TokenType::LBracket => self.parse_index_expression(left),
            _ => Err(ParseError::UnexpectedTokenForInfixParser(tt)),
        }
    }

    /// peek token
    fn peek_token(&mut self) -> Result<&Token, ParseError> {
        if let Some(token) = self.tokens.last() {
            Ok(token)
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }

    /// pop token
    fn pop_token(&mut self) -> Result<Token, ParseError> {
        if let Some(token) = self.tokens.pop() {
            Ok(token)
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }

    /// check if the next token is expected one or not
    fn expect_token(&mut self, expected: TokenType) -> Result<&Token, ParseError> {
        if let Some(token) = self.tokens.last() {
            if token.get_token_type() == expected {
                Ok(token)
            } else {
                Err(ParseError::UnexpectedToken {
                    found: token.get_token_type(),
                    expected,
                })
            }
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }

    /// check if the next token is expected one or not, and consume it if so
    fn consume_expect_token(&mut self, expected: TokenType) -> Result<Token, ParseError> {
        if let Some(token) = self.tokens.last() {
            if token.get_token_type() == expected {
                if let Some(t) = self.tokens.pop() {
                    Ok(t)
                } else {
                    Err(ParseError::Unknown)
                }
            } else {
                Err(ParseError::UnexpectedToken {
                    found: token.get_token_type(),
                    expected,
                })
            }
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }
}

/// get precedence of the operation `tt`
fn get_precedence(tt: TokenType) -> OperationPrecedence {
    match tt {
        TokenType::LParen => OperationPrecedence::Call,
        TokenType::Eq | TokenType::NotEq => OperationPrecedence::Equals,
        TokenType::Lt | TokenType::Gt => OperationPrecedence::LessGreater,
        TokenType::Plus | TokenType::Minus => OperationPrecedence::Sum,
        TokenType::Slash | TokenType::Asterisk => OperationPrecedence::Product,
        TokenType::LBracket => OperationPrecedence::Index,
        _ => OperationPrecedence::Lowest,
    }
}
