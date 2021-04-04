use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;

use thiserror::Error;

/// error types
#[derive(Debug, Error)]
pub enum ParseError {
    #[error(r##"unexpected token "{found:}", "{expected:}" was expected"##)]
    UnexpectedToken { found: Token, expected: Token },
    #[error("unexpected EOF")]
    UnexpectedEof,
    #[error(r#""unexpected token "{0:?}" for infix_parse""#)]
    UnexpectedTokenForInfixParser(Token),
    #[error(r#""unexpected token "{0:?}" for prefix_parse""#)]
    UnexpectedTokenForPrefixParser(Token),
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

        match token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    /// parse let statement
    fn parse_let_statement(&mut self) -> Result<StatementNode, ParseError> {
        self.consume_expect_token(Token::Let)?;

        let token = match self.tokens.pop() {
            Some(t) => t,
            None => {
                return Err(ParseError::UnexpectedEof);
            }
        };

        let name = match &token {
            Token::Ident(ident) => Identifier {
                token: Token::Ident(ident.to_string()),
                value: ident.to_string(),
            },
            _ => {
                return Err(ParseError::UnexpectedToken {
                    found: token,
                    expected: Token::Ident("IDENT".into()),
                })
            }
        };

        self.consume_expect_token(Token::Assign)?;

        let value = self.parse_expression(OperationPrecedence::Lowest)?;

        if !self.tokens.is_empty() && self.peek_token()? == &Token::Semicolon {
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
        self.consume_expect_token(Token::Return)?;
        let token = self.peek_token()?.clone();
        let return_value = self.parse_expression(OperationPrecedence::Lowest)?;

        if !self.tokens.is_empty() && self.peek_token()? == &Token::Semicolon {
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

        if !self.tokens.is_empty() && self.peek_token()? == &Token::Semicolon {
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
        let t = self.peek_token()?.clone();
        let mut ex = self.prefix_parse(&t)?;

        while !self.tokens.is_empty()
            && self.expect_token(Token::Semicolon).is_err()
            && precedence < get_precedence(self.peek_token()?)
        {
            let token = self.peek_token()?.clone();
            ex = self.infix_parse(&token, ex.clone())?;
        }

        Ok(ex)
    }

    /// parse identifier
    fn parse_identifier(&mut self) -> Result<ExpressionNode, ParseError> {
        let token = match self.tokens.pop() {
            Some(t) => t,
            None => {
                return Err(ParseError::Unknown);
            }
        };

        if let Token::Ident(ident) = token {
            Ok(ExpressionNode::IdentifierNode(Box::new(Identifier {
                token: Token::Ident(ident.to_string()),
                value: ident,
            })))
        } else {
            Err(ParseError::UnexpectedToken {
                found: token,
                expected: Token::Ident("IDENT".into()),
            })
        }
    }

    /// parse integer literal
    fn parse_integer_literal(&mut self) -> Result<ExpressionNode, ParseError> {
        let token = match self.tokens.pop() {
            Some(t) => t,
            None => {
                return Err(ParseError::Unknown);
            }
        };

        if let Token::Int(int) = token {
            Ok(ExpressionNode::IntegerLiteralNode(Box::new(
                IntegerLiteral {
                    token: Token::Int(int),
                    value: int,
                },
            )))
        } else {
            Err(ParseError::UnexpectedToken {
                found: token,
                expected: Token::Int(0),
            })
        }
    }

    /// parse string literal
    fn parse_string_literal(&mut self) -> Result<ExpressionNode, ParseError> {
        let token = match self.tokens.pop() {
            Some(t) => t,
            None => return Err(ParseError::Unknown),
        };

        if let Token::StringToken(string) = token {
            Ok(ExpressionNode::StringLiteralNode(Box::new(StringLiteral {
                token: Token::StringToken(string.to_string()),
                value: string,
            })))
        } else {
            Err(ParseError::UnexpectedToken {
                found: token,
                expected: Token::StringToken("STRING".into()),
            })
        }
    }

    /// parse function literal
    fn parse_function_literal(&mut self) -> Result<ExpressionNode, ParseError> {
        let token = self.consume_expect_token(Token::Function)?;

        self.expect_token(Token::LParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_token(Token::LBrace)?;

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
            value: token == Token::True,
        })))
    }

    /// parse grouped expression
    fn parse_grouped_expression(&mut self) -> Result<ExpressionNode, ParseError> {
        self.pop_token()?; // drop LParen

        let exp = self.parse_expression(OperationPrecedence::Lowest)?;

        self.consume_expect_token(Token::RParen)?;

        Ok(exp)
    }

    /// parse if expression
    fn parse_if_expression(&mut self) -> Result<ExpressionNode, ParseError> {
        let token = self.consume_expect_token(Token::If)?;
        self.expect_token(Token::LParen)?;

        let condition = self.parse_expression(OperationPrecedence::Lowest)?;

        self.expect_token(Token::LBrace)?;

        let consequence = Some(self.parse_block_statement()?);

        // RParen has been dropped at grouped_expression

        let alternative = if self.expect_token(Token::Else).is_ok() {
            self.pop_token()?;
            self.expect_token(Token::LBrace)?;
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

        while self.expect_token(Token::RBrace).is_err() {
            block.statements.push(self.parse_statement()?);
        }

        self.consume_expect_token(Token::RBrace)?;

        Ok(StatementNode::BlockStatementNode(Box::new(block)))
    }

    /// parse function parameters
    fn parse_function_parameters(&mut self) -> Result<Vec<ExpressionNode>, ParseError> {
        let mut params = Vec::<ExpressionNode>::new();

        self.pop_token()?; // drop LParen

        if self.expect_token(Token::RParen).is_ok() {
            self.tokens.pop();
            return Ok(params);
        }

        let token = self.pop_token()?;

        params.push(ExpressionNode::IdentifierNode(Box::new(Identifier {
            token: token.clone(),
            value: token.get_literal(),
        })));

        while self.consume_expect_token(Token::Comma).is_ok() {
            let t = self.pop_token()?;

            params.push(ExpressionNode::IdentifierNode(Box::new(Identifier {
                token: t.clone(),
                value: t.get_literal(),
            })));
        }

        self.consume_expect_token(Token::RParen)?;

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

        self.consume_expect_token(Token::RBracket)?;

        Ok(ExpressionNode::IndexExpressionNode(Box::new(
            IndexExpression { token, left, index },
        )))
    }

    /// parse function call arguments
    fn parse_call_arguments(&mut self) -> Result<Vec<ExpressionNode>, ParseError> {
        let mut arguments = Vec::<ExpressionNode>::new();

        if self.expect_token(Token::RParen).is_ok() {
            self.tokens.pop();
            return Ok(arguments);
        }

        let arg = self.parse_expression(OperationPrecedence::Lowest)?;
        arguments.push(arg);

        while !self.tokens.is_empty() && self.tokens.last().unwrap() == &Token::Comma {
            // @todo
            self.tokens.pop();
            let arg = self.parse_expression(OperationPrecedence::Lowest)?;
            arguments.push(arg);
        }

        if self.expect_token(Token::RParen).is_ok() {
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
        let precedence = get_precedence(&token);

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
    fn parse_expression_list(&mut self, end: Token) -> Vec<ExpressionNode> {
        let mut list = Vec::<ExpressionNode>::new();

        if self.consume_expect_token(end).is_ok() {
            return list;
        }

        if let Ok(elm) = self.parse_expression(OperationPrecedence::Lowest) {
            list.push(elm);

            while self.expect_token(Token::Comma).is_ok() {
                self.tokens.pop();

                if let Ok(elm) = self.parse_expression(OperationPrecedence::Lowest) {
                    list.push(elm);
                } else {
                    list.clear();
                    break;
                }
            }

            if self.expect_token(Token::RBracket).is_err() {
                list.clear();
            }

            self.tokens.pop();
        }

        list
    }

    /// parse hash literal
    fn parse_hash_literal(&mut self) -> Result<Vec<(ExpressionNode, ExpressionNode)>, ParseError> {
        let mut hash = Vec::<(ExpressionNode, ExpressionNode)>::new();

        while self.consume_expect_token(Token::RBrace).is_err() {
            let key = match self.parse_expression(OperationPrecedence::Lowest) {
                Ok(k) => k,
                Err(_) => {
                    // @todo
                    hash.clear();
                    break;
                }
            };

            if self.expect_token(Token::Colon).is_err() {
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

            if self.expect_token(Token::Comma).is_ok() {
                self.pop_token()?;
            }
        }

        Ok(hash)
    }

    /// parse prefix
    fn prefix_parse(&mut self, token: &Token) -> Result<ExpressionNode, ParseError> {
        match token {
            Token::Ident(_) => self.parse_identifier(),
            Token::Int(_) => self.parse_integer_literal(),
            Token::StringToken(_) => self.parse_string_literal(),
            Token::Bang | Token::Minus => self.parse_prefix_expression(),
            Token::True | Token::False => self.parse_boolean_expression(),
            Token::LParen => self.parse_grouped_expression(),
            Token::LBracket => Ok(ExpressionNode::ArrayLiteralNode(Box::new(ArrayLiteral {
                token: self.pop_token()?,
                elements: self.parse_expression_list(Token::RBracket),
            }))),
            Token::LBrace => Ok(ExpressionNode::HashLiteralNode(Box::new(HashLiteral {
                token: self.pop_token()?,
                pairs: self.parse_hash_literal()?,
            }))),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function_literal(),
            _ => Err(ParseError::UnexpectedTokenForPrefixParser(token.clone())),
        }
    }

    /// parse infix
    fn infix_parse(
        &mut self,
        token: &Token,
        left: ExpressionNode,
    ) -> Result<ExpressionNode, ParseError> {
        match token {
            Token::Plus
            | Token::Minus
            | Token::Asterisk
            | Token::Slash
            | Token::Gt
            | Token::Lt
            | Token::Eq
            | Token::NotEq => self.parse_infix_expression(left),
            Token::LParen => self.parse_call_expression(left),
            Token::LBracket => self.parse_index_expression(left),
            _ => Err(ParseError::UnexpectedTokenForInfixParser(token.clone())),
        }
    }

    /// peek token
    fn peek_token(&mut self) -> Result<&Token, ParseError> {
        self.tokens.last().ok_or(ParseError::UnexpectedEof)
    }

    /// pop token
    fn pop_token(&mut self) -> Result<Token, ParseError> {
        self.tokens.pop().ok_or(ParseError::UnexpectedEof)
    }

    /// check if the next token is expected one or not
    fn expect_token(&mut self, expected: Token) -> Result<&Token, ParseError> {
        if let Some(token) = self.tokens.last() {
            if token == &expected {
                Ok(token)
            } else {
                Err(ParseError::UnexpectedToken {
                    found: token.clone(),
                    expected,
                })
            }
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }

    /// check if the next token is expected one or not, and consume it if so
    fn consume_expect_token(&mut self, expected: Token) -> Result<Token, ParseError> {
        if let Some(token) = self.tokens.last() {
            if token == &expected {
                self.tokens.pop().ok_or(ParseError::Unknown)
            } else {
                Err(ParseError::UnexpectedToken {
                    found: token.clone(),
                    expected,
                })
            }
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }
}

/// get precedence of the operation `tt`
fn get_precedence(t: &Token) -> OperationPrecedence {
    match t {
        Token::LParen => OperationPrecedence::Call,
        Token::Eq | Token::NotEq => OperationPrecedence::Equals,
        Token::Lt | Token::Gt => OperationPrecedence::LessGreater,
        Token::Plus | Token::Minus => OperationPrecedence::Sum,
        Token::Slash | Token::Asterisk => OperationPrecedence::Product,
        Token::LBracket => OperationPrecedence::Index,
        _ => OperationPrecedence::Lowest,
    }
}
