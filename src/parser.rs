use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;
use std::iter::Peekable;

use thiserror::Error;

/// error types
#[derive(Debug, Error)]
pub enum ParseError {
    #[error(r##"unexpected token "{found:}", "{expected:}" was expected"##)]
    UnexpectedToken { found: Token, expected: Token },
    #[error("unexpected EOF")]
    UnexpectedEof,
    #[error(r#""unexpected token "{0:}" for infix_parse""#)]
    UnexpectedTokenForInfixParser(Token),
    #[error(r#""unexpected token "{0:}" for prefix_parse""#)]
    UnexpectedTokenForPrefixParser(Token),
    #[error("unknown error")]
    Unknown,

    #[error(transparent)]
    InvalidStringForInteger(#[from] std::num::ParseIntError),
}

/// struct for parser
#[derive(Debug)]
pub struct Parser {
    /// statements
    statements: Vec<Result<StatementNode, ParseError>>,
}

impl IntoIterator for Parser {
    type Item = Result<StatementNode, ParseError>;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
    }
}

impl Parser {
    /// constructor
    pub fn new(l: Lexer) -> Parser {
        let mut statements = Vec::new();

        let mut iter = l.into_iter().peekable();

        while iter.peek().is_some() {
            statements.push(parse_statement(&mut iter));
        }

        Parser { statements }
    }
}

fn parse_statement<I>(l: &mut Peekable<I>) -> Result<StatementNode, ParseError>
where
    I: Iterator<Item = Token>,
{
    match l.peek() {
        Some(Token::Let) => parse_let_statement(l),
        Some(Token::Return) => parse_return_statement(l),
        Some(_) => parse_expression_statement(l),
        None => Err(ParseError::UnexpectedEof),
    }
}

fn parse_let_statement<I>(l: &mut Peekable<I>) -> Result<StatementNode, ParseError>
where
    I: Iterator<Item = Token>,
{
    let _token = consume_expect(l, Token::Let)?;
    let ident = consume_expect(l, Token::Ident("".into()))?;

    let name = if let Token::Ident(_) = ident {
        ident
    } else {
        return Err(ParseError::UnexpectedEof);
    };

    consume_expect(l, Token::Assign)?;

    let value = parse_expression(l, OperationPrecedence::Lowest)?;

    if l.peek() == Some(&Token::Semicolon) {
        consume_expect(l, Token::Semicolon)?;
    }

    Ok(StatementNode::LetStatement { name, value })
}

fn parse_return_statement<I>(l: &mut Peekable<I>) -> Result<StatementNode, ParseError>
where
    I: Iterator<Item = Token>,
{
    let _token = consume_expect(l, Token::Return)?;
    let return_value = parse_expression(l, OperationPrecedence::Lowest)?;

    if l.peek() == Some(&Token::Semicolon) {
        consume_expect(l, Token::Semicolon)?;
    }

    Ok(StatementNode::ReturnStatement { return_value })
}

fn parse_expression_statement<I>(l: &mut Peekable<I>) -> Result<StatementNode, ParseError>
where
    I: Iterator<Item = Token>,
{
    let _token = l.peek().ok_or(ParseError::UnexpectedEof)?.clone();
    let expression = parse_expression(l, OperationPrecedence::Lowest)?;

    if l.peek() == Some(&Token::Semicolon) {
        l.next();
    }

    Ok(StatementNode::ExpressionStatement { expression })
}

fn parse_expression<I>(
    l: &mut Peekable<I>,
    precedence: OperationPrecedence,
) -> Result<ExpressionNode, ParseError>
where
    I: Iterator<Item = Token>,
{
    let mut ex = prefix_parse(l)?;

    while l.peek().is_some()
        && l.peek() != Some(&Token::Semicolon)
        && precedence < get_precedence(l.peek().ok_or(ParseError::UnexpectedEof)?)
    {
        ex = infix_parse(l, ex)?;
    }

    Ok(ex)
}

fn prefix_parse<I>(l: &mut Peekable<I>) -> Result<ExpressionNode, ParseError>
where
    I: Iterator<Item = Token>,
{
    let token = l.next().ok_or(ParseError::UnexpectedEof)?;

    match token {
        Token::Int(_) => Ok(ExpressionNode::IntegerLiteral { token }),
        Token::String(_) => Ok(ExpressionNode::StringLiteral { token }),
        Token::Ident(_) => Ok(ExpressionNode::Identifier { token }),
        Token::Boolean(_) => Ok(ExpressionNode::Boolean { token }),
        Token::LParen => {
            let ex = parse_expression(l, OperationPrecedence::Lowest)?;
            consume_expect(l, Token::RParen)?;
            Ok(ex)
        }
        Token::LBracket => {
            let elements = parse_expression_list(l, &Token::RBracket)?;
            consume_expect(l, Token::RBracket)?;
            Ok(ExpressionNode::ArrayLiteral { token, elements })
        }
        Token::LBrace => {
            let pairs = parse_hash_literal(l)?;
            consume_expect(l, Token::RBrace)?;
            Ok(ExpressionNode::HashLiteral { token, pairs })
        }
        Token::If => {
            consume_expect(l, Token::LParen)?;
            let condition = Box::new(parse_expression(l, OperationPrecedence::Lowest)?);
            consume_expect(l, Token::RParen)?;
            let consequence = Some(Box::new(parse_block_statement(l)?));

            let alternative = if l.peek() == Some(&Token::Else) {
                consume_expect(l, Token::Else)?;
                Some(Box::new(parse_block_statement(l)?))
            } else {
                None
            };

            Ok(ExpressionNode::IfExpression {
                condition,
                consequence,
                alternative,
            })
        }
        Token::Function => {
            let parameters = parse_function_parameters(l)?;
            let body = Some(Box::new(parse_block_statement(l)?));

            Ok(ExpressionNode::FunctionLiteral {
                token,
                parameters,
                body,
            })
        }
        Token::Bang | Token::Minus => parse_prefix_expression(l, token),
        _ => Err(ParseError::UnexpectedTokenForPrefixParser(token)),
    }
}

fn parse_prefix_expression<I>(
    l: &mut Peekable<I>,
    token: Token,
) -> Result<ExpressionNode, ParseError>
where
    I: Iterator<Item = Token>,
{
    let right = Box::new(parse_expression(l, OperationPrecedence::Prefix)?);

    Ok(ExpressionNode::PrefixExpression { token, right })
}

fn infix_parse<I>(l: &mut Peekable<I>, left: ExpressionNode) -> Result<ExpressionNode, ParseError>
where
    I: Iterator<Item = Token>,
{
    let token = l.next().ok_or(ParseError::UnexpectedEof)?;

    match &token {
        Token::Plus
        | Token::Minus
        | Token::Asterisk
        | Token::Slash
        | Token::Gt
        | Token::Lt
        | Token::Eq
        | Token::NotEq => {
            let right = Box::new(parse_expression(l, get_precedence(&token))?);
            let left = Box::new(left);
            Ok(ExpressionNode::InfixExpression { token, left, right })
        }
        Token::LParen => {
            let arguments = parse_call_arguments(l)?;
            let function = Box::new(left);
            consume_expect(l, Token::RParen)?;
            Ok(ExpressionNode::CallExpression {
                token,
                function,
                arguments,
            })
        }
        Token::LBracket => {
            let index = Box::new(parse_expression(l, OperationPrecedence::Lowest)?);
            let left = Box::new(left);
            consume_expect(l, Token::RBracket)?;
            Ok(ExpressionNode::IndexExpression { token, left, index })
        }
        _ => Err(ParseError::UnexpectedTokenForInfixParser(token)),
    }
}

fn parse_call_arguments<I>(l: &mut Peekable<I>) -> Result<Vec<ExpressionNode>, ParseError>
where
    I: Iterator<Item = Token>,
{
    let mut arguments = Vec::new();

    if l.peek() == Some(&Token::RParen) {
        return Ok(arguments);
    }

    let arg = parse_expression(l, OperationPrecedence::Lowest)?;
    arguments.push(arg);

    while l.peek() == Some(&Token::Comma) {
        consume_expect(l, Token::Comma)?;
        let arg = parse_expression(l, OperationPrecedence::Lowest)?;
        arguments.push(arg);
    }

    Ok(arguments)
}

fn parse_expression_list<I>(
    l: &mut Peekable<I>,
    end: &Token,
) -> Result<Vec<ExpressionNode>, ParseError>
where
    I: Iterator<Item = Token>,
{
    let mut list = Vec::<ExpressionNode>::new();

    if l.peek() == Some(end) {
        return Ok(list);
    }

    if let Ok(elm) = parse_expression(l, OperationPrecedence::Lowest) {
        list.push(elm);

        while l.peek() == Some(&Token::Comma) {
            consume_expect(l, Token::Comma)?;
            if let Ok(elm) = parse_expression(l, OperationPrecedence::Lowest) {
                list.push(elm);
            } else {
                list.clear();
                break;
            }
        }
    }

    Ok(list)
}

fn parse_hash_literal<I>(
    l: &mut Peekable<I>,
) -> Result<Vec<(ExpressionNode, ExpressionNode)>, ParseError>
where
    I: Iterator<Item = Token>,
{
    let mut hash = Vec::new();

    while l.peek() != Some(&Token::RBrace) {
        let key = parse_expression(l, OperationPrecedence::Lowest)?;
        consume_expect(l, Token::Colon)?;
        let value = parse_expression(l, OperationPrecedence::Lowest)?;
        hash.push((key, value));

        if l.peek() == Some(&Token::RBrace) {
            break;
        }

        consume_expect(l, Token::Comma)?;
    }

    Ok(hash)
}

fn parse_block_statement<I>(l: &mut Peekable<I>) -> Result<StatementNode, ParseError>
where
    I: Iterator<Item = Token>,
{
    let _token = consume_expect(l, Token::LBrace)?;
    let mut statements = Vec::new();

    while l.peek() != Some(&Token::RBrace) {
        statements.push(parse_statement(l)?);
    }

    consume_expect(l, Token::RBrace)?;

    Ok(StatementNode::BlockStatement { statements })
}

fn parse_function_parameters<I>(l: &mut Peekable<I>) -> Result<Vec<ExpressionNode>, ParseError>
where
    I: Iterator<Item = Token>,
{
    let mut params = Vec::new();

    consume_expect(l, Token::LParen)?;

    if l.peek() == Some(&Token::RParen) {
        consume_expect(l, Token::RParen)?;
        return Ok(params);
    }

    let token = l.next().ok_or(ParseError::UnexpectedEof)?;

    params.push(ExpressionNode::Identifier { token });

    while l.peek() == Some(&Token::Comma) {
        consume_expect(l, Token::Comma)?;

        let token = l.next().ok_or(ParseError::UnexpectedEof)?;

        params.push(ExpressionNode::Identifier { token });
    }

    consume_expect(l, Token::RParen)?;

    Ok(params)
}

fn consume_expect<I>(l: &mut Peekable<I>, expect: Token) -> Result<Token, ParseError>
where
    I: Iterator<Item = Token>,
{
    match l.next() {
        Some(token) => match expect {
            Token::Ident(_) => {
                if let Token::Ident(_) = token {
                    Ok(token)
                } else {
                    Err(ParseError::UnexpectedToken {
                        found: token,
                        expected: expect,
                    })
                }
            }
            Token::String(_) => {
                if let Token::String(_) = token {
                    Ok(token)
                } else {
                    Err(ParseError::UnexpectedToken {
                        found: token,
                        expected: expect,
                    })
                }
            }
            Token::Int(_) => {
                if let Token::Int(_) = token {
                    Ok(token)
                } else {
                    Err(ParseError::UnexpectedToken {
                        found: token,
                        expected: expect,
                    })
                }
            }
            _ => {
                if token == expect {
                    Ok(token)
                } else {
                    Err(ParseError::UnexpectedToken {
                        found: token,
                        expected: expect,
                    })
                }
            }
        },
        _ => Err(ParseError::UnexpectedEof),
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
