use std::cell::RefCell;
use std::rc::Rc;

use thiserror::Error;

use crate::ast::{ExpressionNode, StatementNode};
use crate::object::{extend_environment, Environment, Object, ObjectError};
use crate::parser::{ParseError, Parser};
use crate::token::Token;

/// error
#[derive(Debug, Error)]
pub enum EvaluationError {
    #[error("the # of arguments is wrong, expected {0}, got {1}")]
    InvalidNumberOfArguments(usize, usize),
    #[error("not a function: {0}")]
    NotFunction(String),
    #[error("type mismatch: {0}")]
    TypeMismatch(String),
    #[error("index operator not supported: {0}")]
    IndexOperatorNotSupported(String),
    #[error("unknown operator: {0}")]
    UnknownOperator(String),
    #[error("unknown error occured")]
    Unknown,

    #[error(transparent)]
    ObjectEvaluationError(#[from] ObjectError),
    #[error(transparent)]
    ParseError(#[from] ParseError),
}

/// evaluator function
pub fn eval(parser: Parser, env: Rc<RefCell<Environment>>) -> Result<Object, EvaluationError> {
    let mut result = Object::Null;
    for stmt in parser {
        result = eval_statement_node(&stmt?, env.clone())?;
        if let Object::ReturnValue(value) = result {
            return Ok(*value);
        }
    }
    Ok(result)
}

/// evaluator function for statement node
fn eval_statement_node(
    node: &StatementNode,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, EvaluationError> {
    match node {
        StatementNode::BlockStatement { statements } => {
            let mut ret = Object::Null;

            for stmt in statements {
                ret = eval_statement_node(stmt, env.clone())?;
                if let Object::ReturnValue(_) = ret {
                    return Ok(ret);
                }
            }
            Ok(ret)
        }
        StatementNode::ExpressionStatement { expression } => eval_expression_node(expression, env),
        StatementNode::ReturnStatement { return_value } => Ok(Object::new_return_value(
            eval_expression_node(return_value, env)?,
        )),
        StatementNode::LetStatement { name, value } => {
            let val = eval_expression_node(&value, env.clone())?;
            env.borrow_mut().set(&String::from(name), &val);
            Ok(val)
        }
    }
}

/// evaluator function for expression node
fn eval_expression_node(
    node: &ExpressionNode,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, EvaluationError> {
    match node {
        ExpressionNode::IntegerLiteral { token } => Ok(Object::from(token)),
        ExpressionNode::StringLiteral { token } => Ok(Object::from(token)),
        ExpressionNode::Boolean { token } => Ok(Object::from(token)),
        ExpressionNode::PrefixExpression { token, right } => {
            let right = eval_expression_node(right, env)?;
            eval_prefix_expression_node(token, &right)
        }
        ExpressionNode::InfixExpression { token, left, right } => {
            let left = eval_expression_node(left, env.clone())?;
            let right = eval_expression_node(right, env)?;
            eval_infix_expression_node(token, &left, &right)
        }
        ExpressionNode::IfExpression {
            condition,
            consequence,
            alternative,
        } => {
            let condition = eval_expression_node(condition, env.clone())?;
            match condition {
                Object::Boolean(value) => {
                    if value {
                        if let Some(consequence) = consequence {
                            eval_statement_node(consequence, env)
                        } else {
                            Ok(Object::Null)
                        }
                    } else if let Some(alternative) = alternative {
                        eval_statement_node(alternative, env)
                    } else {
                        Ok(Object::Null)
                    }
                }
                Object::Null => {
                    if let Some(alternative) = alternative {
                        eval_statement_node(alternative, env)
                    } else {
                        Ok(Object::Null)
                    }
                }
                _ => {
                    if let Some(consequence) = consequence {
                        eval_statement_node(consequence, env)
                    } else {
                        Ok(Object::Null)
                    }
                }
            }
        }
        ExpressionNode::Identifier { token } => env
            .borrow()
            .get(&String::from(token))
            .map_err(EvaluationError::from),
        ExpressionNode::FunctionLiteral {
            token: _,
            parameters,
            body,
        } => {
            if let Some(b) = body {
                Ok(Object::new_function(parameters, b, env))
            } else {
                Ok(Object::Null)
            }
        }
        ExpressionNode::CallExpression {
            token: _,
            function,
            arguments,
        } => {
            let function = eval_expression_node(function, env.clone())?;
            let args = eval_expressions(arguments, env)?;
            apply_function(&function, &args)
        }
        ExpressionNode::IndexExpression {
            token: _,
            left,
            index,
        } => {
            let left = eval_expression_node(&left, env.clone())?;
            let index = eval_expression_node(&index, env)?;
            eval_index_expression(&left, &index)
        }
        ExpressionNode::ArrayLiteral { token: _, elements } => {
            let elements = eval_expressions(elements, env)?;
            Ok(Object::new_array(&elements))
        }
        ExpressionNode::HashLiteral { token: _, pairs } => {
            let mut result = Vec::<(Object, Object)>::new();

            for pair in pairs {
                let key = eval_expression_node(&pair.0, env.clone())?;
                let value = eval_expression_node(&pair.1, env.clone())?;
                result.push((key, value));
            }
            Ok(Object::new_hash(&result))
        }
    }
}

/// evaluator function for prefix expression node
fn eval_prefix_expression_node(
    operator: &Token,
    right: &Object,
) -> Result<Object, EvaluationError> {
    match operator {
        Token::Bang => eval_bang_operation_expression_node(right),
        Token::Minus => eval_minus_operation_expression_node(right),
        _ => Err(EvaluationError::UnknownOperator(format!(
            "{}{}",
            operator,
            right.object_type()
        ))),
    }
}

/// evaluator function for bang operation expression node
#[allow(clippy::unnecessary_wraps)]
fn eval_bang_operation_expression_node(right: &Object) -> Result<Object, EvaluationError> {
    match right {
        Object::Boolean(value) => Ok(Object::from(!value)),
        Object::Null => Ok(Object::from(true)),
        _ => Ok(Object::from(false)),
    }
}

/// evaluator function for minus operation expression node
fn eval_minus_operation_expression_node(right: &Object) -> Result<Object, EvaluationError> {
    if let Object::Integer(value) = right {
        Ok(Object::from(-value))
    } else {
        Err(EvaluationError::UnknownOperator(format!(
            "-{}",
            right.object_type()
        )))
    }
}

/// evaluator function for infix expression node
fn eval_infix_expression_node(
    operator: &Token,
    left: &Object,
    right: &Object,
) -> Result<Object, EvaluationError> {
    if let Object::Integer(left_integer) = left {
        if let Object::Integer(right_integer) = right {
            return eval_integer_infix_expression(operator, left_integer, right_integer);
        }
    }

    if let Object::String(left_str) = left {
        if let Object::String(right_str) = right {
            if operator == &Token::Plus {
                let mut s = String::new();
                s.push_str(left_str);
                s.push_str(right_str);
                return Ok(Object::from(&s));
            }
        }
    }

    match operator {
        Token::Eq => Ok(Object::from(left == right)),
        Token::NotEq => Ok(Object::from(left != right)),
        _ => {
            if left.object_type() != right.object_type() {
                Err(EvaluationError::TypeMismatch(format!(
                    "{} {} {}",
                    left.object_type(),
                    operator,
                    right.object_type(),
                )))
            } else {
                Err(EvaluationError::UnknownOperator(format!(
                    "{} {} {}",
                    left.object_type(),
                    operator,
                    right.object_type(),
                )))
            }
        }
    }
}

/// evaluator function for integer
fn eval_integer_infix_expression(
    operator: &Token,
    left: &i64,
    right: &i64,
) -> Result<Object, EvaluationError> {
    match operator {
        Token::Plus => Ok(Object::from(left + right)),
        Token::Minus => Ok(Object::from(left - right)),
        Token::Asterisk => Ok(Object::from(left * right)),
        Token::Slash => Ok(Object::from(left / right)),
        Token::Lt => Ok(Object::from(left < right)),
        Token::Gt => Ok(Object::from(left > right)),
        Token::Eq => Ok(Object::from(left == right)),
        Token::NotEq => Ok(Object::from(left != right)),
        _ => Err(EvaluationError::Unknown),
    }
}

/// evaluator function for expressions
fn eval_expressions(
    exps: &[ExpressionNode],
    env: Rc<RefCell<Environment>>,
) -> Result<Vec<Object>, EvaluationError> {
    let mut result = Vec::<Object>::new();

    for e in exps {
        let evaluated = eval_expression_node(e, env.clone())?;
        result.push(evaluated);
    }

    Ok(result)
}

/// evaluator function for index expression
fn eval_index_expression(left: &Object, index: &Object) -> Result<Object, EvaluationError> {
    if let Object::Array(ao) = left {
        if let Object::Integer(idx) = index {
            let max = ao.len();

            if idx < &0 || idx >= &(max as i64) {
                Ok(Object::Null)
            } else {
                Ok(ao[*idx as usize].clone())
            }
        } else {
            Err(EvaluationError::IndexOperatorNotSupported(
                left.object_type().into(),
            ))
        }
    } else if let Object::Hash(ho) = left {
        eval_hash_index_expression(&ho, &index)
    } else {
        Err(EvaluationError::IndexOperatorNotSupported(
            left.object_type().into(),
        ))
    }
}

/// evaluator function for hash
#[allow(clippy::unnecessary_wraps)]
fn eval_hash_index_expression(
    hash: &[(Object, Object)],
    key: &Object,
) -> Result<Object, EvaluationError> {
    match hash.iter().find(|&k| k.0 == *key) {
        Some(o) => Ok(o.1.clone()),
        None => Ok(Object::Null),
    }
}

/// apply function
fn apply_function(function: &Object, args: &[Object]) -> Result<Object, EvaluationError> {
    match function {
        Object::Function {
            parameters,
            body,
            env,
        } => {
            if parameters.len() != args.len() {
                return Err(EvaluationError::InvalidNumberOfArguments(
                    parameters.len(),
                    args.len(),
                ));
            }
            let extended_env = extend_environment(env.clone());
            for (idx, p) in parameters.iter().enumerate() {
                extended_env
                    .clone()
                    .borrow_mut()
                    .set(&String::from(p), &args[idx]);
            }
            let evaluated = eval_statement_node(&body, extended_env)?;

            if let Object::ReturnValue(value) = evaluated {
                Ok(*value)
            } else {
                Ok(evaluated)
            }
        }
        Object::Builtin(bio) => (bio)(args.to_vec()).map_err(EvaluationError::from),
        _ => Err(EvaluationError::NotFunction(function.object_type().into())),
    }
}
