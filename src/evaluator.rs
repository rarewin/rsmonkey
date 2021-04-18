use std::cell::RefCell;
use std::rc::Rc;

use thiserror::Error;

use crate::ast::{BlockStatement, ExpressionNode, LetStatement, ReturnStatement, StatementNode};
use crate::object::{extend_environment, Array, Environment, Hash, Integer, Object, ObjectError};
use crate::parser::{ParseError, Parser};

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
        if let Object::ReturnValueObject(rv) = result {
            return Ok(rv.value);
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
        StatementNode::BlockStatementNode(bs) => eval_block_statement(&bs, env),
        StatementNode::ExpressionStatementNode(es) => eval_expression_node(&es.expression, env),
        StatementNode::ReturnStatementNode(rs) => eval_return_statement(&rs, env),
        StatementNode::LetStatementNode(ls) => eval_let_statement(&ls, env),
    }
}

/// evaluator function for block statement
fn eval_block_statement(
    bl: &BlockStatement,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, EvaluationError> {
    let mut result = Object::Null;
    for stmt in &bl.statements {
        result = eval_statement_node(stmt, env.clone())?;
        if let Object::ReturnValueObject(rv) = result {
            return Ok(Object::ReturnValueObject(rv));
        }
    }
    Ok(result)
}

/// evaluator function for return statement
fn eval_return_statement(
    rs: &ReturnStatement,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, EvaluationError> {
    Ok(Object::new_return_value(eval_expression_node(
        &rs.return_value,
        env,
    )?))
}

/// evaluator function for let statement
fn eval_let_statement(
    ls: &LetStatement,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, EvaluationError> {
    let val = eval_expression_node(&ls.value, env.clone())?;
    env.borrow_mut().set(&format!("{}", &ls.name.token), &val);
    Ok(val)
}

/// evaluator function for expression node
fn eval_expression_node(
    node: &ExpressionNode,
    env: Rc<RefCell<Environment>>,
) -> Result<Object, EvaluationError> {
    match node {
        ExpressionNode::IntegerLiteralNode(il) => Ok(Object::from(&il.token)),
        ExpressionNode::StringLiteralNode(sl) => Ok(Object::from(&sl.token)),
        ExpressionNode::BooleanExpressionNode(be) => Ok(Object::new_boolean(be.value)),
        ExpressionNode::PrefixExpressionNode(pe) => {
            let right = eval_expression_node(&pe.right, env)?;
            eval_prefix_expression_node(&format!("{}", pe.token), &right)
        }
        ExpressionNode::InfixExpressionNode(ie) => {
            let left = eval_expression_node(&ie.left, env.clone())?;
            let right = eval_expression_node(&ie.right, env)?;
            eval_infix_expression_node(&format!("{}", ie.token), &left, &right)
        }
        ExpressionNode::IfExpressionNode(ie) => {
            let condition = eval_expression_node(&ie.condition, env.clone())?;
            match condition {
                Object::BooleanObject(b) => {
                    if b.value {
                        if let Some(consequence) = &ie.consequence {
                            eval_statement_node(consequence, env)
                        } else {
                            Ok(Object::Null)
                        }
                    } else if let Some(alternative) = &ie.alternative {
                        eval_statement_node(alternative, env)
                    } else {
                        Ok(Object::Null)
                    }
                }
                Object::Null => {
                    if let Some(alternative) = &ie.alternative {
                        eval_statement_node(alternative, env)
                    } else {
                        Ok(Object::Null)
                    }
                }
                _ => {
                    if let Some(consequence) = &ie.consequence {
                        eval_statement_node(consequence, env)
                    } else {
                        Ok(Object::Null)
                    }
                }
            }
        }
        ExpressionNode::IdentifierNode(id) => env
            .borrow()
            .get(&format!("{}", id.token))
            .map_err(EvaluationError::from),
        ExpressionNode::FunctionLiteralNode(fl) => {
            if let Some(body) = &fl.body {
                Ok(Object::new_function(&fl.parameters, body, env))
            } else {
                Ok(Object::Null)
            }
        }
        ExpressionNode::CallExpressionNode(ce) => {
            let function = eval_expression_node(&ce.function, env.clone())?;
            let args = eval_expressions(&ce.arguments, env)?;
            apply_function(&function, &args)
        }
        ExpressionNode::IndexExpressionNode(ie) => {
            let left = eval_expression_node(&ie.left, env.clone())?;
            let index = eval_expression_node(&ie.index, env)?;
            eval_index_expression(&left, &index)
        }
        ExpressionNode::ArrayLiteralNode(al) => {
            let elements = eval_expressions(&al.elements, env)?;
            Ok(Object::new_array(&elements))
        }
        ExpressionNode::HashLiteralNode(hl) => {
            let mut result = Vec::<(Object, Object)>::new();

            for pair in &hl.pairs {
                let key = eval_expression_node(&pair.0, env.clone())?;
                let value = eval_expression_node(&pair.1, env.clone())?;
                result.push((key, value));
            }
            Ok(Object::new_hash(&result))
        }
    }
}

/// evaluator function for prefix expression node
fn eval_prefix_expression_node(operator: &str, right: &Object) -> Result<Object, EvaluationError> {
    match operator {
        "!" => eval_bang_operation_expression_node(right),
        "-" => eval_minus_operation_expression_node(right),
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
        Object::BooleanObject(bl) => Ok(Object::new_boolean(!(*bl).value)),
        Object::Null => Ok(Object::new_boolean(true)),
        _ => Ok(Object::new_boolean(false)),
    }
}

/// evaluator function for minus operation expression node
fn eval_minus_operation_expression_node(right: &Object) -> Result<Object, EvaluationError> {
    if let Object::IntegerObject(integer) = right {
        Ok(Object::new_integer(-integer.value))
    } else {
        Err(EvaluationError::UnknownOperator(format!(
            "-{}",
            right.object_type()
        )))
    }
}

/// evaluator function for infix expression node
fn eval_infix_expression_node(
    operator: &str,
    left: &Object,
    right: &Object,
) -> Result<Object, EvaluationError> {
    if let Object::IntegerObject(left_integer) = left {
        if let Object::IntegerObject(right_integer) = right {
            return eval_integer_infix_expression(operator, &left_integer, &right_integer);
        }
    }

    if let Object::StringObject(left_str) = left {
        if let Object::StringObject(right_str) = right {
            if operator == "+" {
                let mut s = String::new();
                s.push_str(&(*left_str).value);
                s.push_str(&(*right_str).value);
                return Ok(Object::new_string(&s));
            }
        }
    }

    match operator {
        "==" => Ok(Object::new_boolean(left == right)),
        "!=" => Ok(Object::new_boolean(left != right)),
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
    operator: &str,
    left: &Integer,
    right: &Integer,
) -> Result<Object, EvaluationError> {
    match operator {
        "+" => Ok(Object::new_integer(left.value + right.value)),
        "-" => Ok(Object::new_integer(left.value - right.value)),
        "*" => Ok(Object::new_integer(left.value * right.value)),
        "/" => Ok(Object::new_integer(left.value / right.value)),
        "<" => Ok(Object::new_boolean(left.value < right.value)),
        ">" => Ok(Object::new_boolean(left.value > right.value)),
        "==" => Ok(Object::new_boolean(left.value == right.value)),
        "!=" => Ok(Object::new_boolean(left.value != right.value)),
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
    if let Object::ArrayObject(ao) = left {
        if let Object::IntegerObject(io) = index {
            eval_array_index_expression(&ao, &io)
        } else {
            Err(EvaluationError::IndexOperatorNotSupported(
                left.object_type().into(),
            ))
        }
    } else if let Object::HashObject(ho) = left {
        eval_hash_index_expression(&ho, &index)
    } else {
        Err(EvaluationError::IndexOperatorNotSupported(
            left.object_type().into(),
        ))
    }
}

/// evaluator function for array index expression
#[allow(clippy::unnecessary_wraps)]
fn eval_array_index_expression(array: &Array, index: &Integer) -> Result<Object, EvaluationError> {
    let idx = index.value;
    let max = array.elements.len();

    if idx < 0 || idx >= (max as i64) {
        Ok(Object::Null)
    } else {
        Ok(array.elements[idx as usize].clone())
    }
}

/// evaluator function for hash
#[allow(clippy::unnecessary_wraps)]
fn eval_hash_index_expression(hash: &Hash, key: &Object) -> Result<Object, EvaluationError> {
    match hash.pairs.iter().find(|&k| k.0 == *key) {
        Some(o) => Ok(o.1.clone()),
        None => Ok(Object::Null),
    }
}

/// apply function
fn apply_function(function: &Object, args: &[Object]) -> Result<Object, EvaluationError> {
    match function {
        Object::FunctionObject(fnc) => {
            if fnc.parameters.len() != args.len() {
                return Err(EvaluationError::InvalidNumberOfArguments(
                    fnc.parameters.len(),
                    args.len(),
                ));
            }
            let extended_env = extend_environment(fnc.env.clone());
            for (idx, p) in fnc.parameters.iter().enumerate() {
                extended_env
                    .clone()
                    .borrow_mut()
                    .set(&p.string(), &args[idx]);
            }
            let evaluated = eval_statement_node(&fnc.body, extended_env)?;
            if let Object::ReturnValueObject(ro) = evaluated {
                Ok(ro.value)
            } else {
                Ok(evaluated)
            }
        }
        Object::BuiltinObject(bio) => (bio.builtin)(args.to_vec()).map_err(EvaluationError::from),
        _ => Err(EvaluationError::NotFunction(function.object_type().into())),
    }
}
