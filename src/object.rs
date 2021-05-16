use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use thiserror::Error;

use crate::ast::{ExpressionNode, StatementNode};
use crate::token::Token;

/// object
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    String(String),
    ReturnValue(Box<Object>),
    Function {
        parameters: Vec<ExpressionNode>,
        body: StatementNode,
        env: Rc<RefCell<Environment>>,
    },
    Array(Vec<Object>),
    Hash(Vec<(Object, Object)>),
    Builtin(fn(Vec<Object>) -> Result<Object, ObjectError>),
    Null,
}

/// error of object
#[derive(Debug, Error)]
pub enum ObjectError {
    #[error("identifier not found: {0}")]
    IdentifierNotFound(String),
    #[error("wrong number of arguments. got={0}, expected={1}")]
    WrongNumberOfArguments(usize, usize),
    #[error("first argument to `last` must be ARRAY, got {0}")]
    InvalidArgumentForLast(String),
    #[error("argument to `first` must be ARRAY, got {0}")]
    InvalidArgumentForFitst(String),
    #[error("argument to `len` not supported, got {0}")]
    InvalidArgumentForLen(String),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(value) => value.fmt(f),
            Object::Boolean(value) => value.fmt(f),
            Object::String(value) => write!(f, r##""{}""##, value),
            Object::ReturnValue(value) => value.fmt(f),
            Object::Function {
                parameters,
                body,
                env: _,
            } => {
                let mut ret = String::new();
                ret.push_str("fn(");
                ret.push_str(
                    &((&parameters)
                        .iter()
                        .map(|x| format!("{}", x))
                        .collect::<Vec<String>>()
                        .join(", ")),
                );
                ret.push_str(") {\n");
                let b = String::from(body);
                ret.push_str(&b);
                ret.push_str(if b.is_empty() { "}" } else { "\n}" });
                write!(f, "{}", ret)
            }
            Object::Array(ao) => {
                let mut ret = String::new();
                ret.push('[');
                ret.push_str(
                    &((&ao)
                        .iter()
                        .map(|x| format!("{}", x))
                        .collect::<Vec<String>>()
                        .join(", ")),
                );
                ret.push(']');
                write!(f, "{}", ret)
            }
            Object::Hash(ho) => {
                let mut ret = String::new();
                ret.push('{');
                ret.push_str(
                    &((ho)
                        .iter()
                        .map(|x| format!("{}: {}", x.0, x.1))
                        .collect::<Vec<String>>()
                        .join(", ")),
                );
                ret.push('}');
                write!(f, "{}", ret)
            }
            Object::Builtin(_) => write!(f, "builtin function"),
            Object::Null => write!(f, ""),
        }
    }
}

impl From<Token> for Object {
    fn from(token: Token) -> Self {
        Self::from(&token)
    }
}

impl From<&Token> for Object {
    fn from(token: &Token) -> Self {
        match token {
            Token::Int(i) => Object::from(*i),
            Token::String(s) => Object::from(s),
            Token::Boolean(b) => Object::from(*b),
            _ => todo!("{:?}", token),
        }
    }
}

impl From<i64> for Object {
    fn from(value: i64) -> Self {
        Object::Integer(value)
    }
}

impl From<&String> for Object {
    fn from(value: &String) -> Self {
        Object::String(value.to_string())
    }
}

impl From<&str> for Object {
    fn from(value: &str) -> Self {
        Object::String(value.to_string())
    }
}

impl From<bool> for Object {
    fn from(value: bool) -> Self {
        Object::Boolean(value)
    }
}

/// implementation of object
impl Object {
    /// object type function
    pub fn object_type(&self) -> &'static str {
        match self {
            Object::Integer(_) => INTEGER_OBJ,
            Object::Boolean(_) => BOOLEAN_OBJ,
            Object::String(_) => STRING_OBJ,
            Object::ReturnValue(_) => RETURN_VALUE_OBJ,
            Object::Function {
                parameters: _,
                body: _,
                env: _,
            } => FUNCTION_OBJ,
            Object::Array(_) => ARRAY_OBJ,
            Object::Hash(_) => HASH_OBJ,
            Object::Builtin(_) => BUILTIN_OBJ,
            Object::Null => "(null)",
        }
    }

    /// create a new return value object
    pub fn new_return_value(value: Object) -> Object {
        Object::ReturnValue(Box::new(value))
    }

    /// create a new function object
    pub fn new_function(
        parameters: &[ExpressionNode],
        body: &StatementNode,
        env: Rc<RefCell<Environment>>,
    ) -> Object {
        Object::Function {
            parameters: parameters.to_vec(),
            body: body.clone(),
            env,
        }
    }

    /// create a new array object
    pub fn new_array(elements: &[Object]) -> Object {
        Object::Array(elements.to_vec())
    }

    /// create a new hash object
    pub fn new_hash(pairs: &[(Object, Object)]) -> Object {
        Object::Hash(pairs.to_vec())
    }
}

/// object type strings
pub const INTEGER_OBJ: &str = "INTEGER";
pub const BOOLEAN_OBJ: &str = "BOOLEAN";
pub const STRING_OBJ: &str = "STRING";
pub const RETURN_VALUE_OBJ: &str = "RETURN_VALUE";
pub const ERROR_OBJ: &str = "ERROR";
pub const FUNCTION_OBJ: &str = "FUNCTION";
pub const ARRAY_OBJ: &str = "ARRAY";
pub const HASH_OBJ: &str = "HASH";
pub const BUILTIN_OBJ: &str = "BUILTIN";

/// struct for String object
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StringObj {
    pub value: String,
}

/// struct for Environment
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Environment>>>,
}

/// implementation of Environment
impl Environment {
    /// create new Environment
    pub fn new() -> Environment {
        Environment {
            store: HashMap::<String, Object>::new(),
            outer: None,
        }
    }

    /// set an element to hash map
    pub fn set(&mut self, key: &str, value: &Object) {
        self.store.insert(key.to_string(), value.clone());
    }

    /// get an element from hash map
    pub fn get(&self, key: &str) -> Result<Object, ObjectError> {
        match key {
            // builtins
            "len" => Ok(Object::Builtin(builtin_len)),
            "first" => Ok(Object::Builtin(builtin_first)),
            "last" => Ok(Object::Builtin(builtin_last)),
            "rest" => Ok(Object::Builtin(builtin_rest)),
            "push" => Ok(Object::Builtin(builtin_push)),
            "puts" => Ok(Object::Builtin(builtin_puts)),
            // normal function
            _ => match self.store.get(key) {
                Some(obj) => Ok(obj.clone()),
                None => match &self.outer {
                    Some(outer_env) => outer_env.clone().borrow().get(key),
                    None => Err(ObjectError::IdentifierNotFound(key.into())),
                },
            },
        }
    }
}

impl Default for Environment {
    fn default() -> Self {
        Self::new()
    }
}

/// extend an Environment
pub fn extend_environment(inner: Rc<RefCell<Environment>>) -> Rc<RefCell<Environment>> {
    Rc::new(RefCell::new(Environment {
        store: HashMap::<String, Object>::new(),
        outer: Some(inner),
    }))
}

/// builtin function "len"
fn builtin_len(parameters: Vec<Object>) -> Result<Object, ObjectError> {
    if parameters.len() != 1 {
        return Err(ObjectError::WrongNumberOfArguments(parameters.len(), 1));
    }

    match &parameters[0] {
        Object::String(s) => Ok(Object::from(s.len() as i64)),
        Object::Array(ao) => Ok(Object::from(ao.len() as i64)),
        _ => Err(ObjectError::InvalidArgumentForLen(
            parameters[0].object_type().into(),
        )),
    }
}

/// builtin function "first"
fn builtin_first(parameters: Vec<Object>) -> Result<Object, ObjectError> {
    if parameters.len() != 1 {
        return Err(ObjectError::WrongNumberOfArguments(parameters.len(), 1));
    }

    if let Object::Array(ao) = &parameters[0] {
        if !ao.is_empty() {
            Ok(ao[0].clone())
        } else {
            Ok(Object::Null)
        }
    } else {
        Err(ObjectError::InvalidArgumentForFitst(
            parameters[0].object_type().into(),
        ))
    }
}

/// builtin function "last"
fn builtin_last(parameters: Vec<Object>) -> Result<Object, ObjectError> {
    if parameters.len() != 1 {
        return Err(ObjectError::WrongNumberOfArguments(parameters.len(), 1));
    }

    if let Object::Array(ao) = &parameters[0] {
        if !ao.is_empty() {
            Ok(ao[ao.len() - 1].clone())
        } else {
            Ok(Object::Null)
        }
    } else {
        Err(ObjectError::InvalidArgumentForLast(
            parameters[0].object_type().into(),
        ))
    }
}

/// builtin function "rest"
fn builtin_rest(parameters: Vec<Object>) -> Result<Object, ObjectError> {
    if parameters.len() != 1 {
        return Err(ObjectError::WrongNumberOfArguments(parameters.len(), 1));
    }

    if let Object::Array(ao) = &parameters[0] {
        if !ao.is_empty() {
            Ok(Object::new_array(&ao[1..].to_vec()))
        } else {
            Ok(Object::Null)
        }
    } else {
        Ok(Object::Null)
    }
}

/// builtin function "push"
fn builtin_push(parameters: Vec<Object>) -> Result<Object, ObjectError> {
    if parameters.len() != 2 {
        return Err(ObjectError::WrongNumberOfArguments(parameters.len(), 2));
    }

    if let Object::Array(ao) = &parameters[0] {
        let mut a = ao.clone();
        a.push(parameters[1].clone());
        Ok(Object::new_array(&a))
    } else {
        Err(ObjectError::InvalidArgumentForLast(
            parameters[0].object_type().into(),
        ))
    }
}

/// buitin function "puts"
#[allow(clippy::unnecessary_wraps)]
fn builtin_puts(parameters: Vec<Object>) -> Result<Object, ObjectError> {
    for p in parameters {
        println!("{}", p);
    }
    Ok(Object::Null)
}
