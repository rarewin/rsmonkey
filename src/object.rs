use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use thiserror::Error;

use crate::ast::{ExpressionNode, StatementNode};

/// object
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    IntegerObject(Box<Integer>),
    BooleanObject(Box<Boolean>),
    StringObject(Box<StringObj>),
    ReturnValueObject(Box<ReturnValue>),
    FunctionObject(Box<Function>),
    ArrayObject(Box<Array>),
    HashObject(Box<Hash>),
    BuiltinObject(Box<Builtin>),
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
            Object::IntegerObject(io) => write!(f, "{}", io.value),
            Object::BooleanObject(bo) => write!(f, "{}", bo.value),
            Object::StringObject(so) => write!(f, r##""{}""##, so.value),
            Object::ReturnValueObject(rvo) => write!(f, "{}", rvo.value),
            Object::FunctionObject(fo) => {
                let mut ret = String::new();
                ret.push_str("fn(");
                ret.push_str(
                    &((&fo.parameters)
                        .iter()
                        .map(|x| x.get_literal())
                        .collect::<Vec<String>>()
                        .join(", ")),
                );
                ret.push_str(") {\n");
                ret.push_str(&fo.body.string());
                ret.push_str(if fo.body.string().is_empty() {
                    "}"
                } else {
                    "\n}"
                });
                write!(f, "{}", ret)
            }
            Object::ArrayObject(ao) => {
                let mut ret = String::new();
                ret.push('[');
                ret.push_str(
                    &((&ao.elements)
                        .iter()
                        .map(|x| format!("{}", x))
                        .collect::<Vec<String>>()
                        .join(", ")),
                );
                ret.push(']');
                write!(f, "{}", ret)
            }
            Object::HashObject(ho) => {
                let mut ret = String::new();
                ret.push('{');
                ret.push_str(
                    &((&ho.pairs)
                        .iter()
                        .map(|x| format!("{}: {}", x.0, x.1))
                        .collect::<Vec<String>>()
                        .join(", ")),
                );
                ret.push('}');
                write!(f, "{}", ret)
            }
            Object::BuiltinObject(_) => write!(f, "builtin function"),
            Object::Null => write!(f, ""),
        }
    }
}

/// implementation of object
impl Object {
    /// object type function
    pub fn object_type(&self) -> &'static str {
        match self {
            Object::IntegerObject(_) => INTEGER_OBJ,
            Object::BooleanObject(_) => BOOLEAN_OBJ,
            Object::StringObject(_) => STRING_OBJ,
            Object::ReturnValueObject(_) => RETURN_VALUE_OBJ,
            Object::FunctionObject(_) => FUNCTION_OBJ,
            Object::ArrayObject(_) => ARRAY_OBJ,
            Object::HashObject(_) => HASH_OBJ,
            Object::BuiltinObject(_) => BUILTIN_OBJ,
            Object::Null => "(null)",
        }
    }

    /// create a new integer object
    pub fn new_integer(value: i64) -> Object {
        Object::IntegerObject(Box::new(Integer { value }))
    }

    /// create a new string object
    pub fn new_string(value: &str) -> Object {
        Object::StringObject(Box::new(StringObj {
            value: value.to_string(),
        }))
    }

    /// create a new boolean object
    pub fn new_boolean(value: bool) -> Object {
        if value {
            Object::BooleanObject(Box::new(TRUE))
        } else {
            Object::BooleanObject(Box::new(FALSE))
        }
    }

    /// create a new return value object
    pub fn new_return_value(value: Object) -> Object {
        Object::ReturnValueObject(Box::new(ReturnValue { value }))
    }

    /// create a new function object
    pub fn new_function(
        parameters: &[ExpressionNode],
        body: &StatementNode,
        env: Rc<RefCell<Environment>>,
    ) -> Object {
        Object::FunctionObject(Box::new(Function {
            parameters: parameters.to_vec(),
            body: body.clone(),
            env,
        }))
    }

    /// create a new array object
    pub fn new_array(elements: &[Object]) -> Object {
        Object::ArrayObject(Box::new(Array {
            elements: elements.to_vec(),
        }))
    }

    /// create a new hash object
    pub fn new_hash(pairs: &[(Object, Object)]) -> Object {
        Object::HashObject(Box::new(Hash {
            pairs: pairs.to_vec(),
        }))
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

/// const boolan object
pub const TRUE: Boolean = Boolean { value: true };
pub const FALSE: Boolean = Boolean { value: false };

/// struct for Integer object
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Integer {
    pub value: i64,
}

/// struct for Boolean object
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Boolean {
    pub value: bool,
}

/// struct for String object
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StringObj {
    pub value: String,
}

/// struct for Return Value object
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ReturnValue {
    pub value: Object,
}

/// struct for Array object
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Array {
    pub elements: Vec<Object>,
}

// struct for Hash object
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Hash {
    pub pairs: Vec<(Object, Object)>,
}

/// struct for Function object
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub parameters: Vec<ExpressionNode>,
    pub body: StatementNode,
    pub env: Rc<RefCell<Environment>>,
}

/// struct for Builtin object
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Builtin {
    pub builtin: fn(Vec<Object>) -> Result<Object, ObjectError>,
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
            "len" => Ok(Object::BuiltinObject(Box::new(Builtin {
                builtin: builtin_len,
            }))),
            "first" => Ok(Object::BuiltinObject(Box::new(Builtin {
                builtin: builtin_first,
            }))),
            "last" => Ok(Object::BuiltinObject(Box::new(Builtin {
                builtin: builtin_last,
            }))),
            "rest" => Ok(Object::BuiltinObject(Box::new(Builtin {
                builtin: builtin_rest,
            }))),
            "push" => Ok(Object::BuiltinObject(Box::new(Builtin {
                builtin: builtin_push,
            }))),
            "puts" => Ok(Object::BuiltinObject(Box::new(Builtin {
                builtin: builtin_puts,
            }))),
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
        Object::StringObject(so) => Ok(Object::new_integer(so.value.len() as i64)),
        Object::ArrayObject(ao) => Ok(Object::new_integer(ao.elements.len() as i64)),
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

    if let Object::ArrayObject(ao) = &parameters[0] {
        if !ao.elements.is_empty() {
            Ok(ao.elements[0].clone())
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

    if let Object::ArrayObject(ao) = &parameters[0] {
        if !ao.elements.is_empty() {
            Ok(ao.elements[ao.elements.len() - 1].clone())
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

    if let Object::ArrayObject(ao) = &parameters[0] {
        if !ao.elements.is_empty() {
            Ok(Object::new_array(&ao.elements[1..].to_vec()))
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

    if let Object::ArrayObject(ao) = &parameters[0] {
        let mut a = ao.elements.clone();
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
