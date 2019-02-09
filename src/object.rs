use crate::ast::{ExpressionNode, StatementNode};
use std::collections::HashMap;

/// object
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    IntegerObject(Box<Integer>),
    BooleanObject(Box<Boolean>),
    ReturnValueObject(Box<ReturnValue>),
    FunctionObject(Box<Function>),
    ErrorObject(Box<Error>),
    Null,
}

/// implementation of object
impl Object {
    /// inspect function
    pub fn inspect(&self) -> String {
        match self {
            Object::IntegerObject(io) => format!("{}", io.value),
            Object::BooleanObject(bo) => format!("{}", bo.value),
            Object::ReturnValueObject(rvo) => format!("{}", rvo.value.inspect()),
            Object::FunctionObject(fo) => {
                let mut ret = String::new();
                ret.push_str("fn(");
                ret.push_str(
                    &((&fo.parameters)
                        .into_iter()
                        .map(|x| x.token_literal())
                        .collect::<Vec<String>>()
                        .join(", ")),
                );
                ret.push_str(") {\n");
                ret.push_str(&fo.body.string());
                ret.push_str("\n}");
                ret
            }
            Object::ErrorObject(eo) => format!("ERROR: {}", eo.message),
            Object::Null => "(null)".into(),
        }
    }

    /// object type function
    pub fn object_type(&self) -> &'static str {
        match self {
            Object::IntegerObject(_) => INTEGER_OBJ,
            Object::BooleanObject(_) => BOOLEAN_OBJ,
            Object::ReturnValueObject(_) => RETURN_VALUE_OBJ,
            Object::ErrorObject(_) => ERROR_OBJ,
            Object::FunctionObject(_) => FUNCTION_OBJ,
            Object::Null => "(null)",
        }
    }

    /// create a new integer object
    pub fn new_integer(value: i64) -> Object {
        Object::IntegerObject(Box::new(Integer { value }))
    }

    /// create a new boolean object
    pub fn new_boolean(value: bool) -> Object {
        if value {
            Object::BooleanObject(Box::new(TRUE))
        } else {
            Object::BooleanObject(Box::new(FALSE))
        }
    }

    /// create a new error object
    pub fn new_error(message: String) -> Object {
        Object::ErrorObject(Box::new({ Error { message } }))
    }

    /// create a new return value object
    pub fn new_return_value(value: Object) -> Object {
        Object::ReturnValueObject(Box::new(ReturnValue { value }))
    }

    /// create a new function object
    pub fn new_function(
        parameters: &Vec<ExpressionNode>,
        body: &StatementNode,
        env: &Environment,
    ) -> Object {
        Object::FunctionObject(Box::new(Function {
            parameters: parameters.to_vec(),
            body: body.clone(),
            env: env.clone(),
        }))
    }
}

/// object type strings
pub const INTEGER_OBJ: &'static str = "INTEGER";
pub const BOOLEAN_OBJ: &'static str = "BOOLEAN";
pub const RETURN_VALUE_OBJ: &'static str = "RETURN_VALUE";
pub const ERROR_OBJ: &'static str = "ERROR";
pub const FUNCTION_OBJ: &'static str = "FUNCTION";

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

/// struct for Return Value object
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ReturnValue {
    pub value: Object,
}

/// struct for Error object
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Error {
    pub message: String,
}

/// struct for Function object
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub parameters: Vec<ExpressionNode>,
    pub body: StatementNode,
    pub env: Environment,
}

/// struct for Environment
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Environment {
    Env {
        store: HashMap<String, Object>,
        outer: Box<Environment>,
    },
    NoEnv,
}

/// implementation of Environment
impl Environment {
    /// create new Environment
    pub fn new() -> Environment {
        Environment::Env {
            store: HashMap::<String, Object>::new(),
            outer: Box::new(Environment::NoEnv),
        }
    }

    /// extend an Environment
    pub fn extend(&self) -> Environment {
        Environment::Env {
            store: HashMap::<String, Object>::new(),
            outer: Box::new(self.clone()),
        }
    }

    /// set an element to hash map
    pub fn set(&mut self, key: &String, value: &Object) {
        match self {
            Environment::Env { store, outer: _ } => store.insert(key.to_string(), value.clone()),
            Environment::NoEnv => None,
        };
    }

    /// get an element from hash map
    pub fn get(&self, key: &String) -> Object {
        match self {
            Environment::Env { store, outer } => match store.get(key) {
                Some(o) => o.clone(),
                _ => outer.get(key),
            },
            Environment::NoEnv => Object::new_error(format!("identifier not found: {}", key)),
        }
    }
}
