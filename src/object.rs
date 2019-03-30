use crate::ast::{ExpressionNode, StatementNode};
use std::collections::HashMap;

/// object
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    IntegerObject(Box<Integer>),
    BooleanObject(Box<Boolean>),
    StringObject(Box<StringObj>),
    ReturnValueObject(Box<ReturnValue>),
    FunctionObject(Box<Function>),
    ArrayObject(Box<Array>),
    BuiltinObject(Box<Builtin>),
    ErrorObject(Box<Error>),
    Null,
}

/// implementation of object
impl Object {
    /// inspect function
    pub fn inspect(&self) -> String {
        match self {
            Object::IntegerObject(io) => io.value.to_string(),
            Object::BooleanObject(bo) => bo.value.to_string(),
            Object::StringObject(so) => so.value.to_string(),
            Object::ReturnValueObject(rvo) => rvo.value.inspect().into(),
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
            Object::ArrayObject(ao) => {
                let mut ret = String::new();
                ret.push_str("[");
                ret.push_str(
                    &((&ao.elements)
                        .into_iter()
                        .map(|x| x.inspect())
                        .collect::<Vec<String>>()
                        .join(", ")),
                );
                ret.push_str("]");
                ret
            }
            Object::BuiltinObject(_) => "builtin function".into(),
            Object::ErrorObject(eo) => format!("ERROR: {}", eo.message),
            Object::Null => "(null)".into(),
        }
    }

    /// object type function
    pub fn object_type(&self) -> &'static str {
        match self {
            Object::IntegerObject(_) => INTEGER_OBJ,
            Object::BooleanObject(_) => BOOLEAN_OBJ,
            Object::StringObject(_) => STRING_OBJ,
            Object::ReturnValueObject(_) => RETURN_VALUE_OBJ,
            Object::ErrorObject(_) => ERROR_OBJ,
            Object::FunctionObject(_) => FUNCTION_OBJ,
            Object::ArrayObject(_) => ARRAY_OBJ,
            Object::BuiltinObject(_) => BUILTIN_OBJ,
            Object::Null => "(null)",
        }
    }

    /// create a new integer object
    pub fn new_integer(value: i64) -> Object {
        Object::IntegerObject(Box::new(Integer { value }))
    }

    /// create a new string object
    pub fn new_string(value: &String) -> Object {
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

    /// create a new array object
    pub fn new_array(elements: &Vec<Object>) -> Object {
        Object::ArrayObject(Box::new(Array {
            elements: elements.to_vec(),
        }))
    }
}

/// object type strings
pub const INTEGER_OBJ: &'static str = "INTEGER";
pub const BOOLEAN_OBJ: &'static str = "BOOLEAN";
pub const STRING_OBJ: &'static str = "STRING";
pub const RETURN_VALUE_OBJ: &'static str = "RETURN_VALUE";
pub const ERROR_OBJ: &'static str = "ERROR";
pub const FUNCTION_OBJ: &'static str = "FUNCTION";
pub const ARRAY_OBJ: &'static str = "ARRAY";
pub const BUILTIN_OBJ: &'static str = "BUILTIN";

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

/// struct for Builtin object
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Builtin {
    pub builtin: fn(Vec<Object>) -> Object,
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
    pub fn get(&self, key: &str) -> Object {
        match key {
            // builtins
            "len" => Object::BuiltinObject(Box::new(Builtin {
                builtin: builtin_len,
            })),
            // normal function
            _ => match self {
                Environment::Env { store, outer } => match store.get(key) {
                    Some(o) => o.clone(),
                    _ => outer.get(key),
                },
                Environment::NoEnv => Object::new_error(format!("identifier not found: {}", key)),
            },
        }
    }
}

/// builtin function "len"
fn builtin_len(parameters: Vec<Object>) -> Object {
    if parameters.len() != 1 {
        Object::new_error(format!(
            "wrong number of arguments. got={}, expected=1",
            parameters.len()
        ))
    } else if let Object::StringObject(so) = &parameters[0] {
        Object::new_integer(so.value.len() as i64)
    } else {
        Object::new_error(format!(
            "argument to `len` not supported, got {}",
            &parameters[0].object_type()
        ))
    }
}
