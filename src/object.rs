use crate::ast::{ExpressionNode, StatementNode};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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
            Object::ReturnValueObject(rvo) => rvo.value.inspect(),
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
                ret.push_str("\n}");
                ret
            }
            Object::ArrayObject(ao) => {
                let mut ret = String::new();
                ret.push_str("[");
                ret.push_str(
                    &((&ao.elements)
                        .iter()
                        .map(|x| x.inspect())
                        .collect::<Vec<String>>()
                        .join(", ")),
                );
                ret.push_str("]");
                ret
            }
            Object::HashObject(ho) => {
                let mut ret = String::new();
                ret.push_str("{");
                ret.push_str(
                    &((&ho.pairs)
                        .iter()
                        .map(|x| format!("{}: {}", x.0.inspect(), x.1.inspect()))
                        .collect::<Vec<String>>()
                        .join(", ")),
                );
                ret.push_str("}");
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

    /// create a new error object
    pub fn new_error(message: String) -> Object {
        Object::ErrorObject(Box::new(Error { message }))
    }

    /// create a new return value object
    pub fn new_return_value(value: Object) -> Object {
        Object::ReturnValueObject(Box::new(ReturnValue { value }))
    }

    /// create a new function object
    pub fn new_function(
        parameters: &[ExpressionNode],
        body: &StatementNode,
        env: Rc<Environment>,
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
    pub env: Rc<Environment>,
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
        store: Rc<RefCell<HashMap<String, Object>>>,
        outer: Rc<Environment>,
    },
    NoEnv,
}

/// implementation of Environment
impl Environment {
    /// create new Environment
    pub fn new() -> Environment {
        Environment::Env {
            store: Rc::new(RefCell::new(HashMap::<String, Object>::new())),
            outer: Rc::new(Environment::NoEnv),
        }
    }

    /// set an element to hash map
    pub fn set(&self, key: &str, value: &Object) {
        match self {
            Environment::Env { store, outer: _ } => {
                store.borrow_mut().insert(key.to_string(), value.clone())
            }
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
            "first" => Object::BuiltinObject(Box::new(Builtin {
                builtin: builtin_first,
            })),
            "last" => Object::BuiltinObject(Box::new(Builtin {
                builtin: builtin_last,
            })),
            "rest" => Object::BuiltinObject(Box::new(Builtin {
                builtin: builtin_rest,
            })),
            "push" => Object::BuiltinObject(Box::new(Builtin {
                builtin: builtin_push,
            })),
            "puts" => Object::BuiltinObject(Box::new(Builtin {
                builtin: builtin_puts,
            })),
            // normal function
            _ => match self {
                Environment::Env { store, outer } => match store.borrow().get(key) {
                    Some(o) => o.clone(),
                    _ => outer.get(key),
                },
                Environment::NoEnv => Object::new_error(format!("identifier not found: {}", key)),
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
pub fn extend_environment(inner: Rc<Environment>) -> Rc<Environment> {
    Rc::new(Environment::Env {
        store: Rc::new(RefCell::new(HashMap::<String, Object>::new())),
        outer: inner,
    })
}

/// builtin function "len"
fn builtin_len(parameters: Vec<Object>) -> Object {
    if parameters.len() != 1 {
        return Object::new_error(format!(
            "wrong number of arguments. got={}, expected=1",
            parameters.len()
        ));
    }

    match &parameters[0] {
        Object::StringObject(so) => Object::new_integer(so.value.len() as i64),
        Object::ArrayObject(ao) => Object::new_integer(ao.elements.len() as i64),
        _ => Object::new_error(format!(
            "argument to `len` not supported, got {}",
            &parameters[0].object_type()
        )),
    }
}

/// builtin function "first"
fn builtin_first(parameters: Vec<Object>) -> Object {
    if parameters.len() != 1 {
        return Object::new_error(format!(
            "wrong number of arguments. got={}, expected=1",
            parameters.len()
        ));
    }

    if let Object::ArrayObject(ao) = &parameters[0] {
        if !ao.elements.is_empty() {
            ao.elements[0].clone()
        } else {
            Object::Null
        }
    } else {
        Object::new_error(format!(
            "argument to `first` must be ARRAY, got {}",
            parameters[0].object_type()
        ))
    }
}

/// builtin function "last"
fn builtin_last(parameters: Vec<Object>) -> Object {
    if parameters.len() != 1 {
        return Object::new_error(format!(
            "wrong number of arguments. got={}, expected=1",
            parameters.len()
        ));
    }

    if let Object::ArrayObject(ao) = &parameters[0] {
        if !ao.elements.is_empty() {
            ao.elements[ao.elements.len() - 1].clone()
        } else {
            Object::Null
        }
    } else {
        Object::new_error(format!(
            "argument to `last` must be ARRAY, got {}",
            parameters[0].object_type()
        ))
    }
}

/// builtin function "rest"
fn builtin_rest(parameters: Vec<Object>) -> Object {
    if parameters.len() != 1 {
        return Object::new_error(format!(
            "wrong number of arguments. got={}, expected=1",
            parameters.len()
        ));
    }

    if let Object::ArrayObject(ao) = &parameters[0] {
        if !ao.elements.is_empty() {
            Object::new_array(&ao.elements[1..].to_vec())
        } else {
            Object::Null
        }
    } else {
        Object::Null
    }
}

/// builtin function "push"
fn builtin_push(parameters: Vec<Object>) -> Object {
    if parameters.len() != 2 {
        return Object::new_error(format!(
            "wrong number of arguments. got={}, expected=2",
            parameters.len()
        ));
    }

    if let Object::ArrayObject(ao) = &parameters[0] {
        let mut a = ao.elements.clone();
        a.push(parameters[1].clone());
        Object::new_array(&a)
    } else {
        Object::new_error(format!(
            "first argument to `last` must be ARRAY, got {}",
            parameters[0].object_type()
        ))
    }
}

/// buitin function "puts"
fn builtin_puts(parameters: Vec<Object>) -> Object {
    for p in parameters {
        println!("{}", p.inspect())
    }
    Object::Null
}
