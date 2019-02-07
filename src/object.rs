use std::collections::HashMap;

/// object
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Object {
    IntegerObject(Box<Integer>),
    BooleanObject(Box<Boolean>),
    ReturnValueObject(Box<ReturnValue>),
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
}

/// object type strings
pub const INTEGER_OBJ: &'static str = "INTEGER";
pub const BOOLEAN_OBJ: &'static str = "BOOLEAN";
pub const RETURN_VALUE_OBJ: &'static str = "RETURN_VALUE";
pub const ERROR_OBJ: &'static str = "ERROR";

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

/// struct for Environment
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
}

/// implementation of Environment
impl Environment {
    /// create new Environment
    pub fn new() -> Environment {
        Environment {
            store: HashMap::<String, Object>::new(),
        }
    }

    /// set an element to hash map
    pub fn set(&mut self, key: &String, value: &Object) {
        self.store.insert(key.to_string(), value.clone());
    }

    /// get an element from hash map
    pub fn get(&self, key: &String) -> Object {
        match self.store.get(key) {
            Some(o) => o.clone(),
            _ => Object::new_error(format!("identifier not found: {}", key)),
        }
    }
}
