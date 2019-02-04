/// object
#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    IntegerObject(Box<Integer>),
    BooleanObject(Box<Boolean>),
    ReturnValueObject(Box<ReturnValue>),
    ErrorObject(Box<Error>),
    Null,
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
#[derive(Debug, PartialEq, Eq)]
pub struct Integer {
    pub value: i64,
}

/// struct for Boolean object
#[derive(Debug, PartialEq, Eq)]
pub struct Boolean {
    pub value: bool,
}

/// struct for Return Value object
#[derive(Debug, PartialEq, Eq)]
pub struct ReturnValue {
    pub value: Object,
}

/// struct for Error object
#[derive(Debug, PartialEq, Eq)]
pub struct Error {
    pub message: String,
}

/// implementation of Integer object
impl Integer {
    /// inspect function
    pub fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    /// object type function
    pub fn object_type(&self) -> &'static str {
        INTEGER_OBJ
    }
}

/// implementation of Boolean object
impl Boolean {
    /// inspect function
    pub fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    /// object type function
    pub fn object_type(&self) -> &'static str {
        BOOLEAN_OBJ
    }
}

/// implementstion of Return Value object
impl ReturnValue {
    /// inspect function
    pub fn inspect(&self) -> String {
        format!(
            "{}",
            match &self.value {
                Object::IntegerObject(io) => (*io).inspect(),
                Object::BooleanObject(bo) => (*bo).inspect(),
                Object::Null => "Null".to_string(),
                _ => "(invalid type)".to_string(),
            }
        )
    }

    /// object type function
    pub fn object_type(&self) -> &'static str {
        RETURN_VALUE_OBJ
    }
}

/// implementstion of Error object
impl Error {
    /// inspect function
    pub fn inspect(&self) -> String {
        format!("ERROR: {}", self.message)
    }

    /// object type function
    pub fn object_type(&self) -> &'static str {
        ERROR_OBJ
    }
}
