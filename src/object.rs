/// object
#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    IntegerObject(Box<Integer>),
    BooleanObject(Box<Boolean>),
    ReturnValueObject(Box<ReturnValue>),
    Null,
}

/// object type
#[derive(Debug)]
pub enum ObjectType {
    IntegerType,
    BooleanType,
    ReturnValueType,
}

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

/// implementation of Integer object
impl Integer {
    /// inspect function
    pub fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    /// object type function
    pub fn object_type(&self) -> ObjectType {
        ObjectType::IntegerType
    }
}

/// implementation of Boolean object
impl Boolean {
    /// inspect function
    pub fn inspect(&self) -> String {
        format!("{}", self.value)
    }

    /// object type function
    pub fn object_type(&self) -> ObjectType {
        ObjectType::BooleanType
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
    pub fn object_type(&self) -> ObjectType {
        ObjectType::ReturnValueType
    }
}
