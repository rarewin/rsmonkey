/// object
#[derive(Debug)]
pub enum Object {
    IntegerObject(Integer),
    BooleanObject(Boolean),
    Null,
}

/// object type
#[derive(Debug)]
pub enum ObjectType {
    IntegerType,
    BooleanType,
}

/// const boolan object
pub const TRUE: Boolean = Boolean { value: true };
pub const FALSE: Boolean = Boolean { value: false };

/// struct for Integer object
#[derive(Debug)]
pub struct Integer {
    pub value: i64,
}

/// struct for Boolean object
#[derive(Debug)]
pub struct Boolean {
    pub value: bool,
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
