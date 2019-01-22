/// object
#[derive(Debug)]
pub enum Object {
    IntegerObject(Integer),
    Null,
}

/// object type
#[derive(Debug)]
pub enum ObjectType {
    IntegerType,
}

/// struct for Integer object
#[derive(Debug)]
pub struct Integer {
    pub value: i64,
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
