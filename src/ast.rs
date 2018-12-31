use crate::token::Token;

/// statement node
#[derive(Debug)]
pub enum StatementNode {
    LetStatementNode(LetStatement),
    ReturnStatementNode(ReturnStatement),
    Null,
}

// expression node
#[derive(Debug)]
pub enum ExpressionNode {
    IdentifierNode(Identifier),
    Null,
}

/// struct for let statement
#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    // pub value: Expression,
}

/// struct for return statement
#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    // pub return_value: Expression,
}

/// struct for identifier
#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

/// struct for programs
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<StatementNode>,
}

impl LetStatement {
    /// get token's literal
    pub fn token_literal(&self) -> String {
        self.token.token_literal()
    }
}

impl Identifier {
    /// get token's literal
    pub fn token_literal(&self) -> String {
        self.token.token_literal()
    }
}

impl Program {
    /// constructer of Program
    pub fn new() -> Program {
        Program {
            statements: Vec::new(),
        }
    }

    /// get the first token's literal
    pub fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            match &self.statements[0] {
                StatementNode::LetStatementNode(s) => s.token_literal(),
                _ => panic!(),
            }
        } else {
            "".to_string()
        }
    }
}
