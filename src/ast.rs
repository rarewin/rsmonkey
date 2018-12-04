use crate::token::Token;

/// enum for nodes
pub enum Node {
    StatementNode(LetStatement),
    ExpressionNode(Identifier),
    Null,
}

/// struct for let statement
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    // pub value: Expression,
}

/// struct for identifier
pub struct Identifier {
    token: Token,
    value: String,
}

/// struct for programs
pub struct Program {
    pub statements: Vec<Node>,
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
                Node::StatementNode(s) => s.token_literal(),
                _ => panic!(),
            }
        } else {
            "".to_string()
        }
    }
}
