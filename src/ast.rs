use crate::token::Token;

/// statement node
#[derive(Debug)]
pub enum StatementNode {
    LetStatementNode(Box<LetStatement>),
    ReturnStatementNode(Box<ReturnStatement>),
    ExpressionStatementNode(Box<ExpressionStatement>),
    Null,
}

/// expression node
#[derive(Debug)]
pub enum ExpressionNode {
    IdentifierNode(Box<Identifier>),
    Null,
}

/// operation precedence
#[derive(Debug, PartialEq, PartialOrd)]
pub enum OperationPrecedence {
    Lowest,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // function call
}

/// struct for let statement
#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: ExpressionNode,
}

/// struct for return statement
#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: ExpressionNode,
}

/// struct for expression statement
#[derive(Debug)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: ExpressionNode,
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
    /// get string of the statement
    pub fn string(&self) -> String {
        let mut ret = String::new();

        ret.push_str(&self.token_literal());
        ret.push_str(" ");
        ret.push_str(&self.name.string());
        ret.push_str(" = ");

        let v = match &self.value {
            ExpressionNode::IdentifierNode(idn) => idn.string(),
            _ => panic!(),
        };
        ret.push_str(&v);

        ret.push_str(";");

        return ret;
    }

    /// get token's literal
    pub fn token_literal(&self) -> String {
        self.token.token_literal()
    }
}

/// ExpressionStatement
impl ExpressionStatement {
    /// get string of the statement
    pub fn string(&self) -> String {
        "".to_string()
    }
}

/// ReturnStatement
impl ReturnStatement {
    /// get string of the statement
    pub fn string(&self) -> String {
        let mut ret = String::new();

        ret.push_str(&self.token.token_literal());
        ret.push_str(" ");

        let v = match &self.return_value {
            ExpressionNode::IdentifierNode(idn) => idn.string(),
            _ => panic!(),
        };

        ret.push_str(&v);

        return ret;
    }
}

impl Identifier {
    /// get string of the identifer
    pub fn string(&self) -> String {
        self.token_literal()
    }

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

    /// get strings of all statements
    pub fn string(&self) -> String {
        let mut ret = String::new();
        for stmt in &self.statements {
            let stmt_str = match stmt {
                StatementNode::LetStatementNode(s) => s.string(),
                _ => panic!(),
            };
            ret.push_str(&stmt_str);
        }
        return ret;
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
