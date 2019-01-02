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
    IntegerLiteralNode(Box<IntegerLiteral>),
    PrefixExpressionNode(Box<PrefixExpression>),
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

/// struct for integer literal
#[derive(Debug)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

/// struct for prefix expression
#[derive(Debug)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: ExpressionNode,
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
        ret.push_str(&extract_string_from_expression_node(&self.value));

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
        ret.push_str(&extract_string_from_expression_node(&self.return_value));

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

impl IntegerLiteral {
    /// get string of the integer literal
    pub fn string(&self) -> String {
        self.token_literal()
    }

    /// get token's literal
    pub fn token_literal(&self) -> String {
        self.token.token_literal()
    }
}

impl PrefixExpression {
    /// get string of the prefix expression
    pub fn string(&self) -> String {
        let mut ret = String::new();
        ret.push_str(&format!(
            "({}{})",
            self.operator,
            extract_string_from_expression_node(&self.right)
        ));
        return ret;
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
            ret.push_str(&extract_string_from_statement_node(&stmt));
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

/// extract string from StatementNode
fn extract_string_from_statement_node(node: &StatementNode) -> String {
    match node {
        StatementNode::LetStatementNode(ls) => ls.string(),
        _ => panic!("unexpected node"),
    }
}

/// extract string from ExpressionNode
fn extract_string_from_expression_node(node: &ExpressionNode) -> String {
    match node {
        ExpressionNode::IdentifierNode(idn) => idn.string(),
        ExpressionNode::IntegerLiteralNode(iln) => iln.string(),
        ExpressionNode::PrefixExpressionNode(pen) => pen.string(),
        _ => panic!("unexpected node"),
    }
}
