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
    InfixExpressionNode(Box<InfixExpression>),
    BooleanExpressionNode(Box<Boolean>),
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

/// struct for infix expression
#[derive(Debug)]
pub struct InfixExpression {
    pub token: Token,
    pub left: ExpressionNode,
    pub operator: String,
    pub right: ExpressionNode,
}

/// struct for boolean
#[derive(Debug)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

/// struct for programs
#[derive(Debug)]
pub struct Program {
    pub statements: Vec<StatementNode>,
}

impl LetStatement {
    /// get string of the statement
    pub fn string(&self) -> String {
        format!(
            "{} {} = {};",
            &self.token_literal(),
            &self.name.string(),
            &extract_string_from_expression_node(&self.value)
        )
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
        extract_string_from_expression_node(&self.expression)
    }
}

/// ReturnStatement
impl ReturnStatement {
    /// get string of the statement
    pub fn string(&self) -> String {
        format!(
            "{} {}",
            &self.token.token_literal(),
            &extract_string_from_expression_node(&self.return_value),
        )
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
        format!(
            "({}{})",
            self.operator,
            extract_string_from_expression_node(&self.right)
        )
    }

    /// get token's literal
    pub fn token_literal(&self) -> String {
        self.token.token_literal()
    }
}

impl InfixExpression {
    /// get string of the infix expression
    pub fn string(&self) -> String {
        format!(
            "({} {} {})",
            extract_string_from_expression_node(&self.left),
            self.operator,
            extract_string_from_expression_node(&self.right),
        )
    }

    /// get token's literal
    pub fn token_literal(&self) -> String {
        self.token.token_literal()
    }
}

/// boolean
impl Boolean {
    /// get string of the boolean value
    pub fn string(&self) -> String {
        self.token.token_literal()
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
        StatementNode::ExpressionStatementNode(es) => es.string(),
        _ => panic!("unexpected node"),
    }
}

/// extract string from ExpressionNode
fn extract_string_from_expression_node(node: &ExpressionNode) -> String {
    match node {
        ExpressionNode::IdentifierNode(idn) => idn.string(),
        ExpressionNode::IntegerLiteralNode(iln) => iln.string(),
        ExpressionNode::PrefixExpressionNode(pen) => pen.string(),
        ExpressionNode::InfixExpressionNode(ien) => ien.string(),
        _ => panic!("unexpected node"),
    }
}
