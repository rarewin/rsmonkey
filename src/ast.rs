use crate::token::Token;

/// statement node
#[derive(Debug)]
pub enum StatementNode {
    LetStatementNode(Box<LetStatement>),
    ReturnStatementNode(Box<ReturnStatement>),
    ExpressionStatementNode(Box<ExpressionStatement>),
    BlockStatementNode(Box<BlockStatement>),
    ProgramStatementNode(Box<Program>),
    Null,
}

/// expression node
#[derive(Debug)]
pub enum ExpressionNode {
    IdentifierNode(Box<Identifier>),
    IntegerLiteralNode(Box<IntegerLiteral>),
    FunctionLiteralNode(Box<FunctionLiteral>),
    PrefixExpressionNode(Box<PrefixExpression>),
    InfixExpressionNode(Box<InfixExpression>),
    BooleanExpressionNode(Box<Boolean>),
    IfExpressionNode(Box<IfExpression>),
    CallExpressionNode(Box<CallExpression>),
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

/// struct for block statement
#[derive(Debug)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<StatementNode>,
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

/// struct for function literal
#[derive(Debug)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<ExpressionNode>,
    pub body: StatementNode,
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

/// struft for if expression
#[derive(Debug)]
pub struct IfExpression {
    pub token: Token,
    pub condition: ExpressionNode,
    pub consequence: StatementNode,
    pub alternative: StatementNode,
}

/// struct for call expression
#[derive(Debug)]
pub struct CallExpression {
    pub token: Token,
    pub function: ExpressionNode,
    pub arguments: Vec<ExpressionNode>,
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

/// BlockStatement
impl BlockStatement {
    /// get strings of block statements
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
            extract_string_from_statement_node(&self.statements[0])
        } else {
            "".to_string()
        }
    }
}

/// Identifier
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

/// integer literal
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

/// function literal
impl FunctionLiteral {
    /// get string of the expression
    pub fn string(&self) -> String {
        let mut ret = String::new();

        ret.push_str(&self.token_literal());
        ret.push_str("(");
        ret.push_str(
            &((&self.parameters)
                .into_iter()
                .map(|x| {
                    if let ExpressionNode::IdentifierNode(ident) = x {
                        ident.string()
                    } else {
                        panic!("unexpected data")
                    }
                })
                .collect::<Vec<String>>()
                .join(", ")),
        );
        ret.push_str(") ");
        ret.push_str(&extract_string_from_statement_node(&self.body));

        return ret;
    }

    /// get token's literal
    pub fn token_literal(&self) -> String {
        self.token.token_literal()
    }
}

/// prefix expression
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

/// infix expression
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

/// if expression
impl IfExpression {
    /// get string of the if expression
    pub fn string(&self) -> String {
        let mut ret = String::new();

        ret.push_str(&format!(
            "if {} {}",
            extract_string_from_expression_node(&self.condition),
            extract_string_from_statement_node(&self.consequence)
        ));

        if let StatementNode::BlockStatementNode(bs) = &self.alternative {
            ret.push_str(&format!(" else {}", bs.string()))
        }

        return ret;
    }
}

/// call expression
impl CallExpression {
    /// get string of the call expression
    pub fn string(&self) -> String {
        let mut ret = String::new();
        let fs = match &self.function {
            ExpressionNode::IdentifierNode(f) => f.string(),
            _ => panic!("unexpected node"),
        };

        ret.push_str(&fs);
        ret.push_str("(");
        ret.push_str(
            &((&self.arguments)
                .into_iter()
                .map(|x| extract_string_from_expression_node(x))
                .collect::<Vec<String>>()
                .join(", ")),
        );
        ret.push_str(")");

        return ret;
    }

    /// get tokken's literal
    pub fn token_literal(&self) -> String {
        self.token.token_literal()
    }
}

/// Program
impl Program {
    /// constructor of Program
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
            extract_token_literal_from_statement_node(&self.statements[0])
        } else {
            "".to_string()
        }
    }
}

/// extract token literal from StatementNode
fn extract_token_literal_from_statement_node(node: &StatementNode) -> String {
    match node {
        StatementNode::LetStatementNode(ls) => ls.token.token_literal(),
        StatementNode::ExpressionStatementNode(es) => es.token.token_literal(),
        _ => panic!("unexpected node"),
    }
}

/// extract string from StatementNode
fn extract_string_from_statement_node(node: &StatementNode) -> String {
    match node {
        StatementNode::LetStatementNode(ls) => ls.string(),
        StatementNode::ExpressionStatementNode(es) => es.string(),
        StatementNode::BlockStatementNode(bs) => bs.string(),
        StatementNode::Null => "(null)".to_string(),
        _ => panic!("unexpected node"),
    }
}

/// extract string from ExpressionNode
fn extract_string_from_expression_node(node: &ExpressionNode) -> String {
    match node {
        ExpressionNode::IdentifierNode(idn) => idn.string(),
        ExpressionNode::IntegerLiteralNode(iln) => iln.string(),
        ExpressionNode::FunctionLiteralNode(fln) => fln.string(),
        ExpressionNode::PrefixExpressionNode(pen) => pen.string(),
        ExpressionNode::InfixExpressionNode(ien) => ien.string(),
        ExpressionNode::BooleanExpressionNode(ben) => ben.string(),
        ExpressionNode::IfExpressionNode(ien) => ien.string(),
        ExpressionNode::CallExpressionNode(cen) => cen.string(),
        ExpressionNode::Null => "(null)".to_string(),
        // _ => panic!("unexpected node (please implement extract_string_from_expression_node() for this node)"),
    }
}
