use crate::token::Token;

/// statement node
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StatementNode {
    LetStatementNode(Box<LetStatement>),
    ReturnStatementNode(Box<ReturnStatement>),
    ExpressionStatementNode(Box<ExpressionStatement>),
    BlockStatementNode(Box<BlockStatement>),
    ProgramStatementNode(Box<Program>),
    Null,
}

/// expression node
#[derive(Debug, PartialEq, Eq, Clone)]
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
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: ExpressionNode,
}

/// struct for return statement
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: ExpressionNode,
}

/// struct for expression statement
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: ExpressionNode,
}

/// struct for block statement
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<StatementNode>,
}

/// struct for identifier
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}

/// struct for integer literal
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

/// struct for function literal
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<ExpressionNode>,
    pub body: StatementNode,
}

/// struct for prefix expression
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: ExpressionNode,
}

/// struct for infix expression
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: ExpressionNode,
    pub operator: String,
    pub right: ExpressionNode,
}

/// struct for boolean
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

/// struft for if expression
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: ExpressionNode,
    pub consequence: StatementNode,
    pub alternative: StatementNode,
}

/// struct for call expression
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: ExpressionNode,
    pub arguments: Vec<ExpressionNode>,
}

/// struct for programs
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub statements: Vec<StatementNode>,
}

/// implementation of statement node
impl StatementNode {
    /// get token's literal
    pub fn token_literal(&self) -> String {
        match self {
            StatementNode::LetStatementNode(ls) => ls.token.token_literal(),
            StatementNode::ReturnStatementNode(rs) => rs.token.token_literal(),
            StatementNode::ExpressionStatementNode(es) => es.token.token_literal(),
            StatementNode::BlockStatementNode(bs) => {
                if bs.statements.len() > 0 {
                    bs.statements[0].token_literal()
                } else {
                    "(empty block statement)".into()
                }
            }
            StatementNode::ProgramStatementNode(ps) => {
                if ps.statements.len() > 0 {
                    ps.statements[0].token_literal()
                } else {
                    "(empty program statement)".into()
                }
            }
            StatementNode::Null => "(null)".into(),
        }
    }

    /// get string of the statement
    pub fn string(&self) -> String {
        match self {
            StatementNode::LetStatementNode(ls) => format!(
                "{} {} = {};",
                ls.token.token_literal(),
                ls.name.token.token_literal(),
                &ls.value.string(),
            ),
            StatementNode::ReturnStatementNode(rs) => {
                format!("{} {}", rs.token.token_literal(), &rs.return_value.string(),)
            }
            StatementNode::ExpressionStatementNode(es) => es.expression.string(),
            StatementNode::BlockStatementNode(bs) => {
                let mut ret = String::new();
                for stmt in &bs.statements {
                    ret.push_str(&stmt.string());
                }
                ret
            }
            StatementNode::ProgramStatementNode(ps) => {
                let mut ret = String::new();
                for stmt in &ps.statements {
                    ret.push_str(&stmt.string());
                }
                ret
            }
            StatementNode::Null => "(null)".into(),
        }
    }
}

/// implementation of expression node
impl ExpressionNode {
    /// get string of the expression node
    pub fn string(&self) -> String {
        match self {
            ExpressionNode::IdentifierNode(_) | ExpressionNode::IntegerLiteralNode(_) => {
                self.token_literal()
            }
            ExpressionNode::FunctionLiteralNode(fln) => {
                let mut ret = String::new();
                ret.push_str(&self.token_literal());
                ret.push_str("(");
                ret.push_str(
                    &((&fln.parameters)
                        .into_iter()
                        .map(|x| x.string())
                        .collect::<Vec<String>>()
                        .join(", ")),
                );
                ret.push_str(") ");
                ret.push_str(&fln.body.string());
                return ret;
            }
            ExpressionNode::PrefixExpressionNode(pen) => {
                format!("({}{})", pen.operator, &pen.right.string())
            }
            ExpressionNode::InfixExpressionNode(ien) => format!(
                "({} {} {})",
                ien.left.string(),
                ien.operator,
                ien.right.string(),
            ),
            ExpressionNode::BooleanExpressionNode(ben) => ben.token.token_literal(),
            ExpressionNode::IfExpressionNode(ien) => {
                let mut ret = String::new();

                ret.push_str(&format!(
                    "if {} {}",
                    ien.condition.string(),
                    ien.consequence.string()
                ));

                if let StatementNode::BlockStatementNode(_) = ien.alternative {
                    ret.push_str(&format!(" else {}", ien.alternative.string()))
                }
                ret
            }
            ExpressionNode::CallExpressionNode(cen) => {
                let mut ret = String::new();

                ret.push_str(&cen.function.string());
                ret.push_str("(");
                ret.push_str(
                    &((&cen.arguments)
                        .into_iter()
                        .map(|x| x.string())
                        .collect::<Vec<String>>()
                        .join(", ")),
                );
                ret.push_str(")");

                ret
            }
            _ => "(unsupported string())".into(),
        }
    }

    /// get the token's literal
    pub fn token_literal(&self) -> String {
        match self {
            ExpressionNode::IdentifierNode(idn) => (*idn).token.token_literal(),
            ExpressionNode::IntegerLiteralNode(iln) => (*iln).token.token_literal(),
            ExpressionNode::FunctionLiteralNode(fln) => (*fln).token.token_literal(),
            ExpressionNode::PrefixExpressionNode(pen) => (*pen).token.token_literal(),
            ExpressionNode::InfixExpressionNode(ien) => (*ien).token.token_literal(),
            ExpressionNode::BooleanExpressionNode(ben) => (*ben).token.token_literal(),
            ExpressionNode::CallExpressionNode(cen) => (*cen).token.token_literal(),
            _ => "(unsupported token_literal())".into(),
        }
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
            ret.push_str(&stmt.string());
        }
        return ret;
    }

    /// get the first token's literal
    pub fn token_literal(&self) -> String {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            "".to_string()
        }
    }
}
