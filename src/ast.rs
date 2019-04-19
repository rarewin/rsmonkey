use crate::token::Token;
//
/// statement node
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StatementNode<'a> {
    LetStatementNode(Box<LetStatement<'a>>),
    ReturnStatementNode(Box<ReturnStatement<'a>>),
    ExpressionStatementNode(Box<ExpressionStatement<'a>>),
    BlockStatementNode(Box<BlockStatement<'a>>),
    ProgramStatementNode(Box<Program<'a>>),
    Null,
}

/// expression node
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExpressionNode<'a> {
    IdentifierNode(Box<Identifier<'a>>),
    IntegerLiteralNode(Box<IntegerLiteral<'a>>),
    StringLiteralNode(Box<StringLiteral<'a>>),
    FunctionLiteralNode(Box<FunctionLiteral<'a>>),
    PrefixExpressionNode(Box<PrefixExpression<'a>>),
    InfixExpressionNode(Box<InfixExpression<'a>>),
    BooleanExpressionNode(Box<Boolean<'a>>),
    IfExpressionNode(Box<IfExpression<'a>>),
    CallExpressionNode(Box<CallExpression<'a>>),
    ArrayLiteralNode(Box<ArrayLiteral<'a>>),
    IndexExpressionNode(Box<IndexExpression<'a>>),
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
    Index,       // array[index]
}

/// struct for let statement
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct LetStatement<'a> {
    pub token: Token<'a>,
    pub name: Identifier<'a>,
    pub value: ExpressionNode<'a>,
}

/// struct for return statement
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ReturnStatement<'a> {
    pub token: Token<'a>,
    pub return_value: ExpressionNode<'a>,
}

/// struct for expression statement
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ExpressionStatement<'a> {
    pub token: Token<'a>,
    pub expression: ExpressionNode<'a>,
}

/// struct for block statement
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct BlockStatement<'a> {
    pub token: Token<'a>,
    pub statements: Vec<StatementNode<'a>>,
}

/// struct for identifier
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Identifier<'a> {
    pub token: Token<'a>,
    pub value: &'a str,
}

/// struct for integer literal
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IntegerLiteral<'a> {
    pub token: Token<'a>,
    pub value: i64,
}

/// struct for string literal
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StringLiteral<'a> {
    pub token: Token<'a>,
    pub value: String,
}

/// struct for function literal
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionLiteral<'a> {
    pub token: Token<'a>,
    pub parameters: Vec<ExpressionNode<'a>>,
    pub body: StatementNode<'a>,
}

/// struct for prefix expression
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PrefixExpression<'a> {
    pub token: Token<'a>,
    pub operator: &'a str,
    pub right: ExpressionNode<'a>,
}

/// struct for infix expression
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InfixExpression<'a> {
    pub token: Token<'a>,
    pub left: ExpressionNode<'a>,
    pub operator: &'a str,
    pub right: ExpressionNode<'a>,
}

/// struct for boolean
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Boolean<'a> {
    pub token: Token<'a>,
    pub value: bool,
}

/// struft for if expression
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IfExpression<'a> {
    pub token: Token<'a>,
    pub condition: ExpressionNode<'a>,
    pub consequence: StatementNode<'a>,
    pub alternative: StatementNode<'a>,
}

/// struct for call expression
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CallExpression<'a> {
    pub token: Token<'a>,
    pub function: ExpressionNode<'a>,
    pub arguments: Vec<ExpressionNode<'a>>,
}

/// struct for array literal
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ArrayLiteral<'a> {
    pub token: Token<'a>,
    pub elements: Vec<ExpressionNode<'a>>,
}

/// struct for index expression
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IndexExpression<'a> {
    pub token: Token<'a>,
    pub left: ExpressionNode<'a>,
    pub index: ExpressionNode<'a>,
}

/// struct for programs
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program<'a> {
    pub statements: Vec<StatementNode<'a>>,
}

/// implementation of statement node
impl<'a> StatementNode<'a> {
    /// get token's literal
    pub fn token_literal(&self) -> &'a str {
        match self {
            StatementNode::LetStatementNode(ls) => ls.token.token_literal(),
            StatementNode::ReturnStatementNode(rs) => rs.token.token_literal(),
            StatementNode::ExpressionStatementNode(es) => es.token.token_literal(),
            StatementNode::BlockStatementNode(bs) => {
                if bs.statements.len() > 0 {
                    bs.statements[0].token_literal()
                } else {
                    "(empty block statement)"
                }
            }
            StatementNode::ProgramStatementNode(ps) => {
                if ps.statements.len() > 0 {
                    ps.statements[0].token_literal()
                } else {
                    "(empty program statement)"
                }
            }
            StatementNode::Null => "(null)",
        }
    }

    /// get string of the statement
    pub fn string(&self) -> String {
        match self {
            StatementNode::LetStatementNode(ls) => format!(
                "{} {} = {};",
                ls.token.token_literal(),
                ls.name.token.token_literal(),
                ls.value.string(),
            ),
            StatementNode::ReturnStatementNode(rs) => format!(
                "{} {}",
                rs.token.token_literal(),
                &rs.return_value.token_literal()
            ),
            StatementNode::ExpressionStatementNode(es) => es.expression.string().to_string(),
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
            StatementNode::Null => "(null)".to_string(),
        }
    }
}

/// implementation of expression node
impl<'a> ExpressionNode<'a> {
    /// get string of the expression node
    pub fn string(&self) -> String {
        match self {
            ExpressionNode::IdentifierNode(_) | ExpressionNode::IntegerLiteralNode(_) => {
                self.token_literal().to_string()
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
                format!("({}{})", pen.operator, pen.right.string())
            }
            ExpressionNode::InfixExpressionNode(ien) => format!(
                "({} {} {})",
                ien.left.string(),
                ien.operator,
                ien.right.string(),
            ),
            ExpressionNode::BooleanExpressionNode(ben) => ben.token.token_literal().to_string(),
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
            ExpressionNode::ArrayLiteralNode(al) => {
                let mut ret = String::new();

                ret.push_str("[");
                ret.push_str(
                    &((&al.elements)
                        .into_iter()
                        .map(|x| x.string())
                        .collect::<Vec<String>>()
                        .join(", ")),
                );
                ret.push_str("]");

                ret
            }
            ExpressionNode::IndexExpressionNode(ien) => {
                let mut ret = String::new();

                ret.push_str("(");
                ret.push_str(&ien.left.string());
                ret.push_str("[");
                ret.push_str(&ien.index.string());
                ret.push_str("]");
                ret.push_str(")");

                ret
            }
            _ => panic!("not supported string(): {:#?}", self),
        }
    }

    /// get the token's literal
    pub fn token_literal(&self) -> &'a str {
        match self {
            ExpressionNode::IdentifierNode(idn) => (*idn).token.token_literal(),
            ExpressionNode::IntegerLiteralNode(iln) => (*iln).token.token_literal(),
            ExpressionNode::FunctionLiteralNode(fln) => (*fln).token.token_literal(),
            ExpressionNode::PrefixExpressionNode(pen) => (*pen).token.token_literal(),
            ExpressionNode::InfixExpressionNode(ien) => (*ien).token.token_literal(),
            ExpressionNode::BooleanExpressionNode(ben) => (*ben).token.token_literal(),
            ExpressionNode::CallExpressionNode(cen) => (*cen).token.token_literal(),
            _ => panic!("not supported string(): {:#?}", self),
        }
    }
}

/// Program
impl<'a> Program<'a> {
    /// constructor of Program
    pub fn new() -> Program<'a> {
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
    pub fn token_literal(&self) -> &'a str {
        if self.statements.len() > 0 {
            self.statements[0].token_literal()
        } else {
            ""
        }
    }

    /// get statements
    pub fn get_statements(&self) -> &Vec<StatementNode> {
        &self.statements
    }
}
