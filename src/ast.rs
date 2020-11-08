use crate::token::Token;

/// statement node
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StatementNode {
    LetStatementNode(Box<LetStatement>),
    ReturnStatementNode(Box<ReturnStatement>),
    ExpressionStatementNode(Box<ExpressionStatement>),
    BlockStatementNode(Box<BlockStatement>),
}

/// expression node
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExpressionNode {
    IdentifierNode(Box<Identifier>),
    IntegerLiteralNode(Box<IntegerLiteral>),
    StringLiteralNode(Box<StringLiteral>),
    FunctionLiteralNode(Box<FunctionLiteral>),
    PrefixExpressionNode(Box<PrefixExpression>),
    InfixExpressionNode(Box<InfixExpression>),
    BooleanExpressionNode(Box<Boolean>),
    IfExpressionNode(Box<IfExpression>),
    CallExpressionNode(Box<CallExpression>),
    ArrayLiteralNode(Box<ArrayLiteral>),
    HashLiteralNode(Box<HashLiteral>),
    IndexExpressionNode(Box<IndexExpression>),
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

/// struct for string literal
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StringLiteral {
    pub token: Token,
    pub value: String,
}

/// struct for function literal
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<ExpressionNode>,
    pub body: Option<StatementNode>,
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
    pub consequence: Option<StatementNode>,
    pub alternative: Option<StatementNode>,
}

/// struct for call expression
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct CallExpression {
    pub token: Token,
    pub function: ExpressionNode,
    pub arguments: Vec<ExpressionNode>,
}

/// struct for array literal
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct ArrayLiteral {
    pub token: Token,
    pub elements: Vec<ExpressionNode>,
}

/// struct for hash literal
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct HashLiteral {
    pub token: Token,
    pub pairs: Vec<(ExpressionNode, ExpressionNode)>,
}

/// struct for index expression
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IndexExpression {
    pub token: Token,
    pub left: ExpressionNode,
    pub index: ExpressionNode,
}

/// implementation of statement node
impl StatementNode {
    /// get token's literal
    pub fn get_literal(&self) -> String {
        match self {
            StatementNode::LetStatementNode(ls) => ls.token.get_literal(),
            StatementNode::ReturnStatementNode(rs) => rs.token.get_literal(),
            StatementNode::ExpressionStatementNode(es) => es.token.get_literal(),
            StatementNode::BlockStatementNode(bs) => {
                if !bs.statements.is_empty() {
                    bs.statements[0].get_literal()
                } else {
                    "(empty block statement)".into()
                }
            }
        }
    }

    /// get string of the statement
    pub fn string(&self) -> String {
        match self {
            StatementNode::LetStatementNode(ls) => format!(
                "{} {} = {};",
                ls.token.get_literal(),
                ls.name.token.get_literal(),
                &ls.value.string(),
            ),
            StatementNode::ReturnStatementNode(rs) => {
                format!("return {};", &rs.return_value.string(),)
            }
            StatementNode::ExpressionStatementNode(es) => es.expression.string(),
            StatementNode::BlockStatementNode(bs) => {
                bs.statements.iter().map(|s| s.string()).collect()
            }
        }
    }
}

/// implementation of expression node
impl ExpressionNode {
    /// get string of the expression node
    pub fn string(&self) -> String {
        match self {
            ExpressionNode::IdentifierNode(_) | ExpressionNode::IntegerLiteralNode(_) => {
                self.get_literal()
            }
            ExpressionNode::FunctionLiteralNode(fln) => format!(
                "{}({}){}",
                &self.get_literal(),
                &((&fln.parameters)
                    .iter()
                    .map(|x| x.string())
                    .collect::<Vec<String>>()
                    .join(", ")),
                if let Some(body) = &fln.body {
                    body.string()
                } else {
                    "".into()
                }
            ),
            ExpressionNode::PrefixExpressionNode(pen) => {
                format!("({}{})", pen.operator, &pen.right.string())
            }
            ExpressionNode::InfixExpressionNode(ien) => format!(
                "({} {} {})",
                ien.left.string(),
                ien.operator,
                ien.right.string(),
            ),
            ExpressionNode::BooleanExpressionNode(ben) => ben.token.get_literal(),
            ExpressionNode::IfExpressionNode(ien) => format!(
                "if {} {}{}",
                ien.condition.string(),
                if let Some(consequence) = &ien.consequence {
                    consequence.string()
                } else {
                    "".into()
                },
                if let Some(alternative) = &ien.alternative {
                    format!(" else {}", alternative.string())
                } else {
                    "".into()
                }
            ),
            ExpressionNode::CallExpressionNode(cen) => format!(
                "{}({})",
                &cen.function.string(),
                &((&cen.arguments)
                    .iter()
                    .map(|x| x.string())
                    .collect::<Vec<String>>()
                    .join(", "))
            ),
            ExpressionNode::ArrayLiteralNode(al) => format!(
                "[{}]",
                &((&al.elements)
                    .iter()
                    .map(|x| x.string())
                    .collect::<Vec<String>>()
                    .join(", "))
            ),
            ExpressionNode::IndexExpressionNode(ien) => {
                format!("({}[{}])", &ien.left.string(), &ien.index.string())
            }
            ExpressionNode::HashLiteralNode(hln) => format!(
                "{{{}}}",
                &((&hln.pairs)
                    .iter()
                    .map(|x| format!("{}:{}", x.0.string(), x.1.string()))
                    .collect::<Vec<String>>()
                    .join(", "))
            ),

            _ => panic!("not supported string(): {:?}", self),
        }
    }

    /// get the token's literal
    pub fn get_literal(&self) -> String {
        match self {
            ExpressionNode::IdentifierNode(idn) => (*idn).token.get_literal(),
            ExpressionNode::IntegerLiteralNode(iln) => (*iln).token.get_literal(),
            ExpressionNode::FunctionLiteralNode(fln) => (*fln).token.get_literal(),
            ExpressionNode::PrefixExpressionNode(pen) => (*pen).token.get_literal(),
            ExpressionNode::InfixExpressionNode(ien) => (*ien).token.get_literal(),
            ExpressionNode::BooleanExpressionNode(ben) => (*ben).token.get_literal(),
            ExpressionNode::CallExpressionNode(cen) => (*cen).token.get_literal(),
            _ => panic!("not supported string(): {:?}", self),
        }
    }
}
