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
}

/// struct for integer literal
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
}

/// struct for string literal
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct StringLiteral {
    pub token: Token,
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
    pub right: ExpressionNode,
}

/// struct for infix expression
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: ExpressionNode,
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
            StatementNode::LetStatementNode(ls) => (&ls.token).into(),
            StatementNode::ReturnStatementNode(rs) => (&rs.token).into(),
            StatementNode::ExpressionStatementNode(es) => (&es.token).into(),
            StatementNode::BlockStatementNode(bs) => {
                if !bs.statements.is_empty() {
                    bs.statements[0].get_literal()
                } else {
                    "(empty block statement)".into()
                }
            }
        }
    }
}

impl From<StatementNode> for String {
    fn from(sn: StatementNode) -> Self {
        Self::from(&sn)
    }
}

impl From<&StatementNode> for String {
    fn from(sn: &StatementNode) -> Self {
        match sn {
            StatementNode::LetStatementNode(ls) => {
                format!(
                    "{} {} = {};",
                    ls.token,
                    ls.name.token,
                    String::from(&ls.value),
                )
            }
            StatementNode::ReturnStatementNode(rs) => {
                format!("return {};", String::from(&rs.return_value))
            }
            StatementNode::ExpressionStatementNode(es) => String::from(&es.expression),
            StatementNode::BlockStatementNode(bs) => {
                bs.statements.iter().map(String::from).collect()
            }
        }
    }
}

/// implementation of expression node
impl ExpressionNode {
    /// get the token's literal
    pub fn get_literal(&self) -> String {
        match self {
            ExpressionNode::IdentifierNode(idn) => (&idn.token).into(),
            ExpressionNode::IntegerLiteralNode(iln) => (&iln.token).into(),
            ExpressionNode::FunctionLiteralNode(fln) => (&fln.token).into(),
            ExpressionNode::PrefixExpressionNode(pen) => (&pen.token).into(),
            ExpressionNode::InfixExpressionNode(ien) => (&ien.token).into(),
            ExpressionNode::BooleanExpressionNode(ben) => (&ben.token).into(),
            ExpressionNode::CallExpressionNode(cen) => (&cen.token).into(),
            _ => panic!("not supported string(): {:?}", self),
        }
    }
}

impl From<ExpressionNode> for String {
    fn from(en: ExpressionNode) -> Self {
        Self::from(&en)
    }
}

impl From<&ExpressionNode> for String {
    fn from(en: &ExpressionNode) -> Self {
        match en {
            ExpressionNode::IdentifierNode(_) | ExpressionNode::IntegerLiteralNode(_) => {
                en.get_literal()
            }
            ExpressionNode::FunctionLiteralNode(fln) => format!(
                "{}({}){}",
                &en.get_literal(),
                &((&fln.parameters)
                    .iter()
                    .map(String::from)
                    .collect::<Vec<String>>()
                    .join(", ")),
                if let Some(body) = &fln.body {
                    String::from(body)
                } else {
                    "".into()
                }
            ),
            ExpressionNode::PrefixExpressionNode(pen) => {
                format!("({}{})", pen.token, String::from(&pen.right))
            }
            ExpressionNode::InfixExpressionNode(ien) => format!(
                "({} {} {})",
                String::from(&ien.left),
                ien.token,
                String::from(&ien.right),
            ),
            ExpressionNode::BooleanExpressionNode(ben) => (&ben.token).into(),
            ExpressionNode::IfExpressionNode(ien) => format!(
                "if {} {}{}",
                String::from(&ien.condition),
                if let Some(consequence) = &ien.consequence {
                    String::from(consequence)
                } else {
                    "".into()
                },
                if let Some(alternative) = &ien.alternative {
                    format!(" else {}", String::from(alternative))
                } else {
                    "".into()
                }
            ),
            ExpressionNode::CallExpressionNode(cen) => format!(
                "{}({})",
                String::from(&cen.function),
                &((&cen.arguments)
                    .iter()
                    .map(String::from)
                    .collect::<Vec<String>>()
                    .join(", "))
            ),
            ExpressionNode::ArrayLiteralNode(al) => format!(
                "[{}]",
                &((&al.elements)
                    .iter()
                    .map(String::from)
                    .collect::<Vec<String>>()
                    .join(", "))
            ),
            ExpressionNode::IndexExpressionNode(ien) => {
                format!(
                    "({}[{}])",
                    String::from(&ien.left),
                    String::from(&ien.index)
                )
            }
            ExpressionNode::HashLiteralNode(hln) => {
                format!(
                    "{{{}}}",
                    &((&hln.pairs)
                        .iter()
                        .map(|x| format!("{}:{}", String::from(&x.0), String::from(&x.1)))
                        .collect::<Vec<String>>()
                        .join(", "))
                )
            }
            _ => panic!("not supported string(): {:?}", en),
        }
    }
}
