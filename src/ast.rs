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

impl std::fmt::Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.token.fmt(f)
    }
}

/// struct for integer literal
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
}

impl std::fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.token.fmt(f)
    }
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

impl std::fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.token.fmt(f)
    }
}

/// struct for prefix expression
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub right: ExpressionNode,
}

impl std::fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.token.fmt(f)
    }
}

/// struct for infix expression
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub left: ExpressionNode,
    pub right: ExpressionNode,
}

impl std::fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.token.fmt(f)
    }
}

/// struct for boolean
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

impl std::fmt::Display for Boolean {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.token.fmt(f)
    }
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

impl std::fmt::Display for CallExpression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.token.fmt(f)
    }
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

impl std::fmt::Display for StatementNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            StatementNode::ExpressionStatementNode(es) => write!(f, "{}", es.token),
            _ => todo!("{:?}", self),
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

impl std::fmt::Display for ExpressionNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ExpressionNode::IdentifierNode(id) => id.fmt(f),
            ExpressionNode::IntegerLiteralNode(il) => il.fmt(f),
            ExpressionNode::FunctionLiteralNode(fl) => fl.fmt(f),
            ExpressionNode::PrefixExpressionNode(pe) => pe.fmt(f),
            ExpressionNode::InfixExpressionNode(ie) => ie.fmt(f),
            ExpressionNode::BooleanExpressionNode(b) => b.fmt(f),
            ExpressionNode::CallExpressionNode(ce) => ce.fmt(f),
            _ => todo!("{:?}", self),
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
            ExpressionNode::IdentifierNode(idn) => (&idn.token).into(),
            ExpressionNode::IntegerLiteralNode(iln) => (&iln.token).into(),
            ExpressionNode::FunctionLiteralNode(fln) => format!(
                "{}({}){}",
                &fln.token,
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
            _ => todo!("not supported: {:?}", en),
        }
    }
}
