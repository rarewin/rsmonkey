use crate::token::Token;

/// statement node
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum StatementNode {
    LetStatement {
        token: Token,
        name: Token,
        value: ExpressionNode,
    },
    ReturnStatement {
        token: Token,
        return_value: ExpressionNode,
    },
    ExpressionStatement {
        token: Token,
        expression: ExpressionNode,
    },
    BlockStatement {
        token: Token,
        statements: Vec<StatementNode>,
    },
}

/// expression node
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum ExpressionNode {
    Identifier {
        token: Token,
    },
    IntegerLiteral {
        token: Token,
    },
    StringLiteral {
        token: Token,
    },
    FunctionLiteral {
        token: Token,
        parameters: Vec<ExpressionNode>,
        body: Option<Box<StatementNode>>,
    },
    PrefixExpression {
        token: Token,
        right: Box<ExpressionNode>,
    },
    InfixExpression {
        token: Token,
        left: Box<ExpressionNode>,
        right: Box<ExpressionNode>,
    },
    Boolean {
        token: Token,
        value: bool,
    },
    IfExpression {
        token: Token,
        condition: Box<ExpressionNode>,
        consequence: Option<Box<StatementNode>>,
        alternative: Option<Box<StatementNode>>,
    },
    CallExpression {
        token: Token,
        function: Box<ExpressionNode>,
        arguments: Vec<ExpressionNode>,
    },
    ArrayLiteral {
        token: Token,
        elements: Vec<ExpressionNode>,
    },
    HashLiteral {
        token: Token,
        pairs: Vec<(ExpressionNode, ExpressionNode)>,
    },
    IndexExpression {
        token: Token,
        left: Box<ExpressionNode>,
        index: Box<ExpressionNode>,
    },
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

impl std::fmt::Display for StatementNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            StatementNode::ExpressionStatement {
                token,
                expression: _,
            } => token.fmt(f),
            StatementNode::BlockStatement {
                token: _,
                statements,
            } => {
                write!(
                    f,
                    "{}",
                    statements
                        .iter()
                        .map(|x| format!("{}", x))
                        .collect::<Vec<String>>()
                        .join(";")
                )
            }
            StatementNode::ReturnStatement {
                token: _,
                return_value,
            } => write!(f, "return {};", return_value),
            StatementNode::LetStatement {
                token: _,
                name,
                value,
            } => write!(f, "let {} = {};", name, value),
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
            StatementNode::LetStatement { token, name, value } => {
                format!("{} {} = {};", token, name, String::from(value),)
            }
            StatementNode::ReturnStatement {
                token: _,
                return_value,
            } => {
                format!("return {};", String::from(return_value))
            }
            StatementNode::ExpressionStatement {
                token: _,
                expression,
            } => String::from(expression),
            StatementNode::BlockStatement {
                token: _,
                statements,
            } => statements.iter().map(String::from).collect(),
        }
    }
}

impl std::fmt::Display for ExpressionNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ExpressionNode::Identifier { token } => token.fmt(f),
            ExpressionNode::IntegerLiteral { token } => token.fmt(f),
            ExpressionNode::FunctionLiteral {
                token,
                parameters: _,
                body: _,
            } => token.fmt(f),
            ExpressionNode::PrefixExpression { token, right: _ } => token.fmt(f),
            ExpressionNode::InfixExpression { token, left, right } => {
                write!(f, "({} {} {})", left, token, right)
            }
            ExpressionNode::Boolean { token, value: _ } => token.fmt(f),
            ExpressionNode::CallExpression {
                token,
                function: _,
                arguments: _,
            } => token.fmt(f),
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
            ExpressionNode::Identifier { token } => token.into(),
            ExpressionNode::IntegerLiteral { token } => token.into(),
            ExpressionNode::FunctionLiteral {
                token,
                parameters,
                body,
            } => format!(
                "{}({}){}",
                token,
                &((parameters)
                    .iter()
                    .map(String::from)
                    .collect::<Vec<String>>()
                    .join(", ")),
                body.as_ref().unwrap()
            ),
            ExpressionNode::PrefixExpression { token, right } => {
                format!("({}{})", token, String::from(right.as_ref()))
            }
            ExpressionNode::InfixExpression { token, left, right } => {
                format!(
                    "({} {} {})",
                    String::from(left.as_ref()),
                    token,
                    String::from(right.as_ref()),
                )
            }
            ExpressionNode::Boolean { token, value: _ } => token.into(),
            ExpressionNode::IfExpression {
                token: _,
                condition,
                consequence,
                alternative,
            } => format!(
                "if {} {}{}",
                condition,
                if let Some(c) = consequence {
                    String::from(c.as_ref())
                } else {
                    "".into()
                },
                if let Some(a) = alternative {
                    format!(" else {}", a)
                } else {
                    "".into()
                }
            ),
            ExpressionNode::CallExpression {
                token: _,
                function,
                arguments,
            } => format!(
                "{}({})",
                String::from(function.as_ref()),
                &(arguments
                    .iter()
                    .map(String::from)
                    .collect::<Vec<String>>()
                    .join(", "))
            ),
            ExpressionNode::ArrayLiteral { token: _, elements } => format!(
                "[{}]",
                &(elements
                    .iter()
                    .map(String::from)
                    .collect::<Vec<String>>()
                    .join(", "))
            ),
            ExpressionNode::IndexExpression {
                token: _,
                left,
                index,
            } => {
                format!(
                    "({}[{}])",
                    String::from(left.as_ref()),
                    String::from(index.as_ref())
                )
            }
            ExpressionNode::HashLiteral { token: _, pairs } => {
                format!(
                    "{{{}}}",
                    &(pairs
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
