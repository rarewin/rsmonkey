use crate::ast::{ExpressionNode, StatementNode};
use crate::object::Integer;
use crate::object::{Object, ObjectType};

#[derive(Debug)]
pub enum EvalNode {
    EvalStatementNode(Box<StatementNode>),
    EvalExpressionNode(Box<ExpressionNode>),
}

/// evaluator function
pub fn eval(node: &EvalNode) -> Object {
    match node {
        EvalNode::EvalStatementNode(sn) => eval_statement_node(sn),
        _ => panic!("not implemented yet"),
    }
}

/// evaluator function for statement node
fn eval_statement_node(node: &StatementNode) -> Object {
    match node {
        StatementNode::ProgramStatementNode(ps) => {
            let mut result = Object::Null;
            for stmt in &ps.statements {
                result = eval_statement_node(stmt)
            }
            result
        }
        StatementNode::ExpressionStatementNode(es) => eval_expression_node(&es.expression),
        _ => panic!("not implemented yet"),
    }
}

/// evaluator function for expression node
fn eval_expression_node(node: &ExpressionNode) -> Object {
    match node {
        ExpressionNode::IntegerLiteralNode(il) => {
            Object::IntegerObject(Integer { value: il.value })
        }
        _ => panic!("not implemented yet"),
    }
}
