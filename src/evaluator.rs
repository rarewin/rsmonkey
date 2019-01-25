use crate::ast::{ExpressionNode, StatementNode};
use crate::object::{Boolean, Integer};
use crate::object::{Object, ObjectType, FALSE, TRUE};

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
        ExpressionNode::BooleanExpressionNode(be) => {
            Object::BooleanObject(if be.value { TRUE } else { FALSE })
        }
        ExpressionNode::PrefixExpressionNode(pe) => {
            let right = eval_expression_node(&pe.right);
            eval_prefix_expression_node(&pe.operator, &right)
        }
        _ => panic!("not implemented yet"),
    }
}

/// evaluator function for prefix expression node
fn eval_prefix_expression_node(operator: &str, right: &Object) -> Object {
    match operator {
        "!" => eval_bang_operation_expression_node(right),
        "-" => eval_minus_operation_expression_node(right),
        _ => Object::Null,
    }
}

/// evaluator function for bang operation expression node
fn eval_bang_operation_expression_node(right: &Object) -> Object {
    match right {
        Object::BooleanObject(TRUE) => Object::BooleanObject(FALSE),
        Object::BooleanObject(FALSE) => Object::BooleanObject(TRUE),
        Object::Null => Object::BooleanObject(TRUE),
        _ => Object::BooleanObject(FALSE),
    }
}

/// evaluator function for minus operation expression node
fn eval_minus_operation_expression_node(right: &Object) -> Object {
    if let Object::IntegerObject(integer) = right {
        Object::IntegerObject(Integer {
            value: -integer.value,
        })
    } else {
        Object::Null
    }
}
