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
        StatementNode::ProgramStatementNode(ps) => eval_statements(&ps.statements),
        StatementNode::BlockStatementNode(bs) => eval_statements(&bs.statements),
        StatementNode::ExpressionStatementNode(es) => eval_expression_node(&es.expression),
        StatementNode::Null => Object::Null,
        _ => panic!("not implemented yet"),
    }
}

/// evaluator function for statements (vector)
fn eval_statements(list: &Vec<StatementNode>) -> Object {
    let mut result = Object::Null;
    for stmt in list {
        result = eval_statement_node(stmt)
    }
    result
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
        ExpressionNode::InfixExpressionNode(ie) => {
            let left = eval_expression_node(&ie.left);
            let right = eval_expression_node(&ie.right);
            eval_infix_expression_node(&ie.operator, &left, &right)
        }
        ExpressionNode::IfExpressionNode(ie) => {
            let condition = eval_expression_node(&ie.condition);
            match condition {
                Object::BooleanObject(b) => {
                    if b.value == true {
                        eval_statement_node(&ie.consequence)
                    } else {
                        eval_statement_node(&ie.alternative)
                    }
                }
                Object::Null => eval_statement_node(&ie.alternative),
                _ => eval_statement_node(&ie.consequence),
            }
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

/// evaluator function for infix expression node
fn eval_infix_expression_node(operator: &str, left: &Object, right: &Object) -> Object {
    if let Object::IntegerObject(left_integer) = left {
        if let Object::IntegerObject(right_integer) = right {
            return eval_integer_infix_expression(operator, &left_integer, &right_integer);
        }
    }
    match operator {
        "==" => Object::BooleanObject(Boolean {
            value: left == right,
        }),
        "!=" => Object::BooleanObject(Boolean {
            value: left != right,
        }),
        _ => Object::Null,
    }
}

/// evaluator function for integer
fn eval_integer_infix_expression(operator: &str, left: &Integer, right: &Integer) -> Object {
    match operator {
        "+" => Object::IntegerObject(Integer {
            value: left.value + right.value,
        }),
        "-" => Object::IntegerObject(Integer {
            value: left.value - right.value,
        }),
        "*" => Object::IntegerObject(Integer {
            value: left.value * right.value,
        }),
        "/" => Object::IntegerObject(Integer {
            value: left.value / right.value,
        }),
        "<" => Object::BooleanObject(Boolean {
            value: left.value < right.value,
        }),
        ">" => Object::BooleanObject(Boolean {
            value: left.value > right.value,
        }),
        "==" => Object::BooleanObject(Boolean {
            value: left.value == right.value,
        }),
        "!=" => Object::BooleanObject(Boolean {
            value: left.value != right.value,
        }),
        _ => Object::Null,
    }
}
