use crate::ast::{
    BlockStatement, ExpressionNode, LetStatement, Program, ReturnStatement, StatementNode,
};
use crate::object::{extend_environment, Array, Environment, Integer, Object};
use std::rc::Rc;

#[derive(Debug)]
pub enum EvalNode {
    EvalStatementNode(Box<StatementNode>),
    EvalExpressionNode(Box<ExpressionNode>),
}

/// evaluator function
pub fn eval(node: &EvalNode, env: Rc<Environment>) -> Object {
    match node {
        EvalNode::EvalStatementNode(sn) => eval_statement_node(sn, env),
        EvalNode::EvalExpressionNode(en) => eval_expression_node(en, env),
    }
}

/// evaluator function for statement node
fn eval_statement_node(node: &StatementNode, env: Rc<Environment>) -> Object {
    match node {
        StatementNode::ProgramStatementNode(ps) => eval_program(&ps, env),
        StatementNode::BlockStatementNode(bs) => eval_block_statement(&bs, env),
        StatementNode::ExpressionStatementNode(es) => eval_expression_node(&es.expression, env),
        StatementNode::ReturnStatementNode(rs) => eval_return_statement(&rs, env),
        StatementNode::LetStatementNode(ls) => eval_let_statement(&ls, env),
    }
}

/// evaluator function for program
fn eval_program(prog: &Program, env: Rc<Environment>) -> Object {
    let mut result = Object::Null;
    for stmt in &prog.statements {
        result = eval_statement_node(stmt, env.clone());
        if let Object::ReturnValueObject(rv) = result {
            return rv.value;
        } else if let Object::ErrorObject(_) = result {
            return result;
        }
    }
    result
}

/// evaluator function for block statement
fn eval_block_statement(bl: &BlockStatement, env: Rc<Environment>) -> Object {
    let mut result = Object::Null;
    for stmt in &bl.statements {
        result = eval_statement_node(stmt, env.clone());
        if let Object::ReturnValueObject(rv) = result {
            return Object::ReturnValueObject(rv);
        } else if let Object::ErrorObject(_) = result {
            return result;
        }
    }
    result
}

/// evaluator function for return statement
fn eval_return_statement(rs: &ReturnStatement, env: Rc<Environment>) -> Object {
    Object::new_return_value(eval_expression_node(&rs.return_value, env))
}

/// evaluator function for let statement
fn eval_let_statement(ls: &LetStatement, env: Rc<Environment>) -> Object {
    let val = eval_expression_node(&ls.value, env.clone());
    if is_error(&val) {
        return val;
    }
    env.set(&ls.name.value, &val);

    val
}

/// evaluator function for expression node
fn eval_expression_node(node: &ExpressionNode, env: Rc<Environment>) -> Object {
    match node {
        ExpressionNode::IntegerLiteralNode(il) => Object::new_integer(il.value),
        ExpressionNode::StringLiteralNode(sl) => Object::new_string(&sl.value),
        ExpressionNode::BooleanExpressionNode(be) => Object::new_boolean(be.value),
        ExpressionNode::PrefixExpressionNode(pe) => {
            let right = eval_expression_node(&pe.right, env);
            if is_error(&right) {
                return right;
            }
            eval_prefix_expression_node(&pe.operator, &right)
        }
        ExpressionNode::InfixExpressionNode(ie) => {
            let left = eval_expression_node(&ie.left, env.clone());
            if is_error(&left) {
                return left;
            }
            let right = eval_expression_node(&ie.right, env);
            if is_error(&right) {
                return right;
            }
            eval_infix_expression_node(&ie.operator, &left, &right)
        }
        ExpressionNode::IfExpressionNode(ie) => {
            let condition = eval_expression_node(&ie.condition, env.clone());
            match condition {
                Object::ErrorObject(_) => condition,
                Object::BooleanObject(b) => {
                    if b.value {
                        if let Some(consequence) = &ie.consequence {
                            eval_statement_node(consequence, env)
                        } else {
                            Object::Null
                        }
                    } else if let Some(alternative) = &ie.alternative {
                        eval_statement_node(alternative, env)
                    } else {
                        Object::Null
                    }
                }
                Object::Null => {
                    if let Some(alternative) = &ie.alternative {
                        eval_statement_node(alternative, env)
                    } else {
                        Object::Null
                    }
                }
                _ => {
                    if let Some(consequence) = &ie.consequence {
                        eval_statement_node(consequence, env)
                    } else {
                        Object::Null
                    }
                }
            }
        }
        ExpressionNode::IdentifierNode(id) => env.get(&id.value),
        ExpressionNode::FunctionLiteralNode(fl) => {
            if let Some(body) = &fl.body {
                Object::new_function(&fl.parameters, body, env)
            } else {
                Object::Null
            }
        }
        ExpressionNode::CallExpressionNode(ce) => {
            let function = eval_expression_node(&ce.function, env.clone());
            if is_error(&function) {
                return function;
            }
            let args = eval_expressions(&ce.arguments, env);
            if args.len() == 1 && is_error(&args[0]) {
                return args[0].clone();
            }
            apply_function(&function, &args)
        }
        ExpressionNode::IndexExpressionNode(ie) => {
            let left = eval_expression_node(&ie.left, env.clone());
            if is_error(&left) {
                return left;
            }

            let index = eval_expression_node(&ie.index, env);
            if is_error(&index) {
                return index;
            }

            eval_index_expression(&left, &index)
        }
        ExpressionNode::ArrayLiteralNode(al) => {
            let elements = eval_expressions(&al.elements, env);
            if elements.len() == 1 && is_error(&elements[0]) {
                elements[0].clone()
            } else {
                Object::new_array(&elements)
            }
        }
        ExpressionNode::HashLiteralNode(hl) => unimplemented!("{:?}", hl),
    }
}

/// evaluator function for prefix expression node
fn eval_prefix_expression_node(operator: &str, right: &Object) -> Object {
    match operator {
        "!" => eval_bang_operation_expression_node(right),
        "-" => eval_minus_operation_expression_node(right),
        _ => Object::new_error(format!(
            "unknown operator: {}{}",
            operator,
            right.object_type()
        )),
    }
}

/// evaluator function for bang operation expression node
fn eval_bang_operation_expression_node(right: &Object) -> Object {
    match right {
        Object::BooleanObject(bl) => Object::new_boolean(!(*bl).value),
        Object::Null => Object::new_boolean(true),
        _ => Object::new_boolean(false),
    }
}

/// evaluator function for minus operation expression node
fn eval_minus_operation_expression_node(right: &Object) -> Object {
    if let Object::IntegerObject(integer) = right {
        Object::new_integer(-integer.value)
    } else {
        Object::new_error(format!("unknown operator: -{}", right.object_type()))
    }
}

/// evaluator function for infix expression node
fn eval_infix_expression_node(operator: &str, left: &Object, right: &Object) -> Object {
    if let Object::IntegerObject(left_integer) = left {
        if let Object::IntegerObject(right_integer) = right {
            return eval_integer_infix_expression(operator, &left_integer, &right_integer);
        }
    }

    if let Object::StringObject(left_str) = left {
        if let Object::StringObject(right_str) = right {
            if operator == "+" {
                let mut s = String::new();
                s.push_str(&(*left_str).value);
                s.push_str(&(*right_str).value);
                return Object::new_string(&s);
            }
        }
    }

    match operator {
        "==" => Object::new_boolean(left == right),
        "!=" => Object::new_boolean(left != right),
        _ => {
            if left.object_type() != right.object_type() {
                Object::new_error(format!(
                    "type mismatch: {} {} {}",
                    left.object_type(),
                    operator,
                    right.object_type(),
                ))
            } else {
                Object::new_error(format!(
                    "unkown operator: {} {} {}",
                    left.object_type(),
                    operator,
                    right.object_type(),
                ))
            }
        }
    }
}

/// evaluator function for integer
fn eval_integer_infix_expression(operator: &str, left: &Integer, right: &Integer) -> Object {
    match operator {
        "+" => Object::new_integer(left.value + right.value),
        "-" => Object::new_integer(left.value - right.value),
        "*" => Object::new_integer(left.value * right.value),
        "/" => Object::new_integer(left.value / right.value),
        "<" => Object::new_boolean(left.value < right.value),
        ">" => Object::new_boolean(left.value > right.value),
        "==" => Object::new_boolean(left.value == right.value),
        "!=" => Object::new_boolean(left.value != right.value),
        _ => Object::Null,
    }
}

/// evaluator function for expressions
fn eval_expressions(exps: &[ExpressionNode], env: Rc<Environment>) -> Vec<Object> {
    let mut result = Vec::<Object>::new();

    for e in exps {
        let evaluated = eval_expression_node(e, env.clone());
        if is_error(&evaluated) {
            return vec![evaluated];
        }
        result.push(evaluated);
    }

    result
}

/// evaluator function for index expression
fn eval_index_expression(left: &Object, index: &Object) -> Object {
    let l = match left {
        Object::ArrayObject(ao) => ao,
        _ => {
            return Object::new_error(format!(
                "index operator not supported: {}",
                left.object_type()
            ))
        }
    };

    let i = match index {
        Object::IntegerObject(io) => io,
        _ => {
            return Object::new_error(format!(
                "index operator not supported: {}",
                left.object_type()
            ))
        }
    };
    eval_array_index_expression(&l, &i)
}

/// evaluator function for array index expression
fn eval_array_index_expression(array: &Array, index: &Integer) -> Object {
    let idx = index.value;
    let max = array.elements.len();

    if idx < 0 || idx >= (max as i64) {
        return Object::Null;
    }

    array.elements[idx as usize].clone()
}

/// check if error or not
fn is_error(obj: &Object) -> bool {
    if let Object::ErrorObject(_) = obj {
        true
    } else {
        false
    }
}

/// apply function
fn apply_function(function: &Object, args: &[Object]) -> Object {
    match function {
        Object::FunctionObject(fnc) => {
            if fnc.parameters.len() != args.len() {
                return Object::new_error(format!(
                    "the # of arguments is wrong, expected {}, got {}",
                    fnc.parameters.len(),
                    args.len(),
                ));
            }
            let extended_env = extend_environment(fnc.env.clone());
            for (idx, p) in fnc.parameters.iter().enumerate() {
                extended_env.set(&p.string(), &args[idx]);
            }
            let evaluated = eval_statement_node(&fnc.body, extended_env);
            if let Object::ReturnValueObject(ro) = evaluated {
                ro.value
            } else {
                evaluated
            }
        }
        Object::BuiltinObject(bio) => (bio.builtin)(args.to_vec()),
        _ => Object::new_error(format!("not a function: {}", function.object_type())),
    }
}
