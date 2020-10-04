use std::rc::Rc;

use rsmonkey::evaluator::eval;
use rsmonkey::lexer::Lexer;
use rsmonkey::object::{Environment, Object};
use rsmonkey::parser::Parser;

/// eval function
fn test_eval(input: &'static str) -> Object {
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let env = Rc::new(Environment::new());
    eval(&mut p, env).unwrap()
}

#[test]
fn test_display_strings() {
    let test_data = vec![
        (Object::new_integer(1), "1"),
        (Object::new_integer(-100), "-100"),
        (Object::new_boolean(true), "true"),
        (Object::new_boolean(false), "false"),
        (Object::new_string("hoge"), r##""hoge""##),
        (Object::new_string("SHIROBAKO"), r##""SHIROBAKO""##),
        (
            Object::new_string("The Legend of Heroes"),
            r##""The Legend of Heroes""##,
        ),
        (
            Object::new_return_value(Object::new_integer(-1234)),
            "-1234",
        ),
        (
            Object::new_return_value(Object::new_boolean(false)),
            "false",
        ),
        (
            test_eval("fn(){}"),
            r##"fn() {
}"##,
        ),
        (
            test_eval("fn(){1}"),
            r##"fn() {
1
}"##,
        ),
        (
            test_eval(
                r##"fn(x) {
  if (x == 0) {
    return 1;
  } else {
    return x * 2;
  }
}"##,
            ),
            r##"fn(x) {
if (x == 0) return 1; else return (x * 2);
}"##,
        ),
        (test_eval("[1,   2,   3,     4  ]"), "[1, 2, 3, 4]"),
        (
            test_eval(r##"[1,   "2",   3,     4  , true  , false]"##),
            r##"[1, "2", 3, 4, true, false]"##,
        ),
        (test_eval(r##"{1:1, 2:  2}"##), r##"{1: 1, 2: 2}"##),
        (test_eval(r##"{"1":1, 2:  "2"}"##), r##"{"1": 1, 2: "2"}"##),
        (test_eval("len"), "builtin function"),
        (test_eval("rest"), "builtin function"),
        (test_eval("first"), "builtin function"),
        (test_eval("puts"), "builtin function"),
        (Object::Null, ""),
    ];

    for t in test_data {
        assert_eq!(format!("{}", t.0), t.1);
    }
}
