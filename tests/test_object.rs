use std::cell::RefCell;
use std::rc::Rc;

use rsmonkey::evaluator::eval;
use rsmonkey::lexer::Lexer;
use rsmonkey::object::{Environment, Object};
use rsmonkey::parser::Parser;

/// eval function
fn test_eval(input: &'static str) -> Object {
    let l = Lexer::new(input);
    let p = Parser::new(l);

    let env = Rc::new(RefCell::new(Environment::new()));
    eval(p, env).unwrap()
}

#[test]
fn test_display_strings() {
    let test_data = vec![
        (Object::from(1), "1"),
        (Object::from(-100), "-100"),
        (Object::from(true), "true"),
        (Object::from(false), "false"),
        (Object::from("hoge"), r##""hoge""##),
        (Object::from("SHIROBAKO"), r##""SHIROBAKO""##),
        (
            Object::from("The Legend of Heroes"),
            r##""The Legend of Heroes""##,
        ),
        (Object::new_return_value(Object::from(-1234)), "-1234"),
        (Object::new_return_value(Object::from(false)), "false"),
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
