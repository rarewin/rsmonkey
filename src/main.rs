extern crate rsmonkey;

use std::env;
use std::io;
use std::io::{BufRead, BufReader, Write};
use std::rc::Rc;

use rsmonkey::lexer::Lexer;
use rsmonkey::parser::Parser;

use rsmonkey::ast::StatementNode;
use rsmonkey::evaluator::eval;
use rsmonkey::evaluator::EvalNode;
use rsmonkey::object::Environment;

const PROMPT: &str = ">> ";
const MONKEY_FACE: &str = r##"            __,__
   .--.  .-"     "-.  .--.
  / .. \/  .-. .-.  \/ .. \
 | |  '|  /   Y   \  |'  | |
 | \   \  \ 0 | 0 /  /   / |
  \ '- ,\.-"""""""-./, -' /
   ''-' /_   ^ ^   _\ '-''
       |  \._   _./  |
       \   \ '~' /   /
        '._ '-=-' _.'
           '-----'
"##;

fn read_input() {
    let username = env::vars().find(|x| x.0 == "USER").unwrap_or_default().1;
    let env = Rc::new(Environment::new());

    println!(
        "Hello {}! This is the `RS'Monkey programming language!",
        username
    );
    println!("Feel free to type in commands");

    let stdin = io::stdin();
    let stdin = stdin.lock();
    let stdin = BufReader::new(stdin);
    let mut lines = stdin.lines();

    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    loop {
        stdout.write_all(PROMPT.as_bytes()).unwrap();
        stdout.flush().unwrap();

        if let Some(Ok(input)) = lines.next() {
            let l = Lexer::new(input.to_string());
            let mut p = Parser::new(l);

            let program = p.parse_program().unwrap(); // @todo

            //            if !p.errors().is_empty() {
            //                for e in p.errors() {
            //                    println!(
            //                        r##"{}
            //Woops! We ran into some monkey business here!
            //	parser error: {}"##,
            //                        MONKEY_FACE, e
            //                    );
            //                    continue;
            //                }
            //            }

            let evaluated = eval(
                &EvalNode::EvalStatementNode(Box::new(StatementNode::ProgramStatementNode(
                    Box::new(program),
                ))),
                env.clone(),
            );

            println!("{}", evaluated.inspect());
        } else {
            break;
        }
    }
}

fn main() {
    read_input();
}
