extern crate rsmonkey;

use std::env;
use std::io;
use std::io::Write;

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

fn read_input() -> io::Result<()> {
    let mut input = String::new();
    let username = env::vars().find(|x| x.0 == "USER").unwrap_or_default().1;
    let mut env = Environment::new();

    println!(
        "Hello {}! This is the `RS'Monkey programming language!",
        username
    );
    println!("Feel free to type in commands");

    while {
        input.clear();
        print!("{}", PROMPT);
        io::stdout().flush()?;
        io::stdin().read_line(&mut input)?;

        let l = Lexer::new(input.to_string());
        let mut p = Parser::new(l);

        let program = p.parse_program();

        if p.errors().len() > 0 {
            for e in p.errors() {
                println!(
                    r##"{}
Woops! We ran into some monkey business here!
	parser error: {}"##,
                    MONKEY_FACE, e
                );
                continue;
            }
        }

        let evaluated = eval(
            &EvalNode::EvalStatementNode(Box::new(StatementNode::ProgramStatementNode(Box::new(
                program,
            )))),
            &mut env,
        );

        println!("{}", evaluated.inspect());

        !input.is_empty()
    } {}

    Ok(())
}

fn main() {
    read_input().unwrap();
}
