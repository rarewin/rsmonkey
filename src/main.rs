extern crate rsmonkey;

use std::env;
use std::io;
use std::io::Write;

use rsmonkey::lexer::Lexer;

#[cfg(test)]
mod test;

fn read_input() -> io::Result<()> {
    let mut input = String::new();
    let username = env::vars().find(|x| x.0 == "USER").unwrap_or_default().1;

    println!(
        "Hello {}! This is the `RS'Monkey programming language!",
        username
    );
    println!("Feel free to type in commands");

    while {
        input.clear();
        print!(">> ");
        io::stdout().flush()?;
        io::stdin().read_line(&mut input)?;

        let mut l = Lexer::new(input.to_string());

        while {
            let t = l.next_token();
            println!("{:?}", t);
            !t.is_eof()
        } {}

        !input.is_empty()
    } {}

    Ok(())
}

fn main() {
    read_input().unwrap();
}
