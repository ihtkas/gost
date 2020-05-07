use std::env;
use std::fs;
mod parser;
use parser::lexer;

fn main() {
    let args: Vec<String> = env::args().collect();
    let content = fs::read_to_string(&args[1]).expect("Couldn't read content from file");

    // let mut tokenizer = lexer::Tokenizer::new(content);
    // let mut flag = true;
    // while flag {
    //     match tokenizer.next_token() {
    //         Ok(Some(t)) => {
    //             println!("----{}--- {}", t, t.kind());
    //             if let lexer::TokenKind::EOF = t.kind() {
    //                 flag = false;
    //             }
    //         }
    //         _ => (),
    //     }
    // }
    let res = parser::Parser::parse(content);

    println!("ast: {:?} ###",res);
}


// fn main(){
//     print("~~~~~~~~~", 123+25, 4534)
// }