use crate::parser::lexer;
use backtrace::Backtrace;
#[derive(Clone, Debug)]
pub struct Error{
    pub pos: lexer::Pos,
    pub msg: String,
}

impl Error{
    pub fn new(pos: lexer::Pos,
        msg: String)-> Error{
            let bt = Backtrace::new();
        println!("{:?}", bt);
            Error{pos, msg}
        }
}