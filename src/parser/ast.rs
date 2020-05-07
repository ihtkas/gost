use crate::parser::lexer;

#[derive(Debug)]
pub struct File {
    defs: Vec<Definition>,
}

impl File {
    pub fn new(defs: Vec<Definition>) -> File {
        File { defs }
    }
    pub fn defs(&self) -> &Vec<Definition> {
        &self.defs
    }
}

#[derive(Debug)]
pub enum Definition {
    StructDef(Struct),
    FuncDef(Func),
}

pub trait Node {
    fn pos(&self) -> lexer::Pos;
    fn end(&self) -> lexer::Pos;
}

#[derive(Debug)]
pub struct Struct {
    pos: lexer::Pos,
    end: lexer::Pos,
    name: lexer::Token,
    prop_defs: Vec<PropDef>,
}

impl Struct {
    pub fn new(
        pos: lexer::Pos,
        end: lexer::Pos,
        name: lexer::Token,
        prop_defs: Vec<PropDef>,
    ) -> Struct {
        Struct {
            pos,
            end,
            name,
            prop_defs,
        }
    }
    pub fn pos(&self) -> &lexer::Pos {
        &self.pos
    }
    pub fn end(&self) -> &lexer::Pos {
        &self.end
    }
    pub fn name(&self) -> &lexer::Token {
        &self.name
    }
    pub fn prop_defs(&self) -> &Vec<PropDef> {
        &self.prop_defs
    }
}
// ex: name, address string
#[derive(Debug)]
pub struct PropDef {
    pos: lexer::Pos,
    end: lexer::Pos,
    names: Vec<lexer::Token>,
    datatype: lexer::Token,
}


impl PropDef {
    pub fn new(
        pos: lexer::Pos,
        end: lexer::Pos,
        names: Vec<lexer::Token>,
        datatype: lexer::Token,
    ) -> PropDef {
        PropDef {
            pos,
            end,
            names,
            datatype,
        }
    }

    pub fn pos(&self) -> &lexer::Pos {
        &self.pos
    }
    pub fn end(&self) -> &lexer::Pos {
        &self.end
    }
    pub fn names(&self) -> &Vec<lexer::Token> {
        &self.names
    }
    pub fn datatype(&self) -> &lexer::Token {
        &self.datatype
    }
}

#[derive(Debug)]
pub struct Func {
    pos: lexer::Pos,
    end: lexer::Pos,
    name: lexer::Token,
    // prop_defs: Vec<PropDef>,
}
