extern crate backtrace;
use crate::error::Error;
use std::collections::HashMap;
// use backtrace::Backtrace;

pub struct Tokenizer {
    content: Vec<char>,
    ind: usize,
    cur_pos: usize,
    cur_line_no: usize,
    next_tokens: Result<Vec<Token>, Error>,
    keywords: HashMap<String, TokenKind>,
    delimiters: HashMap<char, TokenKind>,
    symbols: HashMap<String, TokenKind>,
}

enum Char {
    ValidChar(char),
    EOF,
}

impl Char {
    fn unwrap(self) -> char {
        match self {
            Char::ValidChar(ch) => ch,
            Char::EOF => panic!("Unwrap on EOF character!!!"),
        }
    }
}

impl<'a> Tokenizer {
    pub fn new(content: String) -> Tokenizer {
        return {
            let mut t = Tokenizer {
                next_tokens: Ok(Vec::new()),
                content: content.chars().collect::<Vec<char>>(),
                ind: 0,
                cur_pos: 0,
                cur_line_no: 1,
                keywords: HashMap::new(),
                delimiters: HashMap::new(),
                symbols: HashMap::new(),
            };
            t.keywords
                .insert(String::from("struct"), TokenKind::StructKeyword);
            t.keywords
                .insert(String::from("fn"), TokenKind::FuncKeyword);
            t.keywords
                .insert(String::from("string"), TokenKind::StringDTKeyword);
            t.keywords
                .insert(String::from("uint"), TokenKind::UIntDTKeyword);
            t.keywords.insert(String::from("if"), TokenKind::IfKeyword);

            t.keywords
                .insert(String::from("else"), TokenKind::Elsekeyword);

            t.keywords
                .insert(String::from("for"), TokenKind::ForKeyword);
            t.delimiters.insert('\n', TokenKind::NewLine);
            t.delimiters.insert('{', TokenKind::OpenCurly);
            t.delimiters.insert('}', TokenKind::CloseCurly);
            t.delimiters.insert('(', TokenKind::OpenBrace);
            t.delimiters.insert(')', TokenKind::CloseBrace);
            t.delimiters.insert(',', TokenKind::Comma);
            t.symbols.insert(String::from("+"), TokenKind::Plus);
            t.symbols.insert(String::from("-"), TokenKind::Minus);
            t.symbols.insert(String::from("*"), TokenKind::Star);
            t.symbols.insert(String::from("/"), TokenKind::Slash);
            t.symbols.insert(String::from(">"), TokenKind::Greater);
            t.symbols
                .insert(String::from(">="), TokenKind::GreaterEqual);
            t.symbols.insert(String::from("<"), TokenKind::Lesser);
            t.symbols.insert(String::from("<="), TokenKind::LesserEqual);
            t.symbols.insert(String::from("="), TokenKind::AssignEqual);
            t.symbols.insert(String::from("=="), TokenKind::DoubleEqual);
            t.symbols.insert(String::from("!="), TokenKind::NotEqual);
            t.symbols.insert(String::from("!"), TokenKind::Not);
            t.symbols.insert(String::from("&"), TokenKind::And);
            t.symbols.insert(String::from("|"), TokenKind::Or);
            t.delimiters.insert('.', TokenKind::Dot);
            t
        };
    }

    fn pop_cur_char(&mut self) -> Char {
        if self.ind < self.content.len() {
            let ch = self.content[self.ind];
            if ch == '\n' {
                self.cur_line_no += 1;
                self.cur_pos = 0;
            } else {
                self.cur_pos += 1;
            }
            self.ind += 1;
            Char::ValidChar(ch)
        } else {
            Char::EOF
        }
    }

    fn peek_cur_char(&self) -> Char {
        if self.ind < self.content.len() {
            Char::ValidChar(self.content[self.ind])
        } else {
            Char::EOF
        }
    }

    pub fn next_token(&mut self, skip_space: bool) -> Result<Token, Error> {
        // let bt = Backtrace::new();

        // do_some_work();

        // println!("{:?}", bt);

        if let Ok(list) = &mut self.next_tokens {
            let tk = list.pop();
            if let Some(tk) = tk {
                return Ok(tk);
            }
        }
        let space = self.handle_spaces();
        if let Some(tk) = space {
            if !skip_space {
                return Ok(tk);
            }
        }

        let word = self.handle_word();
        if let Some(tk) = word {
            return Ok(tk);
        }

        let delimiter = self.handle_symbol();
        if let Some(tk) = delimiter {
            return Ok(tk);
        }

        let delimiter = self.handle_delimiter();
        if let Some(tk) = delimiter {
            return Ok(tk);
        }

        let str_lit = self.handle_string();
        if let Ok(Some(tk)) = str_lit {
            return Ok(tk);
        }

        let number = self.handle_number();
        if let Some(tk) = number {
            return Ok(tk);
        }
        return Ok(self.handle_unknown());
    }

    pub fn next_token_checked(
        &mut self,
        skip_space: bool,
        kind1: TokenKind,
        kind2: TokenKind,
    ) -> Result<Option<Token>, Error> {
        let res = self.next_token(skip_space);
        match res {
            Ok(tk) => {
                if tk.kind == kind1 || tk.kind == kind2 {
                    Ok(Some(tk))
                } else {
                    match &mut self.next_tokens {
                        Ok(list) => list.push(tk),
                        _ => {}
                    }
                    Ok(None)
                }
            }
            Err(err) => Err(err),
        }
    }

    pub fn push_token(&mut self, tk: Token) {
        println!("push token {} ", tk);
        match &mut self.next_tokens {
            Ok(list) => list.push(tk),
            _ => {}
        }
    }

    fn handle_unknown(&mut self) -> Token {
        let ch = self.peek_cur_char();
        if let Char::EOF = ch {
            Token::eof_token(Pos::new(self.cur_line_no, self.cur_pos))
        } else {
            let pos = self.cur_pos;
            let ch = self.pop_cur_char().unwrap();
            Token::new(
                TokenKind::Unknown,
                ch.to_string(),
                Pos::new(self.cur_line_no, pos),
                Pos::new(self.cur_line_no, pos),
                None,
            )
        }
    }

    fn handle_spaces(&mut self) -> Option<Token> {
        let ch = self.peek_cur_char();
        match ch {
            Char::ValidChar(ch) => {
                if ch == ' ' {
                    let pos = self.cur_pos;
                    self.pop_cur_char();
                    let mut str = String::from(" ");
                    let mut flag = match self.peek_cur_char() {
                        Char::ValidChar(' ') => true,
                        _ => false,
                    };

                    while flag {
                        str.push_str(" ");
                        self.pop_cur_char();
                        flag = match self.peek_cur_char() {
                            Char::ValidChar(' ') => true,
                            _ => break,
                        };
                    }
                    Some(Token::new(
                        TokenKind::Space,
                        str,
                        Pos::new(self.cur_line_no, pos),
                        Pos::new(self.cur_line_no, self.cur_pos - 1),
                        None,
                    ))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn handle_word(&mut self) -> Option<Token> {
        let ch = self.peek_cur_char();
        match ch {
            Char::ValidChar(ch) => match ch {
                'a'..='z' | 'A'..='Z' | '_' => {
                    let pos = self.cur_pos;
                    let mut word = self.peek_cur_char().unwrap().to_string();
                    self.pop_cur_char();
                    let word_matcher = |t: &mut Tokenizer| {
                        match t.peek_cur_char() {
                            Char::ValidChar(ch) => match ch {
                                'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => true, //number cannot occur as first char for now.
                                _ => false,
                            },
                            _ => false,
                        }
                    };
                    let mut flag = word_matcher(self);

                    while flag {
                        word.push_str(&self.pop_cur_char().unwrap().to_string());
                        flag = word_matcher(self);
                    }
                    let val = self.keywords.get(&word);
                    if let Some(&tk) = val {
                        return Some(Token::new(
                            tk,
                            word,
                            Pos::new(self.cur_line_no, pos),
                            Pos::new(self.cur_line_no, self.cur_pos - 1),
                            None,
                        ));
                    } else {
                        Some(Token::new(
                            TokenKind::Identifier,
                            word,
                            Pos::new(self.cur_line_no, pos),
                            Pos::new(self.cur_line_no, self.cur_pos - 1),
                            None,
                        ))
                    }
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn handle_number(&mut self) -> Option<Token> {
        let ch = self.peek_cur_char();
        match ch {
            Char::ValidChar(ch) => match ch {
                '0'..='9' => {
                    let pos = self.cur_pos;
                    let mut word = self.peek_cur_char().unwrap().to_string();
                    self.pop_cur_char();
                    let num_matcher = |t: &mut Tokenizer| match t.peek_cur_char() {
                        Char::ValidChar(ch) => match ch {
                            '0'..='9' => true,
                            _ => false,
                        },
                        _ => false,
                    };
                    let mut flag = num_matcher(self);

                    while flag {
                        word.push_str(&self.pop_cur_char().unwrap().to_string());
                        flag = num_matcher(self);
                    }

                    Some(Token::new(
                        TokenKind::Int,
                        word,
                        Pos::new(self.cur_line_no, pos),
                        Pos::new(self.cur_line_no, self.cur_pos - 1),
                        None,
                    ))
                }
                _ => None,
            },
            _ => None,
        }
    }

    fn handle_string(&mut self) -> Result<Option<Token>, Error> {
        // TODO: Escape sequence
        let ch = self.peek_cur_char();
        match ch {
            Char::ValidChar(ch) => match ch {
                '"' => {
                    let pos = self.cur_pos;
                    let mut str_lit = String::new();
                    self.pop_cur_char();
                    let mut flag = true;

                    while flag {
                        match self.peek_cur_char() {
                            Char::EOF => {
                                return Err(Error::new(
                                    Pos::new(self.cur_line_no, pos),
                                    String::from("Unterminated String Literal"),
                                ));
                            }
                            Char::ValidChar(ch) => match ch {
                                '"' => {
                                    self.pop_cur_char();
                                    flag = false;
                                }
                                '\n' => {
                                    return Err(Error::new(
                                        Pos::new(self.cur_line_no, pos),
                                        String::from("Unterminated String Literal"),
                                    ));
                                }
                                _ => {
                                    str_lit.push_str(&self.pop_cur_char().unwrap().to_string());
                                }
                            },
                        }
                    }

                    Ok(Some(Token::new(
                        TokenKind::StringLiteral,
                        str_lit,
                        Pos::new(self.cur_line_no, pos),
                        Pos::new(self.cur_line_no, self.cur_pos - 1),
                        None,
                    )))
                }
                _ => Ok(None),
            },
            _ => Ok(None),
        }
    }

    fn handle_delimiter(&mut self) -> Option<Token> {
        let ch = self.peek_cur_char();
        match ch {
            Char::ValidChar(ch) => {
                let val = self.delimiters.get(&ch);
                if let Some(&tk) = val {
                    let pos = self.cur_pos;
                    let ch = self.pop_cur_char().unwrap();
                    Some(Token::new(
                        tk,
                        ch.to_string(),
                        Pos::new(self.cur_line_no, pos),
                        Pos::new(self.cur_line_no, pos),
                        None,
                    ))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn handle_symbol(&mut self) -> Option<Token> {
        let ch = self.peek_cur_char();
        match ch {
            Char::ValidChar(ch) => {
                let mut str = ch.to_string();
                let val = self.symbols.get(&str);
                if let Some(tk) = val {
                    let tk_copy = *tk;
                    let pos = self.cur_pos;
                    self.pop_cur_char();
                    let ch = self.peek_cur_char();
                    match ch {
                        Char::ValidChar(ch) => {
                            str.push(ch);
                            let val = self.symbols.get(&str);
                            if let Some(&tk2) = val {
                                let pos = self.cur_pos;
                                self.pop_cur_char();
                                return Some(Token::new(
                                    tk2,
                                    str,
                                    Pos::new(self.cur_line_no, pos),
                                    Pos::new(self.cur_line_no, pos),
                                    None,
                                ));
                            }else{
                                str.pop();
                            }
                        }
                        _ => {}
                    }
                    Some(Token::new(
                        tk_copy,
                        str,
                        Pos::new(self.cur_line_no, pos),
                        Pos::new(self.cur_line_no, pos),
                        None,
                    ))
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    StructKeyword,
    FuncKeyword,
    StringDTKeyword,
    UIntDTKeyword,
    IfKeyword,
    Elsekeyword,
    ForKeyword,
    // StructName,
    // StructField,
    // DTKeyword,
    OpenCurly,
    CloseCurly,
    OpenBrace,
    CloseBrace,
    Comma,
    Plus,
    Minus,
    Star,
    Slash, //forward slash
    Greater,
    Lesser,
    GreaterEqual,
    LesserEqual,
    DoubleEqual,
    NotEqual,
    And,
    Or,
    AssignEqual,
    Dot,
    Not,
    Int,
    // Float,
    StringLiteral,
    // Comment,
    // MultiLineComment,
    Identifier,
    Space,
    NewLine,
    EOF,
    Start,
    Unknown,
}

impl TokenKind {
    pub fn is_binary_operator(self) -> bool {
        match self {
            TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Greater
            | TokenKind::Lesser
            | TokenKind::GreaterEqual
            | TokenKind::LesserEqual
            | TokenKind::DoubleEqual
            | TokenKind::NotEqual
            | TokenKind::And
            | TokenKind::Or => true,
            _ => false,
        }
    }
}

// impl std::cmp::PartialEq for TokenKind{

// fn eq(&self, tk: &TokenKind) -> bool {
//  }
// }

#[derive(Debug)]
pub struct Token {
    kind: TokenKind,
    value: String,
    start_pos: Pos,
    end_pos: Pos,
    discarded: Option<Vec<Token>>,
}

impl Token {
    fn new(
        kind: TokenKind,
        value: String,
        start_pos: Pos,
        end_pos: Pos,
        discarded: Option<Vec<Token>>,
    ) -> Token {
        println!("{} {:?} {}", kind, start_pos, value);
        // let bt = Backtrace::new();
        // println!("{:?}", bt);
        return Token {
            kind,
            value,
            start_pos,
            end_pos,
            discarded,
        };
    }
    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn is_datatype(&self) -> bool {
        match self.kind {
            TokenKind::StringDTKeyword => true,
            TokenKind::UIntDTKeyword => true,
            _ => false,
        }
    }
    pub fn value(&self) -> &String {
        &self.value
    }
    pub fn start_pos(&self) -> &Pos {
        &self.start_pos
    }
    pub fn end_pos(&self) -> &Pos {
        &self.end_pos
    }
    pub fn discarded(&self) -> &Option<Vec<Token>> {
        &self.discarded
    }

    pub fn add_discarded(&mut self, tk: Token) {
        match &mut self.discarded {
            None => {
                let mut v = Vec::new();
                v.push(tk);
                self.discarded = Some(v);
            }
            Some(v) => {
                v.push(tk);
            }
        }
    }

    fn eof_token(pos: Pos) -> Token {
        Token::new(TokenKind::EOF, String::from(""), pos, pos, None)
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::StructKeyword => write!(f, "{}", "StructKeyword"),
            TokenKind::IfKeyword => write!(f, "{}", "IfKeyword"),
            TokenKind::Elsekeyword => write!(f, "{}", "Elsekeyword"),
            TokenKind::ForKeyword => write!(f, "{}", "ForKeyword"),
            TokenKind::FuncKeyword => write!(f, "{}", "FuncKeyword"),
            TokenKind::StringDTKeyword => write!(f, "{}", "StringDTKeyword"),
            TokenKind::UIntDTKeyword => write!(f, "{}", "UIntDTKeyword"),
            //TokenKind:: StructName=> write!(f, "{}",     "StructName"),
            //TokenKind:: StructField=> write!(f, "{}",     "StructField"),
            //TokenKind:: DTKeyword=> write!(f, "{}",     "DTKeyword"),
            TokenKind::OpenCurly => write!(f, "{}", "OpenCurly"),
            TokenKind::CloseCurly => write!(f, "{}", "CloseCurly"),
            TokenKind::OpenBrace => write!(f, "{}", "OpenBrace"),
            TokenKind::CloseBrace => write!(f, "{}", "CloseBrace"),
            TokenKind::Comma => write!(f, "{}", "Comma"),
            TokenKind::Plus => write!(f, "{}", "Plus"),
            TokenKind::Minus => write!(f, "{}", "Minus"),
            TokenKind::Star => write!(f, "{}", "Star"),
            TokenKind::Slash => write!(f, "{}", "Slash"),
            TokenKind::Greater => write!(f, "{}", "Greater"),
            TokenKind::Lesser => write!(f, "{}", "Lesser"),
            TokenKind::GreaterEqual => write!(f, "{}", "GreaterEqual"),
            TokenKind::LesserEqual => write!(f, "{}", "LesserEqual"),
            TokenKind::AssignEqual => write!(f, "{}", "AssignEqual"),
            TokenKind::DoubleEqual => write!(f, "{}", "DoubleEqual"),
            TokenKind::NotEqual => write!(f, "{}", "NotEqual"),
            TokenKind::Not => write!(f, "{}", "Not"),
            TokenKind::And => write!(f, "{}", "And"),
            TokenKind::Or => write!(f, "{}", "Or"),
            TokenKind::Dot => write!(f, "{}", "Dot"),
            TokenKind::Int => write!(f, "{}", "Int"),
            //TokenKind:: Float=> write!(f, "{}",     "Float"),
            TokenKind::StringLiteral => write!(f, "{}", "StringLiteral"),
            //TokenKind:: Comment=> write!(f, "{}",     "Comment"),
            //TokenKind:: MultiLineComment=> write!(f, "{}",     "MultiLineComment"),
            TokenKind::Identifier => write!(f, "{}", "Identifier"),
            TokenKind::Space => write!(f, "{}", "Space"),
            TokenKind::NewLine => write!(f, "{}", "NewLine"),
            TokenKind::EOF => write!(f, "{}", "EOF"),
            TokenKind::Start => write!(f, "{}", "Start"),
            TokenKind::Unknown => write!(f, "{}", "Unknown"),
        }
    }
}

// Pos represents position in a file.
#[derive(Copy, Clone, Debug)]
pub struct Pos {
    line_no: usize,
    // pos indicates the position in the line.
    pos: usize,
}

impl Pos {
    pub fn new(line_no: usize, pos: usize) -> Pos {
        Pos { line_no, pos }
    }
    pub fn LineNo(&self) -> usize {
        self.line_no
    }
    pub fn Pos(&self) -> usize {
        self.pos
    }
}
