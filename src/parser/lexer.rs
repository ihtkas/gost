use std::collections::HashMap;
use std::rc::Rc;

pub struct Tokenizer {
    content: Vec<char>,
    cur_pos: usize,
    cur_line_no: usize,
    next_tokens: Result<Vec<Token>, (Pos, String)>,
    keywords: HashMap<String, TokenKind>,
    delimiters: HashMap<char, TokenKind>,
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
                cur_pos: 0,
                cur_line_no: 1,
                keywords: HashMap::new(),
                delimiters: HashMap::new(),
            };
            t.keywords
                .insert(String::from("struct"), TokenKind::StructKeyword);
            t.keywords
                .insert(String::from("fn"), TokenKind::FuncKeyword);
            t.keywords
                .insert(String::from("string"), TokenKind::StringDTKeyword);
            t.keywords
                .insert(String::from("uint"), TokenKind::UIntDTKeyword);
            t.delimiters.insert('\n', TokenKind::NewLine);
            t.delimiters.insert('{', TokenKind::OpenCurly);
            t.delimiters.insert('}', TokenKind::CloseCurly);
            t.delimiters.insert('(', TokenKind::OpenBrace);
            t.delimiters.insert(')', TokenKind::CloseBrace);
            t.delimiters.insert(',', TokenKind::Comma);
            t.delimiters.insert('+', TokenKind::Plus);
            t.delimiters.insert('-', TokenKind::Minus);
            t.delimiters.insert('*', TokenKind::Star);
            t.delimiters.insert('/', TokenKind::Slash);
            t.delimiters.insert('.', TokenKind::Dot);
            t
        };
    }

    fn pop_cur_char(&mut self) -> Char {
        if self.cur_pos < self.content.len() {
            let ind = self.cur_pos;
            let ch = self.content[ind];
            if ch == '\n' {
                self.cur_line_no += 1
            }
            self.cur_pos += 1;
            Char::ValidChar(ch)
        } else {
            Char::EOF
        }
    }

    fn peek_cur_char(&self) -> Char {
        if self.cur_pos < self.content.len() {
            Char::ValidChar(self.content[self.cur_pos])
        } else {
            Char::EOF
        }
    }

    fn advance_cur_char(&mut self) {
        if self.cur_pos < self.content.len() {
            self.cur_pos += 1;
        }
    }

    pub fn next_token(&mut self) -> Result<Option<Token>, (Pos, String)> {
        if let Ok(list) = &mut self.next_tokens {
            let tk = list.pop();
            if let Some(tk) = tk {
                return Ok(Some(tk));
            }
        }
        let space = self.handle_spaces();
        if let Some(_) = space {
            return Ok(space);
        }

        let word = self.handle_word();
        if let Some(_) = word {
            return Ok(word);
        }

        let delimiter = self.handle_delimiters();
        if let Some(_) = delimiter {
            return Ok(delimiter);
        }

        let str_lit = self.handle_string();
        if let Ok(Some(_)) = str_lit {
            return str_lit;
        }

        let number = self.handle_number();
        if let Some(_) = number {
            return Ok(number);
        }
        return Ok(self.handle_unknown());
    }

    pub fn next_token_checked(
        &mut self,
        kind1: TokenKind,
        kind2: TokenKind,
    ) -> Result<Option<Token>, (Pos, String)> {
        let res = self.next_token();
        match res {
            Ok(Some(tk)) => {
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
            Ok(None) => Ok(None),
            Err(err) => Err(err),
        }
    }

    pub fn push_token(&mut self, tk: Token) {
        match &mut self.next_tokens {
            Ok(list) => list.push(tk),
            _ => {}
        }
    }

    fn handle_unknown(&mut self) -> Option<Token> {
        let ch = self.peek_cur_char();
        if let Char::EOF = ch {
            Some(Token::eof_token(Pos::new(self.cur_line_no, self.cur_pos)))
        } else {
            let pos = self.cur_pos;
            let ch = self.pop_cur_char().unwrap();
            Some(Token::new(
                TokenKind::Unknown,
                ch.to_string(),
                Pos::new(self.cur_line_no, pos),
                Pos::new(self.cur_line_no, pos),
                None,
            ))
        }
    }

    fn handle_spaces(&mut self) -> Option<Token> {
        let ch = self.peek_cur_char();
        match ch {
            Char::ValidChar(ch) => {
                if ch == ' ' {
                    let pos = self.cur_pos;
                    self.advance_cur_char();
                    let mut str = String::from(" ");
                    let mut flag = match self.peek_cur_char() {
                        Char::ValidChar(' ') => true,
                        _ => false,
                    };

                    while flag {
                        str.push_str(" ");
                        self.advance_cur_char();
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
                    self.advance_cur_char();
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
                    self.advance_cur_char();
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

    fn handle_string(&mut self) -> Result<Option<Token>, (Pos, String)> {
        // TODO: Escape sequence
        let ch = self.peek_cur_char();
        match ch {
            Char::ValidChar(ch) => match ch {
                '"' => {
                    let pos = self.cur_pos;
                    let mut str_lit = String::new();
                    self.advance_cur_char();
                    let mut flag = true;

                    while flag {
                        match self.peek_cur_char() {
                            Char::EOF => {
                                return Err((
                                    Pos::new(self.cur_line_no, pos),
                                    String::from("Unterminated String Literal"),
                                ));
                            }
                            Char::ValidChar(ch) => match ch {
                                '"' => {
                                    self.advance_cur_char();
                                    flag = false;
                                }
                                '\n' => {
                                    return Err((
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

    fn handle_delimiters(&mut self) -> Option<Token> {
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
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    StructKeyword,
    FuncKeyword,
    StringDTKeyword,
    UIntDTKeyword,
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
    Dot,
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
        match self.kind{
            TokenKind::StringDTKeyword=>{true}
            TokenKind::UIntDTKeyword=>{true}
            _=>{false}
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
