pub mod ast;
pub mod lexer;
// pub mod error;
use std::rc::Rc;

pub struct Parser {
    tokenizer: lexer::Tokenizer,
    defs: Vec<ast::Definition>,
}

impl Parser {
    fn new(content: String) -> Parser {
        Parser {
            tokenizer: lexer::Tokenizer::new(content),
            defs: Vec::new(),
        }
    }
    pub fn parse(content: String) -> Result<ast::File, (lexer::Pos, String)> {
        let mut p = Parser::new(content);
        loop {
            let mut t = &mut p.tokenizer;
            let tk = t.next_token();
            match tk {
                Ok(Some(tk)) => match tk.kind() {
                    lexer::TokenKind::StructKeyword => {
                        let err = p.parse_struct(tk);
                        if let Some(err)=err{
                            return Err(err);
                        }
                    }
                    lexer::TokenKind::NewLine=>{}
                    lexer::TokenKind::Space=>{}
                    lexer::TokenKind::EOF=>{
                return        Ok(ast::File::new(p.defs));
                    }
                    _ => {
                        return Err((
                            *tk.start_pos(),
                            String::from("Token kind not handled in parser"),
                        ));
                    }
                },
                
                Ok(None) => {
                    return Ok(ast::File::new(Vec::new()));
                }
                Err((pos, msg)) => {
                    return Err((pos, msg));
                }
            }
        }
        
    }

    fn parse_struct(&mut self, mut keyword: lexer::Token) -> Option<(lexer::Pos, String)> {
        let mut tk = self.tokenizer.next_token();
        let mut last_pos: lexer::Pos;
        match tk {
            Err(err) => {
                return Some(err);
            }

            Ok(Some(tk)) => {
                if *tk.kind() == lexer::TokenKind::Space {
                    last_pos = *tk.end_pos();
                } else {
                    return Some((
                        *keyword.end_pos(),
                        "Expecting space after struct keyword".to_string(),
                    ));
                }
            }
            Ok(None) => {
                return Some((
                    *keyword.end_pos(),
                    "Expecting space after struct keyword".to_string(),
                ))
            }
        }

        tk = self.tokenizer.next_token();
        let mut name;
        match tk {
            Err(err) => {
                return Some(err);
            }

            Ok(Some(tk)) => {
                if *tk.kind() == lexer::TokenKind::Identifier {
                    name = tk;
                    last_pos = *name.end_pos();
                } else {
                    return Some((
                        last_pos,
                        "Expecting name (identifier) after struct keyword".to_string(),
                    ));
                }
            }
            Ok(None) => {
                return Some((
                    last_pos,
                    "Expecting name (identifier) after struct keyword".to_string(),
                ))
            }
        }

        tk = self.tokenizer.next_token();
        match tk {
            Err(err) => {
                return Some(err);
            }

            Ok(Some(space_tk)) => {
                if *space_tk.kind() == lexer::TokenKind::Space {
                    tk = self.tokenizer.next_token();
                    last_pos = *space_tk.end_pos();
                } else {
                    tk = Ok(Some(space_tk));
                }
            }
            Ok(None) => {
                return Some((
                    last_pos,
                    "Expecting space or { after struct name".to_string(),
                ))
            }
        }

        let openCurly;
        match tk {
            Err(err) => {
                return Some(err);
            }

            Ok(Some(tk)) => {
                if *tk.kind() == lexer::TokenKind::OpenCurly {
                    openCurly = tk;
                    last_pos = *openCurly.end_pos();
                } else {
                    return Some((last_pos, "Expecting { after struct name".to_string()));
                }
            }
            Ok(None) => return Some((last_pos, "Expecting { after struct name".to_string())),
        }

        let res = self.consume_new_line_space();
        match res {
            Err(err) => {
                return Some(err);
            }

            Ok(Some(nl)) => {
                last_pos = *nl.end_pos();
            }

            Ok(None) => {
                let tk = self
                    .tokenizer
                    .next_token_checked(lexer::TokenKind::CloseCurly, lexer::TokenKind::CloseCurly);
                match tk {
                    Err(err) => {
                        return Some(err);
                    }
                    Ok(Some(close)) => {
                        self.defs.push(ast::Definition::StructDef(ast::Struct::new(
                            *keyword.start_pos(),
                            *close.end_pos(),
                            name,
                            Vec::new(),
                        )));
                        return None;
                    }
                    _ => {}
                }
            }
        }
        let mut prop_defs = Vec::<ast::PropDef>::new();
        loop {
            tk = self.tokenizer.next_token();
            match tk {
                Err(err) => {
                    return Some(err);
                }
                Ok(Some(tk)) => {
                    if *tk.kind() == lexer::TokenKind::NewLine {
                        last_pos = *tk.end_pos();
                    }else if *tk.kind() == lexer::TokenKind::CloseCurly {
                        self.defs.push(ast::Definition::StructDef(ast::Struct::new(
                            *keyword.start_pos(),
                            *tk.end_pos(),
                            name,
                            prop_defs,
                        )));
                        return None;
                    } else {
                        self.tokenizer.push_token(tk);
                        let res = self.parse_struct_prop_def(last_pos);
                        match res {
                            Err(err) => {
                                return Some(err);
                            }
                            Ok(b) => {
                                last_pos = *b.as_ref().end();
                                prop_defs.push(*b);
                                
                            }
                           
                        }
                    }
                }
                Ok(None)=> return Some((
                    last_pos,
                    "Expecting property definition or }".to_string(),
                ))
            }
        }

        //     _ => {
        //         return Some((
        //             last_pos,
        //             "Expecting space after struct name".to_string(),
        //         ));
        //     }
        // }

        // self.parse_struct_prop_def(next_tk);

        None
    }

    fn consume_space(&mut self) -> Result<(Option<lexer::Token>, usize), (lexer::Pos, String)> {
        let mut count: usize = 0;

        loop {
            let tk = self.tokenizer.next_token();
            match tk {
                Err(tk) => return Err(tk),
                Ok(Some(tk)) => {
                    if *tk.kind() == lexer::TokenKind::Space {
                        count += 1;
                    } else {
                        return Ok((Some(tk), count));
                    }
                }
                Ok(None) => return Ok((None, count)),
            }
        }

        Ok((None, count))
    }

    fn consume_new_line_space(&mut self) -> Result<Option<lexer::Token>, (lexer::Pos, String)> {
        let mut retTk: Option<lexer::Token> = None;

        loop {
            let tk = self
                .tokenizer
                .next_token_checked(lexer::TokenKind::NewLine, lexer::TokenKind::Space);
            match tk {
                Err(tk) => return Err(tk),
                Ok(Some(tk)) => {
                    if *tk.kind() == lexer::TokenKind::NewLine {
                        match &mut retTk {
                            None => retTk = Some(tk),
                            Some(v) => v.add_discarded(tk),
                        };
                    } else if *tk.kind() == lexer::TokenKind::Space {
                        match &mut retTk {
                            Some(v) => v.add_discarded(tk),
                            // can add to discarded only if new line is encountered already.
                            _ => {}
                        };
                    } else {
                        // Not possible
                        return Err((
                            lexer::Pos::new(0, 0),
                            "Parser handling error. Hit a dead end".to_string(),
                        ));
                    }
                }
                Ok(None) => return Ok(retTk),
            }
        }
    }

    fn parse_struct_prop_def(
        &mut self,
        mut last_pos: lexer::Pos,
    ) -> Result<Box<ast::PropDef>, (lexer::Pos, String)> {
        let mut names = Box::new(Vec::<lexer::Token>::new());
        let mut ident_count: usize = 0;
        loop {
            let tk = self.tokenizer.next_token();
            match tk {
                Err(err) => {
                    return Err(err);
                }

                Ok(Some(tk)) => {
                    if *tk.kind() == lexer::TokenKind::Identifier || tk.is_datatype() {
                        last_pos = *tk.end_pos();
                        (*names).push(tk);
                        ident_count += 1;
                    } else if *tk.kind() == lexer::TokenKind::Space {
                        last_pos = *tk.end_pos();
                    // do nothing
                    } else {
                        if *tk.kind() == lexer::TokenKind::NewLine {
                            if ident_count < 2 {
                                return Err((
                                    last_pos,
                                    "Expecting name and datatype for property defintion"
                                        .to_string(),
                                ));
                            } else {
                                let list = names.as_ref();
                                let start = *(list[0].start_pos());
                                let datatype = (*names).pop().unwrap();
                                return Ok(Box::new(ast::PropDef::new(
                                    start,
                                    *datatype.end_pos(),
                                    *names,
                                    datatype,
                                )));
                            }
                        }else{
                            return Err((
                                last_pos,
                                "Expecting valid property definition".to_string(),
                            ));
                        }
                        
                        
                    }
                }
                Ok(None) => {
                    return Err((
                        last_pos,
                        "Expecting name (identifier) after struct keyword".to_string(),
                    ))
                }
            }
        }
    }
}
