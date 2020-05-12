pub mod ast;
pub mod lexer;
// pub mod error;
// use std::rc::Rc;
use crate::error::Error;

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
    pub fn parse(content: String) -> Result<ast::File, Error> {
        let mut p = Parser::new(content);
        loop {
            let t = &mut p.tokenizer;
            let tk = t.next_token(true);
            match tk {
                Ok(tk) => match tk.kind() {
                    lexer::TokenKind::StructKeyword => {
                        let err = p.parse_struct(tk);
                        if let Some(err) = err {
                            return Err(err);
                        }
                    }

                    lexer::TokenKind::FuncKeyword => {
                        let err = p.parse_func(tk);
                        if let Some(err) = err {
                            return Err(err);
                        }
                    }
                    lexer::TokenKind::NewLine => {}
                    lexer::TokenKind::EOF => {
                        return Ok(ast::File::new(p.defs));
                    }
                    _ => {
                        return Err(Error::new(
                            *tk.start_pos(),
                            String::from("Token kind not handled in parser"),
                        ));
                    }
                },
                Err(err) => {
                    return Err(err);
                }
            }
        }
    }

    fn parse_struct(&mut self, keyword: lexer::Token) -> Option<Error> {
        let mut tk = self.tokenizer.next_token(false);
        let mut last_pos: lexer::Pos;
        match tk {
            Err(err) => {
                return Some(err);
            }

            Ok(tk) => {
                if *tk.kind() == lexer::TokenKind::Space {
                    last_pos = *tk.end_pos();
                } else {
                    return Some(Error::new(
                        *keyword.end_pos(),
                        "Expecting space after struct keyword".to_string(),
                    ));
                }
            }
        }

        tk = self.tokenizer.next_token(false);
        let name;
        match tk {
            Err(err) => {
                return Some(err);
            }

            Ok(tk) => {
                if *tk.kind() == lexer::TokenKind::Identifier {
                    name = tk;
                // last_pos = *name.end_pos();
                } else {
                    return Some(Error::new(
                        last_pos,
                        "Expecting name (identifier) after struct keyword".to_string(),
                    ));
                }
            }
        }

        tk = self.tokenizer.next_token(true);

        let open_curly;
        match tk {
            Err(err) => {
                return Some(err);
            }

            Ok(tk) => {
                if *tk.kind() == lexer::TokenKind::OpenCurly {
                    open_curly = tk;
                    last_pos = *open_curly.end_pos();
                } else {
                    return Some(Error::new(*tk.start_pos(), "Expecting { after struct name".to_string()));
                }
            }
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
                let tk = self.tokenizer.next_token_checked(
                    false,
                    lexer::TokenKind::CloseCurly,
                    lexer::TokenKind::CloseCurly,
                );
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
            tk = self.tokenizer.next_token(true);
            match tk {
                Err(err) => {
                    return Some(err);
                }
                Ok(tk) => {
                    if *tk.kind() == lexer::TokenKind::NewLine {
                        last_pos = *tk.end_pos();
                    } else if *tk.kind() == lexer::TokenKind::CloseCurly {
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
            }
        }
    }

    fn parse_func(&mut self, keyword: lexer::Token) -> Option<Error> {
        let mut tk = self.tokenizer.next_token(false);
        let mut last_pos: lexer::Pos;
        match tk {
            Err(err) => {
                return Some(err);
            }

            Ok(tk) => {
                if *tk.kind() == lexer::TokenKind::Space {
                    last_pos = *tk.end_pos();
                } else {
                    return Some(Error::new(
                        *keyword.end_pos(),
                        "Expecting space after fn keyword".to_string(),
                    ));
                }
            }
        }

        tk = self.tokenizer.next_token(false);
        let name;
        match tk {
            Err(err) => {
                return Some(err);
            }

            Ok(tk) => {
                if *tk.kind() == lexer::TokenKind::Identifier {
                    name = tk;
                    last_pos = *name.end_pos();
                } else {
                    return Some(Error::new(
                        last_pos,
                        "Expecting name (identifier) after fn keyword".to_string(),
                    ));
                }
            }
        }

        tk = self.tokenizer.next_token(true);

        let open_brace;
        match tk {
            Err(err) => {
                return Some(err);
            }

            Ok(tk) => {
                if *tk.kind() == lexer::TokenKind::OpenBrace {
                    open_brace = tk;
                    last_pos = *open_brace.end_pos();
                } else {
                    return Some(Error::new(last_pos, "Expecting ( after Func name".to_string()));
                }
            }
        }

        let res = self.parse_func_param_defs(last_pos);
        let param_defs;
        match res {
            Err(err) => {
                return Some(err);
            }
            Ok((defs, pos)) => {
                last_pos = pos;
                param_defs = defs;
            }
        }

        let return_type = match self.parse_return_type() {
            Err(err) => return Some(err),
            Ok(Some(tk)) => {
                last_pos = *tk.end_pos();
                Some(tk)
            }
            Ok(None) => None,
        };

        tk = self.tokenizer.next_token(true);
        let open_curly;
        match tk {
            Err(err) => {
                return Some(err);
            }

            Ok(tk) => {
                if *tk.kind() == lexer::TokenKind::OpenCurly {
                    open_curly = tk;
                    last_pos = *open_curly.end_pos();
                } else {
                    return Some(Error::new(last_pos, "Expecting { after Func name".to_string()));
                }
            }
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
                let tk = self.tokenizer.next_token_checked(
                    false,
                    lexer::TokenKind::CloseCurly,
                    lexer::TokenKind::CloseCurly,
                );
                match tk {
                    Err(err) => {
                        return Some(err);
                    }
                    Ok(Some(close)) => {
                        self.defs.push(ast::Definition::FuncDef(ast::Func::new(
                            *keyword.start_pos(),
                            *close.end_pos(),
                            name,
                            *param_defs,
                            return_type,
                            Vec::new(),
                        )));
                        return None;
                    }
                    _ => {}
                }
            }
        }
        let res = self.parse_block_content(last_pos);
        match res {
            Err(err) => return Some(err),
            Ok((stmts, pos)) => {
                self.defs.push(ast::Definition::FuncDef(ast::Func::new(
                    *keyword.start_pos(),
                    pos,
                    name,
                    *param_defs,
                    return_type,
                    stmts,
                )));
                return None;
            }
        }
    }

    // fn consume_space(&mut self) -> Result<(Option<lexer::Token>, usize), Error> {
    //     let mut count: usize = 0;
    //     loop {
    //         let tk = self.tokenizer.next_token(false);
    //         match tk {
    //             Err(tk) => return Err(tk),
    //             Ok(tk) => {
    //                 if *tk.kind() == lexer::TokenKind::Space {
    //                     count += 1;
    //                 } else {
    //                     return Ok((Some(tk), count));
    //                 }
    //             }
    //         }
    //     }
    // }

    fn consume_new_line_space(&mut self) -> Result<Option<lexer::Token>, Error> {
        let mut ret_tk: Option<lexer::Token> = None;

        loop {
            let tk = self.tokenizer.next_token_checked(
                false,
                lexer::TokenKind::NewLine,
                lexer::TokenKind::Space,
            );
            match tk {
                Err(tk) => return Err(tk),
                Ok(Some(tk)) => {
                    if *tk.kind() == lexer::TokenKind::NewLine {
                        match &mut ret_tk {
                            None => ret_tk = Some(tk),
                            Some(v) => v.add_discarded(tk),
                        };
                    } else if *tk.kind() == lexer::TokenKind::Space {
                        match &mut ret_tk {
                            Some(v) => v.add_discarded(tk),
                            // can add to discarded only if new line is encountered already.
                            _ => {}
                        };
                    } else {
                        // Not possible
                        return Err(Error::new(
                            lexer::Pos::new(0, 0),
                            "Parser handling error. Hit a dead end".to_string(),
                        ));
                    }
                }
                Ok(None) => return Ok(ret_tk),
            }
        }
    }

    fn parse_struct_prop_def(
        &mut self,
        mut last_pos: lexer::Pos,
    ) -> Result<Box<ast::PropDef>, Error> {
        let mut names = Box::new(Vec::<lexer::Token>::new());
        let mut ident_count: usize = 0;
        loop {
            let tk = self.tokenizer.next_token(false);
            match tk {
                Err(err) => {
                    return Err(err);
                }

                Ok(tk) => {
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
                                return Err(Error::new(
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
                        } else {
                            return Err(Error::new(
                                last_pos,
                                "Expecting valid property definition".to_string(),
                            ));
                        }
                    }
                }
            }
        }
    }

    fn parse_func_param_defs(
        &mut self,
        mut last_pos: lexer::Pos,
    ) -> Result<(Box<Vec<ast::ParamDef>>, lexer::Pos), Error> {
        let mut names = Box::new(Vec::<lexer::Token>::new());
        let mut prop_defs = Box::new(Vec::<ast::ParamDef>::new());
        let mut found_comma = false;
        loop {
            let tk = self.tokenizer.next_token(false);
            match tk {
                Err(err) => {
                    return Err(err);
                }

                Ok(tk) => {
                    if *tk.kind() == lexer::TokenKind::Identifier || tk.is_datatype() {
                        if (&*names).len() > 1 {
                            if !found_comma {
                                let list = names.as_ref();
                                let start = *(list[0].start_pos());
                                prop_defs.push(ast::ParamDef::new(
                                    start,
                                    *tk.end_pos(),
                                    *names,
                                    tk,
                                ));
                                names = Box::new(Vec::<lexer::Token>::new());
                            }
                        } else {
                            last_pos = *tk.end_pos();
                            (*names).push(tk);
                        }
                        found_comma = false;
                    } else if *tk.kind() == lexer::TokenKind::Space {
                        last_pos = *tk.end_pos();
                    // do nothing
                    } else if *tk.kind() == lexer::TokenKind::Comma {
                        last_pos = *tk.end_pos();
                        found_comma = true;
                    // do nothing
                    } else {
                        if *tk.kind() == lexer::TokenKind::CloseBrace {
                            if names.len() != 0 {
                                return Err(Error::new(
                                    last_pos,
                                    "Incorrect parameter definition".to_string(),
                                ));
                            } else {
                                return Ok((prop_defs, last_pos));
                            }
                        } else {
                            return Err(Error::new(
                                last_pos,
                                "Expecting valid paramter definition".to_string(),
                            ));
                        }
                    }
                }
            }
        }
    }

    fn parse_return_type(&mut self) -> Result<Option<lexer::Token>, Error> {
        return self.tokenizer.next_token_checked(
            true,
            lexer::TokenKind::Identifier,
            lexer::TokenKind::Identifier,
        );
    }

    fn parse_block_content(
        &mut self,
        mut last_pos: lexer::Pos,
    ) -> Result<(Vec<ast::Stmt>, lexer::Pos), Error> {
        let mut stmts = Vec::<ast::Stmt>::new();
        loop {
            let res = self.consume_new_line_space();
            match res {
                Err(err) => {
                    return Err(err);
                }
                Ok(Some(nl)) => {
                    last_pos = *nl.end_pos();
                }
                _ => {}
            }
            let tk = self.tokenizer.next_token_checked(
                true,
                lexer::TokenKind::CloseCurly,
                lexer::TokenKind::CloseCurly,
            );
            match tk {
                Err(err) => {
                    return Err(err);
                }
                Ok(Some(tk)) => {
                    return Ok((stmts, *tk.end_pos()));
                }
                _ => {}
            }
            let assign = self.parse_assign_stmt();
            match assign {
                Err(err) => {
                    return Err(err);
                }
                Ok(Some(assign)) => {
                    stmts.push(ast::Stmt::Assign(assign));
                    continue;
                }
                _ => {}
            }

            let expr = self.parse_expr(lexer::TokenKind::NewLine, lexer::TokenKind::NewLine);
            match expr {
                Err(err) => {
                    return Err(err);
                }
                Ok((expr, _)) => {
                    stmts.push(ast::Stmt::Expr(expr));
                    continue;
                }
                _ => {}
            }
        }
    }

    fn parse_assign_stmt(&mut self) -> Result<Option<ast::Assign>, Error> {
        Ok(None)
    }

    fn parse_expr(
        &mut self,
        delimiter1: lexer::TokenKind,
        delimiter2: lexer::TokenKind
    ) -> Result<(ast::Expr,lexer::Token) , Error> {
        let mut expr: Option<ast::Expr> = None;
        let mut operator: Option<lexer::Token> = None;
        loop {
            let tk = self.tokenizer.next_token(false);
            let mut loop_expr: Option<ast::Expr> = None;
            match tk {
                Err(err) => {
                    return Err(err);
                }
                Ok(tk) => {
                    if *tk.kind() == delimiter1 || *tk.kind() == delimiter2 {
                        if let None = operator {
                            if let Some(expr) = expr {
                                return Ok((expr,tk));
                            }
                            return Err(Error::new(
                                *tk.start_pos(),
                                "Expecting a valid expression".to_string(),
                            ));
                        }
                        return Err(Error::new(*tk.start_pos(), "Incomplete expr".to_string()));
                    } else if tk.kind().is_binary_operator() {
                        operator = Some(tk);
                    } else {
                        match tk.kind() {
                            lexer::TokenKind::Space => {} //skip
                            lexer::TokenKind::Int | lexer::TokenKind::StringLiteral => {
                                loop_expr = Some(ast::Expr::Literal(tk));
                            }
                            lexer::TokenKind::Identifier => match self.parse_identifier_expr(tk) {
                                Err(err) => return Err(err),
                                Ok(ident_expr) => {
                                    loop_expr = Some(ident_expr);
                                }
                            },
                            _ => return Err(Error::new(*tk.start_pos(), "Not handled yet".to_string())),
                        }
                    }
                }
            }
            if let Some(loop_expr_temp) = loop_expr {
                if let Some(op) = operator {
                    match expr {
                        Some(ast::Expr::BinaryExpr(lhs)) => {
                            expr = Some(ast::Expr::BinaryExpr(lhs.add(op, loop_expr_temp)));
                        }
                        Some(lhs) => {
                            let pos = lhs.pos();
                            let end = loop_expr_temp.end();
                            expr = Some(ast::Expr::BinaryExpr(ast::BinaryExpr::new(
                                Box::new(lhs),
                                Box::new(loop_expr_temp),
                                op,
                                pos,
                                end,
                            )));
                        }
                        None => {} //Not possible if op is not None
                    }
                    operator = None;
                } else {
                    expr = Some(loop_expr_temp);
                }
            }
        }
    }

    fn parse_identifier_expr(
        &mut self,
        iden: lexer::Token,
    ) -> Result<ast::Expr, Error> {
        let tk = self.tokenizer.next_token(true);
        match tk {
            Err(err) => {
                return Err(err);
            }
            Ok(tk) => match tk.kind() {
                lexer::TokenKind::OpenBrace => {
                    return self.parse_func_call(iden, tk);
                }
                _ => return Err(Error::new(*tk.start_pos(), "Not handled yet".to_string())),
            },
        }
    }

    fn parse_func_call(
        &mut self,
        iden: lexer::Token,
        brace: lexer::Token,
    ) -> Result<ast::Expr, Error> {
        let mut expr: Option<ast::Expr>;
        let mut operator: Option<lexer::Token>;
        let mut args = Vec::<ast::Expr>::new();
        loop {
            let tk = self.tokenizer.next_token_checked(
                true,
                lexer::TokenKind::CloseBrace,
                lexer::TokenKind::CloseBrace,
            );
            match tk {
                Err(err) => {
                    return Err(err);
                }
                Ok(Some(tk)) => {
                    return Ok(ast::Expr::FuncCall(ast::FuncCall::new(
                        *iden.start_pos(),
                        *tk.end_pos(),
                        iden,
                        args,
                    )));
                }
                Ok(None) => {
                    let res = self.parse_expr(lexer::TokenKind::Comma, lexer::TokenKind::CloseBrace);
                    match res {
                        Err(err) => {
                            return Err(err);
                        }
                        Ok((expr, delemiter_tk)) => {
                            args.push(expr);
                            if *delemiter_tk.kind() == lexer::TokenKind::CloseBrace{
                                return Ok(ast::Expr::FuncCall(ast::FuncCall::new(
                                    *iden.start_pos(),
                                    *delemiter_tk.end_pos(),
                                    iden,
                                    args,
                                )));
                            }
                        }
                    }
                }
            }
        }
    }
}
