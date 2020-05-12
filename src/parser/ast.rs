use crate::parser::lexer;

use std::mem;

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
    param_defs: Vec<ParamDef>,
    return_type: Option<lexer::Token>,
    stmts: Vec<Stmt>,
}

impl Func {
    pub fn new(
        pos: lexer::Pos,
        end: lexer::Pos,
        name: lexer::Token,
        param_defs: Vec<ParamDef>,
        return_type: Option<lexer::Token>,
        stmts: Vec<Stmt>,
    ) -> Func {
        Func {
            pos,
            end,
            name,
            param_defs,
            return_type,
            stmts,
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
    pub fn param_defs(&self) -> &Vec<ParamDef> {
        &self.param_defs
    }
    pub fn return_type(&self) -> &Option<lexer::Token> {
        &self.return_type
    }
    pub fn stms(&self) -> &Vec<Stmt> {
        &self.stmts
    }
}

// ex: name address string
#[derive(Debug)]
pub struct ParamDef {
    pos: lexer::Pos,
    end: lexer::Pos,
    names: Vec<lexer::Token>,
    datatype: lexer::Token,
}

impl ParamDef {
    pub fn new(
        pos: lexer::Pos,
        end: lexer::Pos,
        names: Vec<lexer::Token>,
        datatype: lexer::Token,
    ) -> ParamDef {
        ParamDef {
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
pub enum Stmt {
    Expr(Expr),
    Assign(Assign),
}

#[derive(Debug)]
pub struct Assign {
    lhs: lexer::Token,
    pos: lexer::Pos,
    end: lexer::Pos,
    operator: lexer::Token,
    rhs: Expr,
}

impl Assign {
    pub fn new(
        pos: lexer::Pos,
        end: lexer::Pos,
        lhs: lexer::Token,
        operator: lexer::Token,
        rhs: Expr,
    ) -> Assign {
        Assign {
            pos,
            end,
            lhs,
            operator,
            rhs,
        }
    }

    pub fn pos(&self) -> &lexer::Pos {
        &self.pos
    }
    pub fn end(&self) -> &lexer::Pos {
        &self.end
    }
    pub fn lhs(&self) -> &lexer::Token {
        &self.lhs
    }
    pub fn rhs(&self) -> &Expr {
        &self.rhs
    }
    pub fn operator(&self) -> &lexer::Token {
        &self.operator
    }
}

#[derive(Debug)]
pub struct If {
    pos: lexer::Pos,
    end: lexer::Pos,
    cond: Expr,
    stms: Vec<Stmt>,
    else_ifs: Vec<If>,
    els: Option<Vec<Stmt>>,
}

impl If {
    pub fn new(
        pos: lexer::Pos,
        end: lexer::Pos,
        cond: Expr,
        stms: Vec<Stmt>,
        else_ifs: Vec<If>,
        els: Option<Vec<Stmt>>,
    ) -> If {
        If {
            pos,
            end,
            cond,
            stms,
            else_ifs,
            els,
        }
    }

    pub fn pos(&self) -> &lexer::Pos {
        &self.pos
    }
    pub fn end(&self) -> &lexer::Pos {
        &self.end
    }
    pub fn cond(&self) -> &Expr {
        &self.cond
    }
    pub fn stms(&self) -> &Vec<Stmt> {
        &self.stms
    }
    pub fn else_ifs(&self) -> &Vec<If> {
        &self.else_ifs
    }
    pub fn els(&self) -> &Option<Vec<Stmt>> {
        &self.els
    }
}

#[derive(Debug)]
pub struct For {
    pos: lexer::Pos,
    end: lexer::Pos,
    cond: Expr,
    stms: Vec<Stmt>,
}

impl For {
    pub fn new(pos: lexer::Pos, end: lexer::Pos, cond: Expr, stms: Vec<Stmt>) -> For {
        For {
            pos,
            end,
            cond,
            stms,
        }
    }

    pub fn pos(&self) -> &lexer::Pos {
        &self.pos
    }
    pub fn end(&self) -> &lexer::Pos {
        &self.end
    }
    pub fn cond(&self) -> &Expr {
        &self.cond
    }
    pub fn stms(&self) -> &Vec<Stmt> {
        &self.stms
    }
}

#[derive(Debug)]
pub enum Expr {
    FuncCall(FuncCall),
    Identifier(lexer::Token),
    Literal(lexer::Token),
    BinaryExpr(BinaryExpr),
    CoveredExpr(Box<CoveredExpr>),
    Empty,
}

impl Expr {
    pub fn pos(&self) -> lexer::Pos {
        match self {
            Expr::FuncCall(expr) => *expr.pos(),
            Expr::BinaryExpr(expr) => *expr.pos(),
            Expr::CoveredExpr(expr) => *expr.pos(),
            Expr::Literal(tk) | Expr::Identifier(tk) => *tk.start_pos(),
            Expr::Empty => lexer::Pos::new(0, 0),
        }
    }

    pub fn end(&self) -> lexer::Pos {
        match self {
            Expr::FuncCall(expr) => *expr.end(),
            Expr::BinaryExpr(expr) => *expr.end(),
            Expr::CoveredExpr(expr) => *expr.end(),
            Expr::Literal(tk) | Expr::Identifier(tk) => *tk.end_pos(),
            Expr::Empty => lexer::Pos::new(0, 0),
        }
    }
}

#[derive(Debug)]
pub struct FuncCall {
    name: lexer::Token,
    pos: lexer::Pos,
    end: lexer::Pos,
    args: Vec<Expr>,
}

impl FuncCall {
    pub fn new(pos: lexer::Pos, end: lexer::Pos, name: lexer::Token, args: Vec<Expr>) -> FuncCall {
        FuncCall {
            pos,
            end,
            name,
            args,
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
    pub fn args(&self) -> &Vec<Expr> {
        &self.args
    }
}

#[derive(Debug)]
pub struct BinaryExpr {
    lhs: Box<Expr>,
    rhs: Box<Expr>,
    operator: lexer::Token,
    pos: lexer::Pos,
    end: lexer::Pos,
}

#[derive(Debug, PartialEq, Eq)]
enum Precedence {
    Lesser,
    Greater,
    Equal,
}

impl BinaryExpr {
    pub fn new(
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        operator: lexer::Token,
        pos: lexer::Pos,
        end: lexer::Pos,
    ) -> BinaryExpr {
        BinaryExpr {
            lhs,
            rhs,
            operator,
            pos,
            end,
        }
    }

    pub fn pos(&self) -> &lexer::Pos {
        &self.pos
    }
    pub fn end(&self) -> &lexer::Pos {
        &self.end
    }
    pub fn lhs(&self) -> &Box<Expr> {
        return &self.lhs;
    }
    pub fn rhs(&self) -> &Box<Expr> {
        return &self.rhs;
    }
    pub fn operator(&self) -> &lexer::Token {
        return &self.operator;
    }
    
    pub fn add(mut self, op: lexer::Token, rhs: Expr) -> BinaryExpr {
        let st = lexer::Pos::new(0, 0);
        match *self.rhs {
            Expr::BinaryExpr(expr) => {
                self.rhs = Box::new(Expr::BinaryExpr(expr.add(op, rhs)));
                return self;
            }
            _ => {}
        }
        let op1 = *self.operator.kind();
        let op2 = *self.operator().kind();
        if BinaryExpr::findPrecendence(op1, op2) == Precedence::Lesser {
            let st = self.rhs().pos();
            let end = rhs.end();
            let mut x: Box<Expr> = Box::new(Expr::Empty);
            mem::swap(&mut self.rhs, &mut x);
            let new_expr = BinaryExpr::new(x, Box::new(rhs), op, st, end);
            self.rhs = Box::new(Expr::BinaryExpr(new_expr));
            return self;
        } else {
            let end = rhs.end();
            return BinaryExpr::new(Box::new(Expr::BinaryExpr(self)), Box::new(rhs), op, st, end);
        }
    }

    // pub fn add_to_leaf(&mut self, op: lexer::Token, rhs: Expr) -> BinaryExpr {}

    fn findPrecendence(op1: lexer::TokenKind, op2: lexer::TokenKind) -> Precedence {
        match op1 {
            lexer::TokenKind::Plus | lexer::TokenKind::Minus => match op2 {
                lexer::TokenKind::Plus | lexer::TokenKind::Minus => Precedence::Equal,
                lexer::TokenKind::Star | lexer::TokenKind::Slash | lexer::TokenKind::Dot => {
                    Precedence::Lesser
                }
                lexer::TokenKind::Greater
                | lexer::TokenKind::Lesser
                | lexer::TokenKind::GreaterEqual
                | lexer::TokenKind::LesserEqual
                | lexer::TokenKind::DoubleEqual
                | lexer::TokenKind::NotEqual
                | lexer::TokenKind::And
                | lexer::TokenKind::Or => Precedence::Greater,
                _ => Precedence::Equal,
            },

            lexer::TokenKind::Star | lexer::TokenKind::Slash => match op2 {
                lexer::TokenKind::Plus | lexer::TokenKind::Minus => Precedence::Greater,
                lexer::TokenKind::Star | lexer::TokenKind::Slash => Precedence::Equal,
                lexer::TokenKind::Dot => Precedence::Lesser,
                lexer::TokenKind::Greater
                | lexer::TokenKind::Lesser
                | lexer::TokenKind::GreaterEqual
                | lexer::TokenKind::LesserEqual
                | lexer::TokenKind::DoubleEqual
                | lexer::TokenKind::NotEqual
                | lexer::TokenKind::And
                | lexer::TokenKind::Or => Precedence::Greater,
                _ => Precedence::Equal,
            },

            lexer::TokenKind::Greater
            | lexer::TokenKind::Lesser
            | lexer::TokenKind::GreaterEqual
            | lexer::TokenKind::LesserEqual
            | lexer::TokenKind::DoubleEqual
            | lexer::TokenKind::NotEqual => match op2 {
                lexer::TokenKind::Plus
                | lexer::TokenKind::Minus
                | lexer::TokenKind::Star
                | lexer::TokenKind::Slash
                | lexer::TokenKind::Dot => Precedence::Lesser,
                lexer::TokenKind::Greater
                | lexer::TokenKind::Lesser
                | lexer::TokenKind::GreaterEqual
                | lexer::TokenKind::LesserEqual
                | lexer::TokenKind::DoubleEqual
                | lexer::TokenKind::NotEqual => Precedence::Equal,
                lexer::TokenKind::And | lexer::TokenKind::Or => Precedence::Greater,
                _ => Precedence::Equal,
            },

            lexer::TokenKind::And | lexer::TokenKind::Or => match op2 {
                lexer::TokenKind::Plus
                | lexer::TokenKind::Minus
                | lexer::TokenKind::Star
                | lexer::TokenKind::Slash
                | lexer::TokenKind::Greater
                | lexer::TokenKind::Lesser
                | lexer::TokenKind::GreaterEqual
                | lexer::TokenKind::LesserEqual
                | lexer::TokenKind::DoubleEqual
                | lexer::TokenKind::NotEqual
                | lexer::TokenKind::Dot => Precedence::Lesser,

                lexer::TokenKind::And | lexer::TokenKind::Or => Precedence::Equal,
                _ => Precedence::Equal,
            },

            lexer::TokenKind::Dot => match op2 {
                lexer::TokenKind::Plus
                | lexer::TokenKind::Minus
                | lexer::TokenKind::Star
                | lexer::TokenKind::Slash
                | lexer::TokenKind::Greater
                | lexer::TokenKind::Lesser
                | lexer::TokenKind::GreaterEqual
                | lexer::TokenKind::LesserEqual
                | lexer::TokenKind::AssignEqual
                | lexer::TokenKind::DoubleEqual
                | lexer::TokenKind::NotEqual
                | lexer::TokenKind::Not
                | lexer::TokenKind::And
                | lexer::TokenKind::Or => Precedence::Greater,
                lexer::TokenKind::Dot => Precedence::Equal,
                _ => Precedence::Equal,
            },
            _ => Precedence::Equal,
        }
    }
}

#[derive(Debug)]
pub struct CoveredExpr {
    expr: Expr,
    pos: lexer::Pos,
    end: lexer::Pos,
}

impl CoveredExpr {
    pub fn new(expr: Expr, pos: lexer::Pos, end: lexer::Pos) -> CoveredExpr {
        CoveredExpr { expr, pos, end }
    }

    pub fn pos(&self) -> &lexer::Pos {
        &self.pos
    }
    pub fn end(&self) -> &lexer::Pos {
        &self.end
    }
    pub fn expr(&self) -> &Expr {
        return &self.expr;
    }
}
