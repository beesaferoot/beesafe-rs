/*

Parser module: Houses implementation for beesafe language parser

*/

use miette::SourceSpan;
use thiserror::Error;

use crate::ast::*;
use crate::lexer::{Lexer, TType, Token};

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Option<Token>,
    peek_token: Option<Token>,
    operator_stack: Vec<Token>,
    operand_stack: Vec<Node>,
    errors: Vec<Error>,
}

#[derive(Clone, Debug, Error)]
pub enum ParseError {
    #[error("Missing identifier: {0}")]
    MissingIdent(String),
    #[error("Undetermined type: {0}")]
    UndeterminedType(String),
    #[error("Placeholder error: {0}")]
    PlaceHolder(String),
    #[error("Invalid syntax: {0}")]
    InvalidSyntax(String),
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        Parser {
            lexer,
            current_token: None,
            peek_token: None,
            operator_stack: vec![],
            operand_stack: vec![],
            errors: vec![],
        }
    }

    pub fn source(&self) -> &'static str {
        self.lexer.source()
    }

    pub fn has_errors(&self) -> bool {
        self.errors.is_empty() == false
    }

    pub fn show_errors(&self) {
        for err in self.errors.clone().into_iter() {
            eprint!(
                "{:?}",
                miette::Report::new(err.clone()).with_source_code(err.src)
            );
        }
    }

    fn consume_next_token(&mut self) {
        match self.peek_token.clone() {
            Some(token) => {
                self.current_token = Some(token);
                self.peek_token = Some(self.lexer.next_token());
            }
            None => {
                self.current_token = Some(self.lexer.next_token());
                self.peek_token = Some(self.lexer.next_token());
            }
        }
    }

    // parse expression using the shunting yard technique (https://en.wikipedia.org/wiki/Shunting_yard_algorithm)
    fn parse_expression(&mut self) -> Node {
        /*

        (expr) -> (produce)
        (produce) -> num|id|define|string_literal|(expr)|null
        num: <-> 1|2|..|intergers
        id: <-> identifiers
        string_literal <-> "string literal" | ''
        null <-> null keyword

        */
        self.expression();
        let node = self.operand_stack.pop().unwrap_or(Node::Error(Error {
            src: self.source(),
            span: (0, 0).into(),
            error_type: ParseError::PlaceHolder(format!("")),
        }));
        node
    }

    // expr -> produce
    fn expression(&mut self) {
        let null_token = Token {
            lexeme: "".to_string(),
            ttype: TType::Null,
            offset: -1,
        };
        self.operator_stack.push(null_token);
        self.produce();
        let mut look_ahead = self.peek_token.as_ref().unwrap();
        let mut ttype = look_ahead.ttype;
        while self.resolve_binary_op(ttype) {
            let op_token = look_ahead.clone();
            self.push_operator(op_token);
            self.consume_next_token();
            self.consume_next_token();
            self.produce();
            ttype = self.peek_token.as_ref().unwrap().ttype;
            look_ahead = self.peek_token.as_ref().unwrap();
        }
        let mut op = self.operator_stack.last().unwrap();
        while op.ttype != TType::Error && op.ttype != TType::Null {
            let is_binary = self.resolve_binary_op(op.ttype);
            self.pop_operator(is_binary);
            op = self.operator_stack.last().unwrap();
        }
        self.operator_stack.pop();
    }

    // (produce) -> ...
    fn produce(&mut self) {
        let curr_token = self.current_token.as_ref().unwrap();
        let ttype = curr_token.ttype;
        match ttype {
            TType::Num => {
                let node = self.create_node(&ttype);
                self.operand_stack.push(node)
            }
            TType::Null => {
                let node = self.create_node(&ttype);
                self.operand_stack.push(node);
            }
            TType::Id => {
                let node = self.create_node(&ttype);
                self.operand_stack.push(node);
            }
            TType::Literal => {
                let node = self.create_node(&ttype);
                self.operand_stack.push(node);
            }
            TType::False => {
                let node = self.parse_bool();
                self.operand_stack.push(node);
            }
            TType::True => {
                let node = self.parse_bool();
                self.operand_stack.push(node);
            }
            _ => {
                if ttype == TType::Minus || ttype == TType::Plus {
                    let token = curr_token.clone();
                    self.push_operator(token);
                    self.consume_next_token();
                    self.produce();
                    self.pop_operator(false);
                } else {
                    //TODO: store error
                }
            }
        }
    }

    fn resolve_binary_op(&self, op_type: TType) -> bool {
        match op_type {
            TType::Plus => true,
            TType::Minus => true,
            TType::Asterisk => true,
            TType::Div => true,
            TType::Lt => true,
            TType::LtEq => true,
            TType::Gt => true,
            TType::GtEq => true,
            TType::NotEq => true,
            TType::Eq => true,
            TType::Assign => true,
            _ => false,
        }
    }

    fn resolve_precedence(&self, op_type: &TType) -> Precendence {
        match op_type {
            TType::Plus => Precendence::Add(2),
            TType::Minus => Precendence::Minus(2),
            TType::Asterisk => Precendence::Mul(3),
            TType::Div => Precendence::Div(3),
            TType::Lt => Precendence::Lt(1),
            TType::LtEq => Precendence::LtEq(1),
            TType::Gt => Precendence::Gt(1),
            TType::GtEq => Precendence::GtEq(1),
            TType::NotEq => Precendence::NotEq(1),
            TType::Eq => Precendence::Eq(1),
            TType::Assign => Precendence::Assign(1),
            _ => Precendence::Base(0),
        }
    }

    fn push_operator(&mut self, op: Token) {
        let precedence = self.resolve_precedence(&op.ttype);
        let mut last_op = self.operator_stack.last().unwrap();
        while self.resolve_precedence(&last_op.ttype) >= precedence {
            self.pop_operator(self.resolve_binary_op(op.ttype));
            last_op = self.operator_stack.last().unwrap();
        }
        self.operator_stack.push(op);
    }

    fn pop_operator(&mut self, is_binary: bool) {
        if self.operand_stack.len() >= 2 && is_binary {
            let right = self.operand_stack.pop().unwrap();
            let left = self.operand_stack.pop().unwrap();
            let op = self.operator_stack.pop().unwrap();
            let node = self.parse_binary_op(op.clone(), left, right);
            self.operand_stack.push(node);
        } else if self.operand_stack.len() > 0 {
            let operand_node = self.operand_stack.pop().unwrap();
            let op = self.operator_stack.pop().unwrap();
            let node = self.parse_uniary_op(op.clone(), Box::new(operand_node));
            self.operand_stack.push(node);
        }
    }

    fn create_node(&mut self, op_type: &TType) -> Node {
        let current_token = self.current_token.as_ref().unwrap();
        let lexeme = current_token.lexeme.to_string();
        let ttype = current_token.ttype;
        let offset = current_token.offset;
        match op_type {
            TType::Num => Node::Number(Number {
                token: current_token.clone(),
                lineno: self.lexer.lineno(),
                value: lexeme.parse().unwrap(),
            }),
            TType::Literal => Node::StringLiteral(StringLiteral {
                token: current_token.clone(),
                lineno: self.lexer.lineno(),
                literal: lexeme.parse().unwrap(),
            }),
            TType::Id => {
                self.consume_next_token();
                Node::Ident(Ident {
                    token: Token {
                        lexeme: lexeme.clone(),
                        ttype: ttype.clone(),
                        offset,
                    },
                    lineno: self.lexer.lineno(),
                    value: lexeme.parse().unwrap(),
                })
            }
            TType::Null => Node::Null(Null {
                lineno: self.lexer.lineno(),
                token: current_token.clone(),
            }),
            _ => Node::Error(Error {
                src: self.source(),
                span: (offset as usize, lexeme.len() + offset as usize).into(),
                error_type: ParseError::UndeterminedType(format!(
                    "couldn't parse the terminal type\n {}",
                    current_token.error_fmt(&self.source())
                )),
            }),
        }
    }

    fn parse_uniary_op(&mut self, op_token: Token, right_operand: Box<Node>) -> Node {
        match op_token.ttype {
            TType::Minus | TType::Plus => Node::UniaryOp(UniaryOp {
                lineno: self.lexer.lineno(),
                token: op_token,
                right: right_operand,
            }),
            _ => Node::Error(Error {
                src: self.source(),
                span: (
                    op_token.offset as usize,
                    op_token.lexeme.len() + op_token.offset as usize,
                )
                    .into(),
                error_type: ParseError::UndeterminedType(format!(
                    "couldn't parse uniary operator type \n {}",
                    op_token.error_fmt(&self.source())
                )),
            }),
        }
    }

    fn parse_binary_op(
        &mut self,
        op_token: Token,
        left_operand: Node,
        right_operand: Node,
    ) -> Node {
        match op_token.ttype {
            TType::Plus => Node::BinaryOp(BinaryOp {
                lineno: self.lexer.lineno(),
                token: op_token,
                right: Box::new(right_operand),
                left: Box::new(left_operand),
            }),
            TType::Minus => Node::BinaryOp(BinaryOp {
                lineno: self.lexer.lineno(),
                token: op_token,
                right: Box::new(right_operand),
                left: Box::new(left_operand),
            }),
            // TType::Asterisk => Node::BinaryOp(BinaryOp {
            //     lineno: self.lexer.lineno(),
            //     token: op_token,
            //     right: Box::new(right_operand),
            //     left: Box::new(left_operand),
            // }),
            TType::Div => Node::BinaryOp(BinaryOp {
                lineno: self.lexer.lineno(),
                token: op_token,
                right: Box::new(right_operand),
                left: Box::new(left_operand),
            }),
            _ => Node::Error(Error {
                src: self.source(),
                span: (
                    op_token.offset as usize,
                    op_token.lexeme.len() + op_token.offset as usize,
                )
                    .into(),
                error_type: ParseError::UndeterminedType(format!(
                    "couldn't parse binary operator type \n {}",
                    op_token.error_fmt(&self.source())
                )),
            }),
        }
    }

    fn parse_bool(&mut self) -> Node {
        let current_token = self.current_token.as_ref().unwrap();
        if current_token.ttype == TType::True {
            return Node::Boolean(Boolean {
                lineno: self.lexer.lineno(),
                token: current_token.clone(),
                value: true,
            });
        }
        Node::Boolean(Boolean {
            lineno: self.lexer.lineno(),
            token: current_token.clone(),
            value: false,
        })
    }

    fn parse_init_stmt(&mut self) -> Node {
        todo!()
    }

    fn parse_declare_stmt(&mut self) -> Node {
        todo!()
    }

    fn parse_if_stmt(&mut self) -> Node {
        todo!()
    }

    fn parse_block_stmt(&mut self) -> Result<Node, Error> {
        let lineno = self.lexer.lineno();
        let cur_token = self.current_token.as_ref().unwrap();
        let mut stmts: Vec<Box<Node>> = vec![];

        if cur_token.ttype != TType::Lbrace {
            return Err(Error::new(
                self.source(),
                (
                    cur_token.offset as usize,
                    cur_token.lexeme.len() + cur_token.offset as usize,
                )
                    .into(),
                ParseError::InvalidSyntax("".to_string()),
            ));
        }

        let b_token = self.current_token.clone();
        self.consume_next_token();

        loop {
            let cur_token = self.current_token.as_ref().unwrap();
            let ttype = cur_token.ttype;
            if ttype != TType::Rbrace && ttype != TType::Newline {
                let node = self.parse_stmt(&ttype);
                stmts.push(Box::new(node));
            }

            if ttype == TType::Rbrace {
                break;
            }

            if ttype == TType::Eob {
                break;
            }
            self.consume_next_token();
        }

        Ok(Node::Block(Block {
            lineno,
            token: b_token.unwrap(),
            statements: stmts,
        }))
    }

    fn parse_return_stmt(&mut self) -> Node {
        let current_token = self.current_token.clone();

        let lineno = self.lexer.lineno();
        self.consume_next_token();

        Node::Return(Return {
            lineno,
            token: current_token.unwrap(),
            value: Box::new(self.parse_expression()),
        })
    }

    fn parse_call_expr(&mut self) -> Node {
        todo!()
    }

    fn parse_func_literal(&mut self) -> Node {
        todo!()
    }

    fn parse_range_expr(&mut self) -> Node {
        todo!()
    }

    fn parse_func_smt(&mut self) -> Node {
        todo!()
    }

    fn parse_while_stmt(&mut self) -> Node {
        todo!()
    }

    fn parse_for_stmt(&mut self) -> Node {
        let f_lineno = self.lexer.lineno();
        let f_token = self.current_token.clone();
        let f_id: Result<Node, Error>;
        let f_iter: Result<Node, Error>;
        let f_block: Result<Node, Error>;

        match self.peek_token.clone() {
            Some(token) => {
                if token.ttype != TType::Id {
                    let span: SourceSpan = (
                        token.offset as usize,
                        (token.lexeme.len() + token.offset as usize),
                    )
                        .into();
                    let err = Error::new(
                        self.source(),
                        span,
                        ParseError::InvalidSyntax("".to_string()),
                    );
                    f_id = Err(err)
                } else {
                    self.consume_next_token();
                    f_id = Ok(self.create_node(&token.ttype))
                }
            }
            None => {
                let err = Error::new(
                    self.source(),
                    (0, 0).into(),
                    ParseError::InvalidSyntax("".to_string()),
                );
                f_id = Err(err)
            }
        };

        if f_id.is_err() {
            return Node::Error(f_id.err().unwrap());
        }

        if self.current_token.as_ref().unwrap().ttype != TType::In {
            let current_token = self.current_token.as_ref().unwrap();
            return Node::Error(Error::new(
                self.source(),
                (
                    current_token.offset as usize,
                    current_token.lexeme.len() + current_token.offset as usize,
                )
                    .into(),
                ParseError::InvalidSyntax("".to_string()),
            ));
        }

        self.consume_next_token();

        f_iter = self.parse_for_iter_expr();

        if f_iter.is_err() {
            return Node::Error(f_iter.err().unwrap());
        }

        self.consume_next_token();

        f_block = self.parse_block_stmt();

        if f_block.is_err() {
            return Node::Error(f_block.err().unwrap());
        }

        Node::For(ForStmt {
            lineno: f_lineno,
            target: Box::new(f_id.ok().unwrap()),
            token: f_token.unwrap(),
            iter: Box::new(f_iter.ok().unwrap()),
            block: Box::new(f_block.ok().unwrap()),
        })
    }

    fn parse_for_iter_expr(&mut self) -> Result<Node, Error> {
        let current_token = self.current_token.as_ref().unwrap();
        match current_token.ttype {
            TType::Num => self.parse_num_range(),
            _ => Err(Error::new(
                self.source(),
                (
                    current_token.offset as usize,
                    current_token.lexeme.len() + current_token.offset as usize,
                )
                    .into(),
                ParseError::InvalidSyntax("".to_string()),
            )),
        }
    }

    fn parse_num_range(&mut self) -> Result<Node, Error> {
        let lineno = self.lexer.lineno();
        let cur_token = self.current_token.as_ref().unwrap();
        let ttype = cur_token.ttype;
        let start = self.create_node(&ttype);
        self.consume_next_token();
        let cur_token = self.current_token.as_ref().unwrap();

        if cur_token.ttype != TType::Range {
            return Err(Error::new(
                self.source(),
                (
                    cur_token.offset as usize,
                    cur_token.lexeme.len() + cur_token.offset as usize,
                )
                    .into(),
                ParseError::InvalidSyntax("".to_string()),
            ));
        }

        self.consume_next_token();
        let cur_token = self.current_token.as_ref().unwrap();
        let ttype = cur_token.ttype;

        if cur_token.ttype != TType::Num {
            return Err(Error::new(
                self.source(),
                (
                    cur_token.offset as usize,
                    cur_token.lexeme.len() + cur_token.offset as usize,
                )
                    .into(),
                ParseError::InvalidSyntax("".to_string()),
            ));
        }
        let end = self.create_node(&ttype);
        Ok(Node::Range(Range {
            lineno,
            start: Box::new(start),
            end: Box::new(end),
        }))
    }

    pub fn parse_stmt(&mut self, ttype: &TType) -> Node {
        let node = match ttype {
            TType::If => self.parse_if_stmt(),
            TType::Declare => self.parse_declare_stmt(),
            TType::Define => self.parse_func_smt(),
            TType::Init => self.parse_init_stmt(),
            TType::For => self.parse_for_stmt(),
            TType::Return => self.parse_return_stmt(),
            TType::Lbrace => match self.parse_block_stmt() {
                Ok(node) => node,
                Err(err) => Node::Error(err),
            },
            _ => self.parse_expression(),
        };
        node
    }
    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };
        self.consume_next_token();
        let mut current_token = self.current_token.as_ref().unwrap();
        let mut ttype = current_token.ttype;
        while ttype != TType::Eob {
            if ttype == TType::Newline {
                self.consume_next_token();
                current_token = self.current_token.as_ref().unwrap();
                ttype = current_token.ttype;
                continue;
            }
            let node = self.parse_stmt(&ttype);
            match node {
                Node::Error(err) => self.errors.push(err),
                _ => program.statements.push(node),
            }

            self.consume_next_token();
            current_token = self.current_token.as_ref().unwrap();
            ttype = current_token.ttype;
        }
        program
    }
}
