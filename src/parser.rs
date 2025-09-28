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
            span: (
                self.current_token.as_ref().unwrap().offset as usize,
                self.current_token.as_ref().unwrap().lexeme.len() as usize,
            )
                .into(),
            error_type: ParseError::InvalidSyntax(format!(
                "Invalid expression: {}",
                self.current_token.as_ref().unwrap().lexeme
            )),
        }));
        node
    }

    fn parse_index_expr(&mut self) -> Node {
        let lineno = self.lexer.lineno();
        let mut current_token = self.current_token.clone().unwrap();

        let object = self.create_node(&TType::Id);

        self.consume_next_token();
        current_token = self.current_token.clone().unwrap();

        if current_token.ttype != TType::Lbracket {
            let span = (current_token.offset as usize, current_token.lexeme.len()).into();
            let err = Error::new(
                self.source(),
                span,
                ParseError::InvalidSyntax(format!(
                    "Expected '[' after object, got {}",
                    current_token.lexeme
                )),
            );
            return Node::Error(err);
        }
        self.consume_next_token();

        let index = self.parse_expression();

        self.consume_next_token();
        current_token = self.current_token.clone().unwrap();

        if current_token.ttype != TType::Rbracket {
            let span = (current_token.offset as usize, current_token.lexeme.len()).into();
            let err = Error::new(
                self.source(),
                span,
                ParseError::InvalidSyntax(format!(
                    "Expected ']' after index, got {}",
                    current_token.lexeme
                )),
            );
            return Node::Error(err);
        }

        Node::Index(Index {
            lineno,
            token: current_token.clone(),
            object: Box::new(object),
            index: Box::new(index),
        })
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
            self.push_operator(op_token, true);
            self.consume_next_token();
            self.consume_next_token();
            self.produce();
            ttype = self.peek_token.as_ref().unwrap().ttype;
            look_ahead = self.peek_token.as_ref().unwrap();
        }
        let default_token = &Token {
            ttype: TType::Error,
            lexeme: String::from(""),
            offset: -1,
        };
        let mut op = self.operator_stack.last().unwrap_or(default_token);
        while op.ttype != TType::Error && op.ttype != TType::Null {
            let is_binary = self.resolve_binary_op(op.ttype);
            self.pop_operator(is_binary);
            if self.operator_stack.last().is_some() {
                op = self.operator_stack.last().unwrap();
            } else {
                break;
            }
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
                if self.peek_token.as_ref().unwrap().ttype == TType::Range {
                    let range_node = self.parse_num_range().unwrap();
                    self.operand_stack.pop();
                    self.operand_stack.push(range_node);
                } else {
                    self.operand_stack.push(node);
                }
            }
            TType::Null => {
                let node = self.create_node(&ttype);
                self.operand_stack.push(node);
            }
            TType::Id => {
                // Check if this is a function call
                if self.peek_token.as_ref().unwrap().ttype == TType::Lparen {
                    let call_node = self.parse_call_expr();
                    self.operand_stack.pop();
                    self.operand_stack.push(call_node);
                } else if self.peek_token.as_ref().unwrap().ttype == TType::Lbracket {
                    let index_node = self.parse_index_expr();
                    self.operand_stack.push(index_node);
                } else {
                    let node = self.create_node(&ttype);
                    self.operand_stack.push(node);
                }
            }
            TType::Define => {
                if self.peek_token.as_ref().unwrap().ttype != TType::Lparen {
                    let span = (curr_token.offset as usize, curr_token.lexeme.len()).into();
                    let err = Error::new(
                        self.source(),
                        span,
                        ParseError::InvalidSyntax(format!(
                            "Expected '(' after 'define' keyword in a function expression, got {}",
                            curr_token.lexeme
                        )),
                    );
                    self.errors.push(err);
                    return;
                }

                self.consume_next_token();
                let node = self.parse_func_literal();
                self.operand_stack.push(node);
                if self.peek_token.as_ref().unwrap().ttype == TType::Lparen {
                    self.consume_next_token();
                    let call_node = self.parse_call_expr();
                    self.operand_stack.push(call_node);
                    self.operator_stack.push(Token {
                        lexeme: "()".to_string(),
                        ttype: TType::Call,
                        offset: -1,
                    });
                }
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
            TType::Lbracket => {
                let array_node = self.parse_array_literal();
                self.operand_stack.push(array_node);
            }
            TType::Lparen => {
                self.consume_next_token();
                self.expression();

                if self.peek_token.as_ref().unwrap().ttype != TType::Rparen {
                    let curr_token = self.current_token.as_ref().unwrap();
                    let span = (curr_token.offset as usize, curr_token.lexeme.len()).into();
                    let err = Error::new(
                        self.source(),
                        span,
                        ParseError::InvalidSyntax(format!(
                            "Expected ')' after expression, got {}",
                            curr_token.lexeme
                        )),
                    );
                    self.errors.push(err);
                } else {
                    self.consume_next_token();
                }
            }
            _ => {
                if self.resolve_uniary_op(ttype) {
                    let token = curr_token.clone();
                    self.push_operator(token, false);
                    self.consume_next_token();
                    self.produce();
                    self.pop_operator(false);
                } else {
                    let span = (curr_token.offset as usize, curr_token.lexeme.len()).into();
                    let err = Error::new(
                        self.source(),
                        span,
                        ParseError::InvalidSyntax(format!(
                            "Unexpected token in expression: {}",
                            curr_token.lexeme
                        )),
                    );
                    self.errors.push(err);
                }
            }
        }
    }

    fn resolve_uniary_op(&self, op_type: TType) -> bool {
        match op_type {
            TType::Bang => true,
            TType::Minus => true,
            _ => false,
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
            TType::Call => true,
            TType::Range => true,
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
            TType::Call => Precendence::Call(5),
            TType::Range => Precendence::Range(0),
            _ => Precendence::Base(0),
        }
    }

    fn push_operator(&mut self, op: Token, is_binary: bool) {
        let precedence = self.resolve_precedence(&op.ttype);

        if self.operator_stack.last().is_none() {
            self.operator_stack.push(op);
            return;
        }

        while let Some(last_op) = self.operator_stack.last() {
            if self.resolve_precedence(&last_op.ttype) >= precedence {
                self.pop_operator(is_binary);
            } else {
                break;
            }
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
        } else {
            self.operator_stack.pop();
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
            TType::Id => Node::Ident(Ident {
                token: Token {
                    lexeme: lexeme.clone(),
                    ttype: ttype.clone(),
                    offset,
                },
                lineno: self.lexer.lineno(),
                value: lexeme.parse().unwrap(),
            }),
            TType::Null => Node::Null(Null {
                lineno: self.lexer.lineno(),
                token: current_token.clone(),
            }),
            _ => Node::Error(Error {
                src: self.source(),
                span: (offset as usize, lexeme.len()).into(),
                error_type: ParseError::UndeterminedType(format!(
                    "couldn't parse the terminal type\n {}",
                    current_token.error_fmt(&self.source())
                )),
            }),
        }
    }

    fn parse_uniary_op(&mut self, op_token: Token, right_operand: Box<Node>) -> Node {
        match op_token.ttype {
            TType::Minus | TType::Plus | TType::Bang => Node::UniaryOp(UniaryOp {
                lineno: self.lexer.lineno(),
                token: op_token,
                right: right_operand,
            }),
            _ => Node::Error(Error {
                src: self.source(),
                span: (op_token.offset as usize, op_token.lexeme.len()).into(),
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
            TType::Plus
            | TType::Minus
            | TType::Asterisk
            | TType::Div
            | TType::Lt
            | TType::LtEq
            | TType::Gt
            | TType::GtEq
            | TType::Eq
            | TType::NotEq
            | TType::Assign
            | TType::Range => Node::BinaryOp(BinaryOp {
                lineno: self.lexer.lineno(),
                token: op_token,
                right: Box::new(right_operand),
                left: Box::new(left_operand),
            }),
            TType::Call => Node::Call(Call {
                lineno: self.lexer.lineno(),
                token: op_token,
                func: Some(Box::new(left_operand)),
                args: match right_operand {
                    Node::Call(call) => call.args,
                    _ => vec![],
                },
            }),
            _ => Node::Error(Error {
                src: self.source(),
                span: (op_token.offset as usize, op_token.lexeme.len() as usize).into(),
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
        let lineno = self.lexer.lineno();
        let init_token = self.current_token.clone().unwrap();

        self.consume_next_token();

        let current_token = self.current_token.as_ref().unwrap();
        if current_token.ttype != TType::Id {
            let span = (current_token.offset as usize, current_token.lexeme.len()).into();
            let err = Error::new(
                self.source(),
                span,
                ParseError::MissingIdent(format!(
                    "Expected identifier after 'init' keyword, got {}",
                    current_token.lexeme
                )),
            );
            return Node::Error(err);
        }

        let ttype = current_token.ttype;
        let ident = self.create_node(&ttype);
        self.consume_next_token();

        let current_token = self.current_token.as_ref().unwrap();
        if current_token.ttype != TType::Assign {
            let span = (current_token.offset as usize, current_token.lexeme.len()).into();
            let err = Error::new(
                self.source(),
                span,
                ParseError::InvalidSyntax(format!(
                    "Expected '=' after identifier, got {}",
                    current_token.lexeme
                )),
            );
            return Node::Error(err);
        }

        self.consume_next_token();
        let value = self.parse_expression();

        Node::Init(Init {
            lineno,
            token: init_token,
            ident: Box::new(ident),
            value: Box::new(value),
        })
    }

    fn parse_declare_stmt(&mut self) -> Node {
        let lineno = self.lexer.lineno();
        let declare_token = self.current_token.clone().unwrap();
        let mut idents = Vec::new();

        self.consume_next_token();

        loop {
            let current_token = self.current_token.as_ref().unwrap();

            if current_token.ttype != TType::Id {
                let span = (current_token.offset as usize, current_token.lexeme.len()).into();
                let err = Error::new(
                    self.source(),
                    span,
                    ParseError::MissingIdent(format!(
                        "Expected identifier in declare statement, got {}",
                        current_token.lexeme
                    )),
                );
                return Node::Error(err);
            }

            let ttype = current_token.ttype;
            idents.push(self.create_node(&ttype));
            self.consume_next_token();

            let current_token = self.current_token.as_ref().unwrap();
            match current_token.ttype {
                TType::Comma => {
                    self.consume_next_token();
                    continue;
                }
                TType::Newline | TType::Eob => break,
                _ => {
                    let span = (current_token.offset as usize, current_token.lexeme.len()).into();
                    let err = Error::new(
                        self.source(),
                        span,
                        ParseError::InvalidSyntax(format!(
                            "Expected ',' or newline in declare statement, got {}",
                            current_token.lexeme
                        )),
                    );
                    return Node::Error(err);
                }
            }
        }

        Node::Declare(Declare {
            lineno,
            token: declare_token,
            idents,
        })
    }

    fn parse_if_stmt(&mut self) -> Node {
        let lineno = self.lexer.lineno();
        let if_token = self.current_token.clone().unwrap();

        // Move to predicate start
        self.consume_next_token();

        // Parse predicate expression (optionally wrapped in parentheses)
        let predicate = self.parse_expression();

        // Consume optional ')'
        let mut current_token = self.current_token.as_ref().unwrap().clone();
        if current_token.ttype == TType::Rparen {
            self.consume_next_token();
            current_token = self.current_token.as_ref().unwrap().clone();
        }

        // Expect and parse block
        let block = match current_token.ttype {
            TType::Lbrace => match self.parse_block_stmt() {
                Ok(node) => node,
                Err(err) => return Node::Error(err),
            },
            _ => {
                let span = (current_token.offset as usize, current_token.lexeme.len()).into();
                let err = Error::new(
                    self.source(),
                    span,
                    ParseError::InvalidSyntax(format!(
                        "Expected '{{' to start block after if condition, got '{}'",
                        current_token.lexeme
                    )),
                );
                return Node::Error(err);
            }
        };

        // Prepare else-if and else parsing
        let mut else_if_nodes: Vec<Box<Node>> = Vec::new();
        let mut else_block: Option<Box<Node>> = None;

        // Skip optional newlines between '}' and 'else'
        let mut next_tt = self.peek_token.as_ref().unwrap().ttype;
        while next_tt == TType::Newline {
            self.consume_next_token();
            next_tt = self.peek_token.as_ref().unwrap().ttype;
        }

        // Handle a single trailing 'else' which can be either 'else if ...' (nested if)
        // or an 'else { ... }' block. If it's an 'else if', we parse it recursively and
        // stop, letting the nested if handle any further chaining.
        if next_tt == TType::Else {
            // consume 'else'
            self.consume_next_token();
            // advance to next significant token
            self.consume_next_token();

            let current_token = self.current_token.as_ref().unwrap().clone();
            if current_token.ttype == TType::If {
                let nested_if = self.parse_if_stmt();
                else_if_nodes.push(Box::new(nested_if));
            } else {
                match current_token.ttype {
                    TType::Lbrace => match self.parse_block_stmt() {
                        Ok(node) => else_block = Some(Box::new(node)),
                        Err(err) => return Node::Error(err),
                    },
                    _ => {
                        let span =
                            (current_token.offset as usize, current_token.lexeme.len()).into();
                        let err = Error::new(
                            self.source(),
                            span,
                            ParseError::InvalidSyntax(format!(
                                "Expected '{{' to start else block, got '{}'",
                                current_token.lexeme
                            )),
                        );
                        return Node::Error(err);
                    }
                }
            }
        }

        Node::If(If {
            lineno,
            token: if_token,
            predicate: Box::new(predicate),
            else_if: else_if_nodes,
            else_block,
            block: Box::new(block),
        })
    }

    fn parse_block_stmt(&mut self) -> Result<Node, Error> {
        let lineno = self.lexer.lineno();
        let cur_token = self.current_token.as_ref().unwrap();
        let mut stmts: Vec<Box<Node>> = vec![];

        if cur_token.ttype != TType::Lbrace {
            return Err(Error::new(
                self.source(),
                (cur_token.offset as usize, cur_token.lexeme.len()).into(),
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
        let lineno = self.lexer.lineno();
        let mut current_token = self.current_token.as_ref().unwrap();

        // First token should be an identifier (function name)
        // if not assume it's an anonymous function call
        let func_is_named = current_token.ttype == TType::Id
            && self.peek_token.as_ref().unwrap().ttype == TType::Lparen;

        let ttype = current_token.ttype;
        let mut func_name: Option<Node> = None;
        if func_is_named {
            func_name = Some(self.create_node(&ttype));
            self.consume_next_token();

            // Next token should be '('
            current_token = self.current_token.as_ref().unwrap();
            if current_token.ttype != TType::Lparen {
                let span = (
                    current_token.offset as usize,
                    current_token.lexeme.len() as usize,
                )
                    .into();
                let err = Error::new(
                    self.source(),
                    span,
                    ParseError::InvalidSyntax(format!(
                        "Expected '(' after function name, got {}",
                        current_token.lexeme
                    )),
                );
                return Node::Error(err);
            }
        }
        self.consume_next_token();

        let mut args = Vec::new();

        // Parse argument list
        loop {
            current_token = self.current_token.as_ref().unwrap();

            if current_token.ttype == TType::Rparen {
                break;
            }

            if !args.is_empty() {
                if current_token.ttype != TType::Comma {
                    let span = (
                        current_token.offset as usize,
                        current_token.lexeme.len() as usize,
                    )
                        .into();
                    let err = Error::new(
                        self.source(),
                        span,
                        ParseError::InvalidSyntax(format!(
                            "Expected ',' between arguments, got {}",
                            current_token.lexeme
                        )),
                    );
                    return Node::Error(err);
                }
                self.consume_next_token();
            }

            args.push(self.parse_expression());
            self.consume_next_token();
        }

        match func_name {
            None => Node::Call(Call {
                lineno,
                token: current_token.clone(),
                func: None,
                args,
            }),
            Some(fname) => Node::Call(Call {
                lineno,
                token: current_token.clone(),
                func: Some(Box::new(fname)),
                args,
            }),
        }
    }

    fn parse_func_literal(&mut self) -> Node {
        let lineno = self.lexer.lineno();
        let current_token = self.current_token.as_ref().unwrap().clone();

        // Next token should be '('
        if current_token.ttype != TType::Lparen {
            let span = (
                current_token.offset as usize,
                current_token.lexeme.len() as usize,
            )
                .into();
            let err = Error::new(
                self.source(),
                span,
                ParseError::InvalidSyntax(format!(
                    "Expected '(' after function name, got {}",
                    current_token.lexeme
                )),
            );
            return Node::Error(err);
        }

        self.consume_next_token();
        let mut params = Vec::new();

        // Parse parameter list
        loop {
            let current_token = self.current_token.as_ref().unwrap().clone();

            if current_token.ttype == TType::Rparen {
                break;
            }

            if !params.is_empty() {
                if current_token.ttype != TType::Comma {
                    let span = (
                        current_token.offset as usize,
                        current_token.lexeme.len() as usize,
                    )
                        .into();
                    let err = Error::new(
                        self.source(),
                        span,
                        ParseError::InvalidSyntax(format!(
                            "Expected ',' between parameters, got {}",
                            current_token.lexeme
                        )),
                    );
                    return Node::Error(err);
                }
                self.consume_next_token();
            }

            let current_token = self.current_token.as_ref().unwrap().clone();
            if current_token.ttype != TType::Id {
                let span = (
                    current_token.offset as usize,
                    current_token.lexeme.len() as usize,
                )
                    .into();
                let err = Error::new(
                    self.source(),
                    span,
                    ParseError::MissingIdent(format!(
                        "Expected parameter name, got {}",
                        current_token.lexeme
                    )),
                );
                return Node::Error(err);
            }

            let ttype = current_token.ttype;
            params.push(self.create_node(&ttype));
            self.consume_next_token();
        }

        self.consume_next_token(); // Consume ')'

        // Parse function body
        let current_token = self.current_token.as_ref().unwrap().clone();
        let body = match current_token.ttype {
            TType::Lbrace => match self.parse_block_stmt() {
                Ok(node) => node,
                Err(err) => return Node::Error(err),
            },
            _ => {
                let span = (
                    current_token.offset as usize,
                    current_token.lexeme.len() as usize,
                )
                    .into();
                let err = Error::new(
                    self.source(),
                    span,
                    ParseError::InvalidSyntax(format!(
                        "Expected '{{' for function body, got {}",
                        current_token.lexeme
                    )),
                );
                return Node::Error(err);
            }
        };

        Node::Function(Function {
            lineno,
            token: current_token,
            params,
            body: Box::new(body),
        })
    }

    fn parse_array_literal(&mut self) -> Node {
        let lineno = self.lexer.lineno();
        let mut elements = Vec::new();

        self.consume_next_token();

        loop {
            let current_token = self.current_token.as_ref().unwrap();

            if current_token.ttype == TType::Rbracket {
                break;
            }

            if !elements.is_empty() {
                if current_token.ttype != TType::Comma {
                    let span = (
                        current_token.offset as usize,
                        current_token.lexeme.len() as usize,
                    )
                        .into();
                    let err = Error::new(
                        self.source(),
                        span,
                        ParseError::InvalidSyntax(format!(
                            "Expected ',' between array elements, got {}",
                            current_token.lexeme
                        )),
                    );
                    return Node::Error(err);
                }
                self.consume_next_token();
            }

            elements.push(self.parse_expression());
            self.consume_next_token();
        }

        self.consume_next_token();

        Node::Array(Array { lineno, elements })
    }

    fn parse_func_smt(&mut self) -> Node {
        let lineno = self.lexer.lineno();
        let define_token = self.current_token.clone().unwrap();

        // Consume 'define' token
        self.consume_next_token();

        // Next token should be an identifier (function name)
        let current_token = self.current_token.as_ref().unwrap().clone();
        if current_token.ttype != TType::Id {
            let span = (
                current_token.offset as usize,
                current_token.lexeme.len() as usize,
            )
                .into();
            let err = Error::new(
                self.source(),
                span,
                ParseError::MissingIdent(format!(
                    "Expected function name after 'define', got {}",
                    current_token.lexeme
                )),
            );
            return Node::Error(err);
        }

        let ttype = current_token.ttype;
        let func_name = self.create_node(&ttype);
        self.consume_next_token();

        let func_literal = self.parse_func_literal();

        Node::FunctionExpr(FunctionExpr {
            lineno,
            token: define_token,
            name: Box::new(func_name),
            func: Box::new(func_literal),
        })
    }

    fn parse_while_stmt(&mut self) -> Node {
        let lineno = self.lexer.lineno();
        let while_token = self.current_token.clone().unwrap();

        self.consume_next_token();

        let predicate = self.parse_expression();

        let mut current_token = self.current_token.as_ref().unwrap().clone();

        if current_token.ttype == TType::Rparen {
            self.consume_next_token();
            current_token = self.current_token.as_ref().unwrap().clone();
        }

        let block = match current_token.ttype {
            TType::Lbrace => match self.parse_block_stmt() {
                Ok(node) => node,
                Err(err) => return Node::Error(err),
            },
            _ => {
                let span = (
                    current_token.offset as usize,
                    current_token.lexeme.len() as usize,
                )
                    .into();
                let err = Error::new(
                    self.source(),
                    span,
                    ParseError::InvalidSyntax(format!(
                        "Expected '{{' after while condition, got {}",
                        current_token.lexeme
                    )),
                );
                return Node::Error(err);
            }
        };

        Node::While(While {
            lineno,
            token: while_token,
            predicate: Box::new(predicate),
            block: Box::new(block),
        })
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
                    let span: SourceSpan =
                        (token.offset as usize, token.lexeme.len() as usize).into();
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

        self.consume_next_token();

        if self.current_token.as_ref().unwrap().ttype != TType::In {
            let current_token = self.current_token.as_ref().unwrap();
            return Node::Error(Error::new(
                self.source(),
                (
                    current_token.offset as usize,
                    current_token.lexeme.len() as usize,
                )
                    .into(),
                ParseError::InvalidSyntax("".to_string()),
            ));
        }

        self.consume_next_token();

        let iter_node = self.parse_expression();

        f_iter = match iter_node {
            Node::Error(err) => Err(err),
            _ => Ok(iter_node),
        };

        if f_iter.is_err() {
            return Node::Error(f_iter.err().unwrap());
        }

        if self
            .current_token
            .as_ref()
            .unwrap_or(&Token::from("", TType::Eob, 0))
            .ttype
            != TType::Lbrace
        {
            self.consume_next_token();
        }

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
                (cur_token.offset as usize, cur_token.lexeme.len() as usize).into(),
                ParseError::InvalidSyntax("".to_string()),
            ));
        }

        self.consume_next_token();
        let cur_token = self.current_token.as_ref().unwrap();
        let ttype = cur_token.ttype;

        if cur_token.ttype != TType::Num {
            return Err(Error::new(
                self.source(),
                (cur_token.offset as usize, cur_token.lexeme.len() as usize).into(),
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
            TType::While => self.parse_while_stmt(),
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
            // println!("pushing node: {:#?}", node);
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
