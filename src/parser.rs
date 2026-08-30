/*

Parser module: Houses implementation for beesafe language parser

*/

use miette::SourceSpan;
use thiserror::Error;

use crate::ast::*;
use crate::lexer::{Lexer, TType, Token};
use crate::source::{SourceId, SourceMap};

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    source: SourceId,
    current_token: Option<Token>,
    peek_token: Option<Token>,
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
    pub fn new(lexer: &'a mut Lexer, source: SourceId) -> Self {
        Parser {
            lexer,
            source,
            current_token: None,
            peek_token: None,
            errors: vec![],
        }
    }

    pub fn source(&self) -> SourceId {
        self.source
    }

    pub fn has_errors(&self) -> bool {
        self.errors.is_empty() == false
    }

    pub fn show_errors(&self, sources: &SourceMap) {
        for err in self.errors.clone().into_iter() {
            let text = sources.text(err.src).to_string();
            eprint!("{:?}", miette::Report::new(err).with_source_code(text));
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

    // Pratt parser. On entry current_token is the expression's first token; on
    // exit it is the last.
    fn parse_expression(&mut self) -> Node {
        self.parse_expr_bp(0)
    }

    // Left binding power, right binding power. Higher binds tighter.
    // Left-associative operators use (n, n + 1), right-associative (n + 1, n).
    fn infix_bp(op_type: TType) -> Option<(u8, u8)> {
        Some(match op_type {
            TType::Assign => (2, 1),
            TType::Range => (3, 4),
            TType::Eq | TType::NotEq => (5, 6),
            TType::Lt | TType::LtEq | TType::Gt | TType::GtEq => (7, 8),
            TType::Plus | TType::Minus => (9, 10),
            TType::Asterisk | TType::Div => (11, 12),
            _ => return None,
        })
    }

    const PREFIX_BP: u8 = 13;

    fn parse_expr_bp(&mut self, min_bp: u8) -> Node {
        let mut left = self.parse_prefix();
        // An expression over a bad operand is itself bad. Propagate the error
        // instead of wrapping it in a node the executor cannot evaluate.
        if matches!(left, Node::Error(_)) {
            return left;
        }

        while let Some(op) = self.peek_token.clone() {
            let (left_bp, right_bp) = match Self::infix_bp(op.ttype) {
                Some(bp) => bp,
                None => break,
            };
            if left_bp < min_bp {
                break;
            }
            self.consume_next_token(); // current: the operator
            self.consume_next_token(); // current: first token of the right operand
            let right = self.parse_expr_bp(right_bp);
            if matches!(right, Node::Error(_)) {
                return right;
            }
            left = self.make_infix(op, left, right);
        }
        left
    }

    fn make_infix(&mut self, op: Token, left: Node, right: Node) -> Node {
        if op.ttype == TType::Range {
            return Node::Range(Range {
                lineno: self.lexer.lineno(),
                start: Box::new(left),
                end: Box::new(right),
            });
        }
        self.parse_binary_op(op, left, right)
    }

    fn parse_prefix(&mut self) -> Node {
        let token = self.current_token.clone().unwrap();
        let ttype = token.ttype;
        match ttype {
            TType::Minus | TType::Bang => {
                self.consume_next_token();
                let operand = self.parse_expr_bp(Self::PREFIX_BP);
                if matches!(operand, Node::Error(_)) {
                    return operand;
                }
                self.parse_uniary_op(token, Box::new(operand))
            }
            TType::Num | TType::Null | TType::Literal => self.create_node(&ttype),
            TType::True | TType::False => self.parse_bool(),
            TType::Id => match self.peek_token.as_ref().unwrap().ttype {
                TType::Lparen => self.parse_call_expr(),
                TType::Lbracket => self.parse_index_expr(),
                _ => self.create_node(&ttype),
            },
            TType::Lbracket => self.parse_array_literal(),
            TType::Define => self.parse_func_expr(token),
            TType::Lparen => {
                self.consume_next_token();
                let inner = self.parse_expr_bp(0);
                if self.peek_token.as_ref().unwrap().ttype != TType::Rparen {
                    let curr = self.current_token.as_ref().unwrap();
                    let span = (curr.offset as usize, curr.lexeme.len()).into();
                    self.errors.push(Error::new(
                        self.source(),
                        span,
                        ParseError::InvalidSyntax(format!(
                            "Expected ')' after expression, got {}",
                            curr.lexeme
                        )),
                    ));
                } else {
                    self.consume_next_token();
                }
                inner
            }
            _ => {
                let span = (token.offset as usize, token.lexeme.len()).into();
                let err = Error::new(
                    self.source(),
                    span,
                    ParseError::InvalidSyntax(format!(
                        "Unexpected token in expression: {}",
                        token.lexeme
                    )),
                );
                self.errors.push(err.clone());
                Node::Error(err)
            }
        }
    }

    // `define(params){ body }`, optionally invoked straight away: `define(){ ... }()`
    fn parse_func_expr(&mut self, define_token: Token) -> Node {
        if self.peek_token.as_ref().unwrap().ttype != TType::Lparen {
            let span = (define_token.offset as usize, define_token.lexeme.len()).into();
            let err = Error::new(
                self.source(),
                span,
                ParseError::InvalidSyntax(format!(
                    "Expected '(' after 'define' keyword in a function expression, got {}",
                    define_token.lexeme
                )),
            );
            self.errors.push(err.clone());
            return Node::Error(err);
        }

        self.consume_next_token();
        let func = self.parse_func_literal();

        if self.peek_token.as_ref().unwrap().ttype != TType::Lparen {
            return func;
        }

        self.consume_next_token();
        let args = match self.parse_call_expr() {
            Node::Call(call) => call.args,
            err => return err,
        };
        Node::Call(Call {
            lineno: self.lexer.lineno(),
            token: self.current_token.clone().unwrap(),
            func: Some(Box::new(func)),
            args,
        })
    }

    fn parse_index_expr(&mut self) -> Node {
        let lineno = self.lexer.lineno();

        let object = self.create_node(&TType::Id);

        self.consume_next_token();

        let mut current_token = self.current_token.clone().unwrap();

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
                error_type: ParseError::UndeterminedType(
                    "couldn't parse the terminal type".to_string(),
                ),
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
                error_type: ParseError::UndeterminedType(
                    "couldn't parse uniary operator type".to_string(),
                ),
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
                span: (op_token.offset as usize, op_token.lexeme.len()).into(),
                error_type: ParseError::UndeterminedType(
                    "couldn't parse binary operator type".to_string(),
                ),
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
                let span = (current_token.offset as usize, current_token.lexeme.len()).into();
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
                    let span = (current_token.offset as usize, current_token.lexeme.len()).into();
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
            let span = (current_token.offset as usize, current_token.lexeme.len()).into();
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
                    let span = (current_token.offset as usize, current_token.lexeme.len()).into();
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
                let span = (current_token.offset as usize, current_token.lexeme.len()).into();
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
                let span = (current_token.offset as usize, current_token.lexeme.len()).into();
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
                    let span = (current_token.offset as usize, current_token.lexeme.len()).into();
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

        // The loop stops on `]`. An expression parser leaves current_token on the
        // expression's last token, not past it.
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
            let span = (current_token.offset as usize, current_token.lexeme.len()).into();
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
                let span = (current_token.offset as usize, current_token.lexeme.len()).into();
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
                    let span: SourceSpan = (token.offset as usize, token.lexeme.len()).into();
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
                (current_token.offset as usize, current_token.lexeme.len()).into(),
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
