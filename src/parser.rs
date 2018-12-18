use super::syntax::*;

pub struct Parser<'a> {
    pub(crate) input: &'a str,
    source: Source,
    pub(crate) position: Position,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str, source: Source) -> Parser<'a> {
        Parser {
            input,
            source,
            position: Position {
                offset: 0,
                line: 0,
                col: 0,
            },
        }
    }

    fn meta(&self) -> Meta {
        Meta {
            start: self.position,
            source: self.source.clone(),
        }
    }

    /// Returns true if there are no non-whitespace tokens up until the end of the input (comments
    /// are considered whitespace).
    pub fn end(&mut self) -> bool {
        self.skip_ws();
        self.input.len() == 0
    }

    /// Reads the the next char.
    fn next(&mut self) -> Option<char> {
        if let Some(c) = self.input.chars().next() {
            if c == '\n' {
                self.position.line += 1;
                self.position.col = 0;
            } else {
                self.position.col += 1;
            }

            let utf8_len = c.len_utf8();
            self.position.offset += utf8_len;
            self.input = &self.input[utf8_len..];

            return Some(c);
        }

        None
    }

    /// Skip one char ahead.
    fn skip(&mut self) {
        let _ = self.next();
    }

    /// Skip while the predicate returns true.
    fn skip_while<P: Fn(char) -> bool>(&mut self, p: P) {
        while let Some(c) = self.peek() {
            if !p(c) {
                return;
            } else {
                self.skip();
            }
        }
    }

    // If the input begins with the given str, skip over it and return true. Else, do not
    // advance the input and return false.
    fn skip_str(&mut self, expected: &str) -> bool {
        if self.input.starts_with(expected) {
            for _ in 0..expected.chars().count() {
                self.skip();
            }
            true
        } else {
            false
        }
    }

    // Reads the next char, return whether it matched. Returns false on end of input.
    fn expect(&mut self, expected: char) -> bool {
        self.next().map(|c| c == expected).unwrap_or(false)
    }

    // Returns the next byte without consuming it.
    fn peek(&self) -> Option<char> {
        self.input.chars().next()
    }

    fn skip_ws(&mut self) {
        loop {
            match self.peek() {
                Some('#') => {
                    self.skip();
                    loop {
                        match self.next() {
                            Some('\n') | None => break,
                            _ => {}
                        }
                    }
                }
                Some(' ') | Some('\n') => self.skip(),
                _ => return,
            }
        }
    }

    // Returns true if the input begins with the string, followed by a non-identifier char.
    // Advances the input beyond the string.
    fn expect_kw(&mut self, kw: &str) -> bool {
        if self.peek_kw(kw) {
            self.input = &self.input[kw.len()..];
            true
        } else {
            false
        }
    }

    // Returns true if the input begins with the string, followed by a non-identifier char.
    fn peek_kw(&mut self, kw: &str) -> bool {
        self.input.starts_with(kw) && (self.input.len() == kw.len() || !is_ident_byte(self.input.as_bytes()[kw.len()]))
    }

    // Checks whether the input starts with a (well-known) keyword.
    fn starts_with_a_kw(&mut self) -> bool {
        // TODO update as keywords are added
        if self.input.starts_with("nil") {
            return self.input.len() == 3 || !is_ident_byte(self.input.as_bytes()[3]);
        } else if self.input.starts_with("true") {
            return self.input.len() == 4 || !is_ident_byte(self.input.as_bytes()[4]);
        } else if self.input.starts_with("false") {
            return self.input.len() == 5 || !is_ident_byte(self.input.as_bytes()[5]);
        } else {
            false
        }
    }
}

fn is_ident_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == 0x5F // '_'
}

impl<'a> Parser<'a> {
    fn p_nil(&mut self) -> Option<((), Meta)> {
        let meta = self.meta();
        if self.expect_kw("nil") {
            Some(((), meta))
        } else {
            None
        }
    }

    fn p_bool(&mut self) -> Option<(bool, Meta)> {
        let meta = self.meta();
        match self.peek() {
            None => None,

            Some('t') => if self.expect_kw("true") {
                Some((true, meta))
            } else {
                None
            }

            Some(_) => if self.expect_kw("false") {
                Some((false, meta))
            } else {
                None
            }
        }
    }

    // Non-leftrecursive expressions
    fn p_lexp(&mut self) -> Result<Expression, ParseExpressionError> {
        self.skip_ws();

        match self.peek() {
            None => Err(ParseExpressionError::Empty),
            Some(c) if c.is_ascii_alphabetic() => {
                if self.starts_with_a_kw() {
                    if self.peek_kw("nil") {
                        let meta = self.p_nil().unwrap().1;
                        Ok(Expression(_Expression::Nil, meta))
                    } else if self.peek_kw("true") || self.peek_kw("false") {
                        let (b, meta) = self.p_bool().unwrap();
                        Ok(Expression(_Expression::Bool(b), meta))
                    } else {
                        Err(ParseExpressionError::NonExpressionKw)
                    }
                } else {
                    unimplemented!()
                    // Ok(self.p_id()?.into())
                }
            },
            Some(_) => Err(ParseExpressionError::Leading),
        }
    }

    pub fn p_exp(&mut self) -> Result<Expression, ParseExpressionError> {
        let mut left = self.p_lexp()?;
        self.skip_ws();

        match self.peek() {
            None => Ok(left),
            Some(_) => Ok(left),
        }
    }

    fn p_statement(&mut self) -> Result<Statement, ParseStatementError> {
        self.skip_ws();

        match self.peek() {
            None => Err(ParseStatementError::Empty),
            Some(_) => {
                let exp = self.p_exp()?;
                let meta = exp.1.clone();
                Ok(Statement(_Statement::Exp(exp), meta))
            },
        }
    }

    // just semicolon-separated statements, no braces (use p_block for those)
    pub fn p_program(&mut self) -> Result<Box<[Statement]>, ParseStatementError> {
        let mut stmts = Vec::new();

        loop {
            stmts.push(self.p_statement()?);
            self.skip_ws();
            match self.peek() {
                Some(';') => self.skip(),
                _ => return Ok(stmts.into_boxed_slice()),
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseExpressionError {
    #[fail(display = "expected expression, got end of input")]
    Empty,
    #[fail(display = "expected expression, got invalid first character")]
    Leading,
    #[fail(display = "got trailing non-whitespace character after an expression")]
    Trailing,
    #[fail(display = "expected expression, got a keyword that does not start an expression")]
    NonExpressionKw,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseStatementError {
    #[fail(display = "expected statement, got end of input")]
    Empty,
    #[fail(display = "erroneous expression in statement")]
    Exp(#[fail(cause)]ParseExpressionError),
}

impl From<ParseExpressionError> for ParseStatementError {
    fn from(err: ParseExpressionError) -> ParseStatementError {
        ParseStatementError::Exp(err)
    }
}
