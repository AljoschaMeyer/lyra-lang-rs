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

    /// Consume as much whitespace (including comments) as possible.
    pub fn skip_ws(&mut self) {
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
        // TODO update as keywords are implemented
        if self.input.starts_with("if") {
            return self.input.len() == 2 || !is_ident_byte(self.input.as_bytes()[2]);
        } else if self.input.starts_with("nil")
        || self.input.starts_with("mut")
        || self.input.starts_with("let") {
            return self.input.len() == 3 || !is_ident_byte(self.input.as_bytes()[3]);
        } else if self.input.starts_with("true") {
            return self.input.len() == 4 || !is_ident_byte(self.input.as_bytes()[4]);
        } else if self.input.starts_with("false")
        || self.input.starts_with("throw") {
            return self.input.len() == 5 || !is_ident_byte(self.input.as_bytes()[5]);
        } else if self.input.starts_with("return") {
            return self.input.len() == 6 || !is_ident_byte(self.input.as_bytes()[6]);
        } else {
            false
        }
    }
}

fn is_ident_byte(b: u8) -> bool {
    b.is_ascii_alphanumeric() || b == 0x5F // '_'
}

impl<'a> Parser<'a> {
    fn p_id(&mut self) -> Result<(String, Meta), ParseIdError> {
        let meta = self.meta();
        if self.starts_with_a_kw() {
            return Err(ParseIdError::Kw);
        }

        let start = self.input;

        match self.next() {
            None => return Err(ParseIdError::Empty),
            Some(c) if c.is_ascii_alphabetic() => {}
            Some(_) => return Err(ParseIdError::Leading),
        }

        self.skip_while(|c| c.is_ascii_alphanumeric() || c == '_');
        
        let id = start[..start.len() - self.input.len()].to_string();
        return Ok((id, meta));
    }

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
    
    fn p_if(&mut self) -> Result<Expression, ParseError> {
        let meta = self.meta();
        if self.peek_kw("if") { // TODO abstract this into a method
            self.skip_str("if");
        } else {
            return Err(ParseIfError::NoIfKw.into());
        }
        
        self.skip_ws();
        let cond = Box::new(self.p_exp()?);
        
        self.skip_ws();
        if !self.expect('{') {
            return Err(ParseIfError::NoLBraceThen.into());
        }
        
        self.skip_ws();        
        let then = self.p_statements()?;
        
        self.skip_ws();        
        if !self.expect('}') {
            return Err(ParseIfError::NoRBraceThen.into());
        }
        
        self.skip_ws();
        
        if self.peek_kw("else") {// TODO abstract this into a method
            self.skip_str("else");
            
            self.skip_ws();
            if !self.expect('{') {
                return Err(ParseIfError::NoLBraceElse.into());
            }
            
            self.skip_ws();        
            let else_ = self.p_statements()?;
            
            self.skip_ws();        
            if !self.expect('}') {
                return Err(ParseIfError::NoRBraceElse.into());
            }
            
            return Ok(Expression(_Expression::If(cond, then, Some(else_)), meta));
        } else {
            return Ok(Expression(_Expression::If(cond, then, None), meta));
        }
    }

    // Non-leftrecursive expressions
    fn p_lexp(&mut self) -> Result<Expression, ParseError> {
        match self.peek() {
            None => Err(ParseError::EmptyExp),
            Some(c) if c.is_ascii_alphabetic() => {
                if self.starts_with_a_kw() {
                    if self.peek_kw("nil") {
                        let meta = self.p_nil().unwrap().1;
                        Ok(Expression(_Expression::Nil, meta))
                    } else if self.peek_kw("true") || self.peek_kw("false") {
                        let (b, meta) = self.p_bool().unwrap();
                        Ok(Expression(_Expression::Bool(b), meta))
                    } else if self.peek_kw("if") {
                        self.p_if()
                    } else {
                        Err(ParseError::KwExp)
                    }
                } else {
                    let (id, meta) = self.p_id()?;
                    Ok(Expression(_Expression::Id(id), meta))
                }
            },
            Some('(') => {
                self.skip();
                self.skip_ws();
                
                let exp = self.p_exp()?;
                
                self.skip_ws();
                if self.expect(')') {
                    return Ok(exp);
                } else {
                    Err(ParseError::RParen)
                }
            }
            Some(_) => Err(ParseError::LeadingExp),
        }
    }

    pub fn p_exp(&mut self) -> Result<Expression, ParseError> {
        let left = self.p_lexp()?;
        let meta = left.1.clone();
        self.skip_ws();
        
        if self.skip_str("&&") {
            self.skip_ws();
            let rhs = Box::new(self.p_exp()?);
            Ok(Expression(_Expression::Land(Box::new(left), rhs), meta))
        } else if self.skip_str("||") {
            self.skip_ws();
            let rhs = Box::new(self.p_exp()?);
            Ok(Expression(_Expression::Lor(Box::new(left), rhs), meta))
        } else {
            Ok(left)
        }
    }
    
    pub fn p_pattern(&mut self) -> Result<Pattern, ParsePatternError> {
        match self.peek() {
            None => Err(ParsePatternError::Empty),
            Some('_') => {
                match self.input.as_bytes().get(1) {
                    Some(b) if is_ident_byte(*b) => {
                        let (id, meta) = self.p_id().unwrap();
                        Ok(Pattern(_Pattern::Id { id, mutable: false }, meta))
                    }
                    _ => {
                        let meta = self.meta();
                        self.skip();
                        Ok(Pattern(_Pattern::Blank, meta))
                    }
                } 
            }
            Some(c) if c.is_ascii_alphabetic() => {
                if self.starts_with_a_kw() {
                    if self.peek_kw("nil") {
                        let meta = self.p_nil().unwrap().1;
                        Ok(Pattern(_Pattern::Nil, meta))
                    } else if self.peek_kw("true") || self.peek_kw("false") {
                        let (b, meta) = self.p_bool().unwrap();
                        Ok(Pattern(_Pattern::Bool(b), meta))
                    } else if self.peek_kw("mut") {
                        self.skip_str("mut");
                        self.skip_ws();
                        let (id, meta) = self.p_id()?; // meta points to the identifier, not the mut keyword
                        Ok(Pattern(_Pattern::Id { id, mutable: true }, meta))
                    } else {
                        Err(ParsePatternError::NonPatternKw)
                    }
                } else {
                    let (id, meta) = self.p_id()?;
                    Ok(Pattern(_Pattern::Id { id, mutable: false }, meta))
                }
            },
            Some(_) => Err(ParsePatternError::Leading),
        }
    }
    
    fn p_let(&mut self) -> Result<Statement, ParseError> {
        let meta = self.meta();
        if self.input.len() == 0 {
            return Err(ParseLetError::Empty.into());
        }

        if self.expect_kw("let") {
            self.skip_ws();
            match self.p_pattern() {
                Err(err) => return Err(ParseLetError::Pattern(err).into()),
                Ok(lhs) => {
                    self.skip_ws();
                    if !self.expect('=') {
                        return Err(ParseLetError::EqualsSign.into());
                    }

                    self.skip_ws();
                    let rhs = self.p_exp()?;
                    
                    return Ok(Statement(_Statement::Let(lhs, rhs), meta));
                },
            }
        } else {
            return Err(ParseLetError::Leading.into());
        }
    }
    
    // TODO this is sooo ugly
    fn p_statement(&mut self) -> Result<Statement, ParseError> {
        match self.peek() {
            None => Err(ParseError::EmptyStmt),
            Some(c) if c.is_ascii_alphabetic() => {
                if self.starts_with_a_kw() {
                    if self.peek_kw("let") {
                        self.p_let()
                    } else if self.peek_kw("throw") {
                        let meta = self.meta();
                        self.skip_str("throw");
                        self.skip_ws();
                        Ok(Statement(_Statement::Throw(self.p_exp()?), meta))
                    } else if self.peek_kw("return") {
                        let meta = self.meta();
                        self.skip_str("return");
                        self.skip_ws();
                        Ok(Statement(_Statement::Return(self.p_exp()?), meta))
                    } else if self.peek_kw("mut") || self.peek_kw("else") { // use a function for non-statement keywords (also do this in p_exp for exp keywords?)
                        Err(ParseError::KwStmt)
                    } else {
                        let exp = self.p_exp()?;
                        let meta = exp.1.clone();
                        Ok(Statement(_Statement::Exp(exp), meta))
                    }
                } else {
                    let exp = self.p_exp()?;
                    let meta = exp.1.clone();
                    
                    match exp.0 {
                        _Expression::Id(ref id) => {
                            match self.peek() {
                                Some('=') => {
                                    self.skip();
                                    self.skip_ws();
                                    let rhs = self.p_exp()?;
                                    Ok(Statement(_Statement::Assign(id.to_string(), rhs), meta))
                                }
                                _ => Ok(Statement(_Statement::Exp(exp), meta)),
                            }
                        }
                        _ => Ok(Statement(_Statement::Exp(exp), meta)),
                    }                  
                }
            },
            Some(_) => {
                let exp = self.p_exp()?;
                let meta = exp.1.clone();
                Ok(Statement(_Statement::Exp(exp), meta))
            }
        }
    }
    
    // just semicolon-separated statements, no braces (use p_block for those)
    pub fn p_statements(&mut self) -> Result<Box<[Statement]>, ParseError> {
        let mut stmts = Vec::new();
        
        if self.end() {
            return Ok(stmts.into_boxed_slice());
        }

        loop {
            self.skip_ws();
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
pub enum ParseIdError {
    #[fail(display = "expected identifier, got end of input")]
    Empty,
    #[fail(display = "expected identifier, did not get an ascii alphanumeric character")]
    Leading,
    #[fail(display = "expected identifier, got a keyword")]
    Kw,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseIfError {
    #[fail(display = "expected if expression, did not get the `if` keyword")]
    NoIfKw,
    #[fail(display = "expected left brace `{{` after the if condition")]
    NoLBraceThen,
    #[fail(display = "expected right brace `}}` after the if condition")]
    NoRBraceThen,
    #[fail(display = "expected left brace `{{` after the `else` condition")]
    NoLBraceElse,
    #[fail(display = "expected right brace `}}` after the `else` condition")]
    NoRBraceElse,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParsePatternError {
    #[fail(display = "expected pattern, got end of input")]
    Empty,
    #[fail(display = "expected pattern, got invalid first character")]
    Leading,
    #[fail(display = "expected pattern, got a keyword that does not start a pattern")]
    NonPatternKw,
    #[fail(display = "erroneous identifier")]
    Id(#[fail(cause)]ParseIdError),
}

impl From<ParseIdError> for ParsePatternError {
    fn from(err: ParseIdError) -> ParsePatternError {
        ParsePatternError::Id(err)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseLetError {
    #[fail(display = "expected let statement, got end of input")]
    Empty,
    #[fail(display = "expected let statement, did not get the `let` keyword")]
    Leading,
    #[fail(display = "invalid left-hand side of let statement")]
    Pattern(#[fail(cause)]ParsePatternError),
    #[fail(display = "let expression requires `=` after the left-hand side")]
    EqualsSign,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseError {
    #[fail(display = "expected a right parentheses `)` to close a left parentheses")]
    RParen,
    #[fail(display = "expected expression, got end of input")]
    EmptyExp,
    #[fail(display = "expected expression, got invalid first character")]
    LeadingExp,
    #[fail(display = "expected expression, got a keyword that does not start an expression")]
    KwExp,
    #[fail(display = "expected statement, got end of input")]
    EmptyStmt,
    #[fail(display = "expected statement, got invalid first character")]
    LeadingStmt,
    #[fail(display = "expected statement, got a keyword that does not start a statement")]
    KwStmt,
    #[fail(display = "erroneous identifier")]
    Id(#[fail(cause)]ParseIdError),
    #[fail(display = "erroneous if expression")]
    If(#[fail(cause)]ParseIfError),
    #[fail(display = "erroneous let statement")]
    Let(#[fail(cause)]ParseLetError),
}

impl From<ParseIdError> for ParseError {
    fn from(err: ParseIdError) -> ParseError {
        ParseError::Id(err)
    }
}

impl From<ParseIfError> for ParseError {
    fn from(err: ParseIfError) -> ParseError {
        ParseError::If(err)
    }
}

impl From<ParseLetError> for ParseError {
    fn from(err: ParseLetError) -> ParseError {
        ParseError::Let(err)
    }
}
