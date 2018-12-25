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
        || self.input.starts_with("let")
        || self.input.starts_with("try") {
            return self.input.len() == 3 || !is_ident_byte(self.input.as_bytes()[3]);
        } else if self.input.starts_with("true")
        || self.input.starts_with("case")
        || self.input.starts_with("loop") {
            return self.input.len() == 4 || !is_ident_byte(self.input.as_bytes()[4]);
        } else if self.input.starts_with("false")
        || self.input.starts_with("throw")
        || self.input.starts_with("break")
        || self.input.starts_with("while")
        || self.input.starts_with("catch") {
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
    
    fn p_while(&mut self) -> Result<Expression, ParseError> {
        let meta = self.meta();
        if self.peek_kw("while") { // TODO abstract this into a method
            self.skip_str("while");
        } else {
            return Err(ParseWhileError::NoWhileKw.into());
        }
        
        self.skip_ws();
        let cond = Box::new(self.p_exp()?);
        
        self.skip_ws();
        if !self.expect('{') {
            return Err(ParseWhileError::NoLBrace.into());
        }
        
        self.skip_ws();        
        let body = self.p_statements()?;
        
        self.skip_ws();        
        if !self.expect('}') {
            return Err(ParseWhileError::NoRBrace.into());
        }
        
        return Ok(Expression(_Expression::While(cond, body), meta));
    }
    
    fn p_try(&mut self) -> Result<Expression, ParseError> {
        let meta = self.meta();
        if self.peek_kw("try") { // TODO abstract this into a method
            self.skip_str("try");
        } else {
            return Err(ParseTryError::NoTryKw.into());
        }
        
        self.skip_ws();
        if !self.expect('{') {
            return Err(ParseTryError::NoLBraceTry.into());
        }
        
        self.skip_ws();        
        let try_body = self.p_statements()?;
        
        self.skip_ws();        
        if !self.expect('}') {
            return Err(ParseTryError::NoRBraceTry.into());
        }
        
        self.skip_ws();
        if self.peek_kw("catch") { // TODO abstract this into a method
            self.skip_str("catch");
        } else {
            return Err(ParseTryError::NoCatchKw.into());
        }
        
        self.skip_ws();
        match self.p_pattern() {
            Err(err) => return Err(ParseTryError::Pattern(err).into()),
            Ok(pat) => {
                self.skip_ws();
                if !self.expect('{') {
                    return Err(ParseTryError::NoLBraceCatch.into());
                }
                
                self.skip_ws();        
                let catch_body = self.p_statements()?;
                
                self.skip_ws();        
                if !self.expect('}') {
                    return Err(ParseTryError::NoRBraceCatch.into());
                }    
                
                return Ok(Expression(_Expression::Try(try_body, pat, catch_body), meta));
            },
        }
    }
    
    fn p_case(&mut self) -> Result<Expression, ParseError> {
        let meta = self.meta();
        if self.peek_kw("case") { // TODO abstract this into a method
            self.skip_str("case");
        } else {
            return Err(ParseCaseError::NoCaseKw.into());
        }
        
        self.skip_ws();
        let matchee = Box::new(self.p_exp()?);
        
        self.skip_ws();
        if !self.expect('{') {
            return Err(ParseCaseError::NoLBrace.into());
        }
        
        self.skip_ws();        
        let branches = self.p_branches()?;
        
        self.skip_ws();        
        if !self.expect('}') {
            return Err(ParseCaseError::NoRBrace.into());
        }
        
        return Ok(Expression(_Expression::Case(matchee, branches), meta));
    }
    
    fn p_loop(&mut self) -> Result<Expression, ParseError> {
        let meta = self.meta();
        if self.peek_kw("loop") { // TODO abstract this into a method
            self.skip_str("loop");
        } else {
            return Err(ParseLoopError::NoLoopKw.into());
        }
        
        self.skip_ws();
        let matchee = Box::new(self.p_exp()?);
        
        self.skip_ws();
        if !self.expect('{') {
            return Err(ParseLoopError::NoLBrace.into());
        }
        
        self.skip_ws();        
        let branches = self.p_branches()?;
        
        self.skip_ws();        
        if !self.expect('}') {
            return Err(ParseLoopError::NoRBrace.into());
        }
        
        return Ok(Expression(_Expression::Loop(matchee, branches), meta));
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
                    } else if self.peek_kw("while") {
                        self.p_while()
                    } else if self.peek_kw("try") {
                        self.p_try()
                    } else if self.peek_kw("case") {
                        self.p_case()
                    } else if self.peek_kw("loop") {
                        self.p_loop()
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
        
        if self.skip_str("(") {
            self.skip_ws();
            let args = self.p_exps()?;
            
            self.skip_ws();        
            if !self.expect(')') {
                return Err(ParseError::RParenApplication);
            }
            
            return Ok(Expression(_Expression::Application(Box::new(left), args), meta));            
        } else if self.skip_str("&&") {
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
    
    // semicolon-separated expressions, terminated by a `)` (which is *not* consumed by this parser)
    pub fn p_exps(&mut self) -> Result<Box<[Expression]>, ParseError> {
        let mut exps = Vec::new();
        
        self.skip_ws();
        if let Some(')') = self.peek() {
            return Ok(exps.into_boxed_slice());
        }

        loop {
            self.skip_ws();
            exps.push(self.p_exp()?);
            self.skip_ws();
            match self.peek() {
                Some(',') => self.skip(),
                Some(')') => return Ok(exps.into_boxed_slice()),
                _ => return Err(ParseError::ExpList),
            }
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
    
    fn p_patterns(&mut self) -> Result<Patterns, ParseError> {
        let meta = self.meta();
        self.skip_ws();
        let mut pats = Vec::new();
        
        loop {
            self.skip_ws();
            let pat = self.p_pattern()?;
            pats.push(pat);
            self.skip_ws();
            
            if self.input.starts_with("=>") {
                return Ok(Patterns(pats.into_boxed_slice(), None, meta));
            } else if self.skip_str("|") {
                continue;
            } else if self.peek_kw("if") {
                self.skip_str("if");
                self.skip_ws();
                let guard = Box::new(self.p_exp()?);
                return Ok(Patterns(pats.into_boxed_slice(), Some(guard), meta));
            }
        }
    }
    
    fn p_branch(&mut self) -> Result<(Patterns, Box<[Statement]>), ParseError> {
        self.skip_ws();
        let patterns = self.p_patterns()?;
        
        self.skip_ws();
        if !self.skip_str("=>") {
            return Err(ParseBranchError::Arrow.into());
        }
        
        self.skip_ws();
        if !self.expect('{') {
            return Err(ParseBranchError::NoLBrace.into());
        }
        
        self.skip_ws();        
        let body = self.p_statements()?;
        
        self.skip_ws();        
        if !self.expect('}') {
            return Err(ParseBranchError::NoRBrace.into());
        }
        
        return Ok((patterns, body));
    }
    
    // branches, terminated by a `}` (which is *not* consumed by this parser)
    fn p_branches(&mut self) -> Result<Box<[(Patterns, Box<[Statement]>)]>, ParseError> {
        let mut branches = Vec::new();
        
        self.skip_ws();
        if let Some('}') = self.peek() {
            return Ok(branches.into_boxed_slice());
        }

        loop {
            self.skip_ws();
            branches.push(self.p_branch()?);
            self.skip_ws();
            if let Some('}') = self.peek() {
                return Ok(branches.into_boxed_slice());
            }
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
                    } else if self.peek_kw("break") {
                        let meta = self.meta();
                        self.skip_str("break");
                        self.skip_ws();
                        Ok(Statement(_Statement::Break(self.p_exp()?), meta))
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
    
    // semicolon-separated statements, terminated by a `}` (which is *not* consumed by this parser)
    pub fn p_statements(&mut self) -> Result<Box<[Statement]>, ParseError> {
        let mut stmts = Vec::new();
        
        self.skip_ws();
        if let Some('}') = self.peek() {
            return Ok(stmts.into_boxed_slice());
        }

        loop {
            self.skip_ws();
            stmts.push(self.p_statement()?);
            self.skip_ws();
            match self.peek() {
                Some(';') => self.skip(),
                Some('}') => return Ok(stmts.into_boxed_slice()),
                _ => return Err(ParseError::StmtList),
            }
        }
    }
    
    // semicolon-separated statements, no braces
    pub fn p_program(&mut self) -> Result<Box<[Statement]>, ParseError> {
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
    #[fail(display = "expected right brace `}}` to terminate the then branch")]
    NoRBraceThen,
    #[fail(display = "expected left brace `{{` after the `else` keyword")]
    NoLBraceElse,
    #[fail(display = "expected right brace `}}` to terminate the else branch")]
    NoRBraceElse,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseWhileError {
    #[fail(display = "expected while expression, did not get the `while` keyword")]
    NoWhileKw,
    #[fail(display = "expected left brace `{{` after the while condition")]
    NoLBrace,
    #[fail(display = "expected right brace `}}` to terminate the while loop body")]
    NoRBrace,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseTryError {
    #[fail(display = "expected try expression, did not get the `try` keyword")]
    NoTryKw,
    #[fail(display = "expected left brace `{{` after the try keyword")]
    NoLBraceTry,
    #[fail(display = "expected right brace `}}` to terminate the try body")]
    NoRBraceTry,
    #[fail(display = "expected catch block, did not get the `catch` keyword")]
    NoCatchKw,
    #[fail(display = "expected left brace `{{` after the catch pattern")]
    NoLBraceCatch,
    #[fail(display = "expected right brace `}}` to terminate the catch body")]
    NoRBraceCatch,
    #[fail(display = "invalid catch pattern")]
    Pattern(#[fail(cause)]ParsePatternError),
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseCaseError {
    #[fail(display = "expected case expression, did not get the `case` keyword")]
    NoCaseKw,
    #[fail(display = "expected left brace `{{` after the case matchee")]
    NoLBrace,
    #[fail(display = "expected right brace `}}` to terminate the case body")]
    NoRBrace,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseLoopError {
    #[fail(display = "expected loop expression, did not get the `loop` keyword")]
    NoLoopKw,
    #[fail(display = "expected left brace `{{` after the loop matchee")]
    NoLBrace,
    #[fail(display = "expected right brace `}}` to terminate the loop body")]
    NoRBrace,
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
pub enum ParseBranchError {
    #[fail(display = "expected a `=>` arrow after the pattern(s)")]
    Arrow,
    #[fail(display = "expected left brace `{{` after the arrow")]
    NoLBrace,
    #[fail(display = "expected right brace `}}` to terminate the branch body")]
    NoRBrace,
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
    #[fail(display = "expected a right parentheses `)` to terminate the function application")]
    RParenApplication,
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
    #[fail(display = "expected either a semicolon to continue the list of statements, or a right curly brace `}}` to terminate it")]
    StmtList,
    #[fail(display = "expected either a comma to continue the list of expressions, or a right paren `)` to terminate it")]
    ExpList,
    #[fail(display = "erroneous identifier")]
    Id(#[fail(cause)]ParseIdError),
    #[fail(display = "erroneous if expression")]
    If(#[fail(cause)]ParseIfError),
    #[fail(display = "erroneous while expression")]
    While(#[fail(cause)]ParseWhileError),
    #[fail(display = "erroneous try expression")]
    Try(#[fail(cause)]ParseTryError),
    #[fail(display = "erroneous case expression")]
    Case(#[fail(cause)]ParseCaseError),
    #[fail(display = "erroneous loop expression")]
    Loop(#[fail(cause)]ParseLoopError),
    #[fail(display = "erroneous branch")]
    Branch(#[fail(cause)]ParseBranchError),
    #[fail(display = "erroneous pattern")]
    Pattern(#[fail(cause)]ParsePatternError),
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

impl From<ParseWhileError> for ParseError {
    fn from(err: ParseWhileError) -> ParseError {
        ParseError::While(err)
    }
}

impl From<ParseTryError> for ParseError {
    fn from(err: ParseTryError) -> ParseError {
        ParseError::Try(err)
    }
}

impl From<ParseCaseError> for ParseError {
    fn from(err: ParseCaseError) -> ParseError {
        ParseError::Case(err)
    }
}

impl From<ParseLoopError> for ParseError {
    fn from(err: ParseLoopError) -> ParseError {
        ParseError::Loop(err)
    }
}

impl From<ParseBranchError> for ParseError {
    fn from(err: ParseBranchError) -> ParseError {
        ParseError::Branch(err)
    }
}

impl From<ParsePatternError> for ParseError {
    fn from(err: ParsePatternError) -> ParseError {
        ParseError::Pattern(err)
    }
}

impl From<ParseLetError> for ParseError {
    fn from(err: ParseLetError) -> ParseError {
        ParseError::Let(err)
    }
}
