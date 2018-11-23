use std::convert::TryFrom;
use std::sync::Arc;

use num::{BigInt, BigRational, Num};

use super::syntax::*;

pub struct Parser<'a> {
    input: &'a str,
    source: Source,
    position: Position,
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

    /// Returns true if there are no non-whitespace tokens up until the end of the input (comments
    /// are considered whitespace).
    pub fn end(&mut self) -> bool {
        self.skip_ws();
        self.input.len() == 0
    }

    fn meta(&self) -> Meta {
        Meta {
            start: self.position,
            source: self.source.clone(),
        }
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
        if self.input.starts_with("if") {
            return self.input.len() == 2 || !is_ident_byte(self.input.as_bytes()[2]);
        } else if self.input.starts_with("nil") ||
            self.input.starts_with("let") ||
            self.input.starts_with("try") {
            return self.input.len() == 3 || !is_ident_byte(self.input.as_bytes()[3]);
        } else if self.input.starts_with("true") || self.input.starts_with("args") {
            return self.input.len() == 4 || !is_ident_byte(self.input.as_bytes()[4]);
        } else if self.input.starts_with("false") ||
            self.input.starts_with("throw") ||
            self.input.starts_with("pause") ||
            self.input.starts_with("trace") {
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

    fn p_args(&mut self) -> Option<Meta> {
        let meta = self.meta();
        if self.expect_kw("args") {
            Some(meta)
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

    fn p_int(&mut self) -> Result<(BigRational, Meta), ParseIntError> {
        let meta = self.meta();
        let start = self.input;

        if self.skip_str("0x") {
            // parse hexadecimal literal
            match self.next() {
                None => return Err(ParseIntError::EmptyHex),
                Some(c) if c.is_ascii_hexdigit() => {}
                Some(_) => return Err(ParseIntError::LeadingHex),
            }

            self.skip_while(|c| c.is_ascii_hexdigit() || c == '_');

            let int = BigInt::from_str_radix(&start[2..start.len() - self.input.len()], 16).unwrap();
            return Ok((BigRational::from_integer(int), meta));
        } else {
            // parse either a decimal integer or a float
            match self.next() {
                None => return Err(ParseIntError::Empty),
                Some(c) if c.is_ascii_digit() => {}
                Some(_) => return Err(ParseIntError::Leading),
            }

            self.skip_while(|c| c.is_ascii_digit() || c == '_');

            let int = BigInt::from_str_radix(&start[..start.len() - self.input.len()], 10).unwrap();
            return Ok((BigRational::from_integer(int), meta));
        }
    }

    fn p_char(&mut self) -> Result<(char, Meta), ParseCharError> {
        let meta = self.meta();
        match self.next() {
            None => return Err(ParseCharError::Empty),
            Some('\'') => {}
            Some(_) => return Err(ParseCharError::Leading),
        }

        let c;

        match self.next() {
            None => return Err(ParseCharError::EndOfInput),

            Some('\\') => {
                match self.next() {
                    None => return Err(ParseCharError::EndOfInput),
                    Some('\'') => c = '\'',
                    Some('\\') => c = '\\',
                    Some('n') => c = '\n',
                    Some('t') => c = '\t',
                    Some('0') => c = '\0',
                    Some('u') => {
                        if !self.expect('{') {
                            return Err(ParseCharError::UnicodeLBrace);
                        }

                        let mut count = 0;
                        let mut acc = 0;
                        let mut done = false;

                        while !done {
                            match self.next() {
                                None => return Err(ParseCharError::EndOfInput),
                                Some('}') => done = true,
                                Some(c) if c.is_ascii_hexdigit() => {
                                    if count == 6 {
                                        return Err(ParseCharError::UnicodeTooLong);
                                    }
                                    acc <<= 4;
                                    acc += c.to_digit(16).unwrap();
                                    count += 1;
                                }
                                _ => return Err(ParseCharError::UnicodeDigit),
                            }
                        }

                        match char::try_from(acc) {
                            Err(_) => return Err(ParseCharError::UnicodeScalar),
                            Ok(ch) => c = ch,
                        }
                    }
                    _ => return Err(ParseCharError::Escape),
                }
            }

            Some(ch) => c = ch,
        }

        match self.next() {
            None => return Err(ParseCharError::EndOfInput),
            Some('\'') => return Ok((c, meta)),
            Some(_) => return Err(ParseCharError::TooLong),
        }
    }

    fn p_string(&mut self) -> Result<(String, Meta), ParseStringError> {
        let meta = self.meta();
        let mut decoded = String::new();

        match self.next() {
            None => return Err(ParseStringError::Empty),
            Some('"') => {}
            Some(_) => return Err(ParseStringError::Leading),
        }

        loop {
            match self.next() {
                None => return Err(ParseStringError::EndOfInput),

                Some('"') => return Ok((decoded, meta)),

                Some('\\') => {
                    match self.next() {
                        None => return Err(ParseStringError::EndOfInput),
                        Some('"') => decoded.push('"'),
                        Some('\\') => decoded.push('\\'),
                        Some('n') => decoded.push('\n'),
                        Some('t') => decoded.push('\t'),
                        Some('0') => decoded.push('\0'),
                        Some('u') => {
                            if !self.expect('{') {
                                return Err(ParseStringError::UnicodeLBrace);
                            }

                            let mut count = 0;
                            let mut acc = 0;
                            let mut done = false;

                            while !done {
                                match self.next() {
                                    None => return Err(ParseStringError::EndOfInput),
                                    Some('}') => done = true,
                                    Some(c) if c.is_ascii_hexdigit() => {
                                        if count == 6 {
                                            return Err(ParseStringError::UnicodeTooLong);
                                        }
                                        acc <<= 4;
                                        acc += c.to_digit(16).unwrap();
                                        count += 1;
                                    }
                                    _ => return Err(ParseStringError::UnicodeDigit),
                                }
                            }

                            match char::try_from(acc) {
                                Err(_) => return Err(ParseStringError::UnicodeScalar),
                                Ok(c) => decoded.push(c),
                            }
                        }
                        _ => {
                            println!("{}", self.input);
                            return Err(ParseStringError::Escape);
                        },
                        // _ => return Err(ParseStringError::Escape),
                    }
                }

                Some(c) => decoded.push(c),
            }
        }
    }

    fn p_seq(&mut self) -> Result<(Vec<Expression>, Meta), ParseExpressionError> {
        let meta = self.meta();
        let mut seq = Vec::new();

        match self.next() {
            None => return Err(ParseSeqError::Empty.into()),
            Some('[') => {}
            Some(_) => return Err(ParseSeqError::Leading.into()),
        }

        self.skip_ws();
        if let Some(']') = self.peek() {
            self.skip();
            return Ok((seq, meta));
        }

        seq.push(self.p_exp(false)?);

        loop {
            self.skip_ws();
            match self.next() {
                None => return Err(ParseSeqError::EndOfInput.into()),
                Some(']') => return Ok((seq, meta)),
                Some(',') => {
                    self.skip_ws();
                    seq.push(self.p_exp(false)?);
                }
                Some(_) => return Err(ParseSeqError::CommaOrEnd.into()),
            }
        }
    }

    fn p_set(&mut self) -> Result<(Set, Meta), ParseExpressionError> {
        let meta = self.meta();
        let mut set = Vec::new();

        match self.next() {
            None => return Err(ParseSetError::Empty.into()),
            Some('@') => {}
            Some(_) => return Err(ParseSetError::Leading.into()),
        }
        if !self.expect('{') {
            return Err(ParseSetError::OpeningBrace.into());
        }

        self.skip_ws();
        if let Some('}') = self.peek() {
            self.skip();
            return Ok((Set(set), meta));
        }

        set.push(self.p_exp(false)?);

        loop {
            self.skip_ws();
            match self.next() {
                None => return Err(ParseSetError::EndOfInput.into()),
                Some('}') => return Ok((Set(set), meta)),
                Some(',') => {
                    self.skip_ws();
                    set.push(self.p_exp(false)?);
                }
                Some(_) => return Err(ParseSetError::CommaOrEnd.into()),
            }
        }
    }

    fn p_map(&mut self) -> Result<(Vec<(Expression, Expression)>, Meta), ParseExpressionError> {
        let meta = self.meta();
        let mut map = Vec::new();

        match self.next() {
            None => return Err(ParseMapError::Empty.into()),
            Some('{') => {}
            Some(_) => return Err(ParseMapError::Leading.into()),
        }

        self.skip_ws();
        if let Some('}') = self.peek() {
            self.skip();
            return Ok((map, meta));
        }
        let fst_key = self.p_exp(false)?;

        self.skip_ws();
        if !self.expect(':') {
            return Err(ParseMapError::Colon.into());
        }
        self.skip_ws();

        let fst_val = self.p_exp(false)?;
        map.push((fst_key, fst_val));

        let mut key;
        let mut val;

        loop {
            self.skip_ws();
            match self.next() {
                None => return Err(ParseMapError::EndOfInput.into()),
                Some('}') => {
                    return Ok((map, meta));
                },
                Some(',') => {
                    self.skip_ws();
                    key = Some(self.p_exp(false)?);

                    self.skip_ws();
                    if !self.expect(':') {
                        return Err(ParseMapError::Colon.into());
                    }
                    self.skip_ws();

                    val = Some(self.p_exp(false)?);
                    map.push((key.take().unwrap(), val.take().unwrap()));
                }
                Some(_) => return Err(ParseMapError::CommaOrEnd.into()),
            }
        }
    }

    fn p_if(&mut self, tail: bool) -> Result<(If, Meta), ParseExpressionError> {
        let meta = self.meta();
        if self.input.len() == 0 {
            return Err(ParseIfError::Empty.into());
        }

        if self.expect_kw("if") {
            self.skip_ws();
            let cond = Box::new(self.p_exp(false)?);

            self.skip_ws();
            if !self.expect_kw("then") {
                return Err(ParseIfError::Then.into());
            }
            self.skip_ws();
            let then = Box::new(self.p_exp(tail)?);
            self.skip_ws();

            if self.expect_kw("else") {
                self.skip_ws();
                let else_ = Some(Box::new(self.p_exp(tail)?));
                return Ok((If { cond, then, else_ }, meta));
            } else {
                return Ok((If { cond, then, else_: None }, meta));
            }
        } else {
            return Err(ParseIfError::Leading.into());
        }
    }

    fn p_throw(&mut self, tail: bool) -> Result<(Throw, Meta), ParseExpressionError> {
        let meta = self.meta();
        if self.input.len() == 0 {
            return Err(ParseThrowError::Empty.into());
        }

        if self.expect_kw("throw") {
            self.skip_ws();
            return Ok((Throw(Box::new(self.p_exp(tail)?)), meta));
        } else {
            return Err(ParseThrowError::Leading.into());
        }
    }

    fn p_try(&mut self, tail: bool) -> Result<(Try, Meta), ParseExpressionError> {
        let meta = self.meta();
        if self.input.len() == 0 {
            return Err(ParseTryError::Empty.into());
        }

        if self.expect_kw("try") {
            self.skip_ws();
            let to_try = Box::new(self.p_exp(false)?);

            self.skip_ws();
            if !self.expect_kw("catch") {
                return Err(ParseTryError::Catch.into());
            }

            self.skip_ws();
            match self.p_pattern() {
                Err(err) => return Err(ParseTryError::Pattern(err).into()),
                Ok(caught) => {
                    self.skip_ws();
                    let catcher = Box::new(self.p_exp(tail)?);

                    return Ok((Try { to_try, caught, catcher }, meta));
                }
            }
        } else {
            return Err(ParseTryError::Leading.into());
        }
    }

    fn p_pause(&mut self, tail: bool) -> Result<(Pause, Meta), ParseExpressionError> {
        let meta = self.meta();
        if self.input.len() == 0 {
            return Err(ParsePauseError::Empty.into());
        }

        if self.expect_kw("pause") {
            self.skip_ws();

            match self.peek() {
                None => return Err(ParsePauseError::EndOfInputAfterKeyword.into()),
                Some('i') => {
                    if self.expect_kw("if") {
                        self.skip_ws();
                        let cond = Some(Box::new(self.p_exp(false)?));

                        self.skip_ws();
                        if !self.expect(';') {
                            return Err(ParsePauseError::AfterCond.into());
                        }

                        self.skip_ws();
                        let continuing = Box::new(self.p_exp(tail)?);
                        return Ok((Pause { cond, continuing }, meta));
                    } else {
                        return Err(ParsePauseError::AfterKw.into());
                    }
                }
                Some(';') => {
                    self.skip();
                    self.skip_ws();
                    let continuing = Box::new(self.p_exp(tail)?);
                    return Ok((Pause { cond: None, continuing }, meta));
                }
                Some(_) => return Err(ParsePauseError::AfterKw.into()),
            }
        } else {
            return Err(ParsePauseError::Leading.into());
        }
    }

    fn p_id(&mut self) -> Result<(Id, Meta), ParseIdError> {
        let meta = self.meta();
        if self.starts_with_a_kw() {
            return Err(ParseIdError::Kw);
        }

        let start = self.input;

        match self.next() {
            None => return Err(ParseIdError::Empty),
            Some('A'...'Z') | Some('a'...'z') | Some('_') => {},
            Some(_) => return Err(ParseIdError::Leading),
        }

        self.skip_while(|c| c.is_ascii_alphanumeric() || c == '_');

        return Ok((Id(start[..start.len() - self.input.len()].to_string()), meta));
    }

    fn p_pattern(&mut self) -> Result<Pattern, ParsePatternError> {
        match self.peek() {
            None => return Err(ParsePatternError::Empty),
            Some('{') => {
                let mut ids = Vec::new();

                self.skip_ws();
                ids.push(self.p_id()?);

                loop {
                    self.skip_ws();
                    match self.next() {
                        None => return Err(ParsePatternError::EndOfInput),
                        Some('}') => return Ok(Pattern::Map(ids)),
                        Some(',') => {
                            self.skip_ws();
                            ids.push(self.p_id()?);
                        }
                        Some(_) => return Err(ParsePatternError::CommaOrEnd),
                    }
                }
            },
            Some('A'...'Z') | Some('a'...'z') | Some('_') => {
                match self.p_id() {
                    Err(err) => return Err(ParsePatternError::Id(err)),
                    Ok(id) => return Ok(Pattern::Id(id.0, id.1)),
                }
            }
            Some(_) => return Err(ParsePatternError::Leading),
        }
    }

    fn p_let(&mut self, tail: bool) -> Result<(Let, Meta), ParseExpressionError> {
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
                    let rhs = Box::new(self.p_exp(false)?);

                    self.skip_ws();
                    if !self.expect(';') {
                        return Err(ParseLetError::Semicolon.into());
                    }

                    self.skip_ws();
                    let continuing = Box::new(self.p_exp(tail)?);

                    return Ok((Let { lhs, rhs, continuing }, meta));
                },
            }
        } else {
            return Err(ParseLetError::Leading.into());
        }
    }

    fn p_fun(&mut self) -> Result<(Fun, Meta), ParseExpressionError> {
        let meta = self.meta();
        match self.next() {
            None => return Err(ParseFunError::Empty.into()),
            Some('(') => {}
            Some(_) => return Err(ParseFunError::Leading.into()),
        }

        self.skip_ws();
        let args = if let Some(')') = self.peek() {
            self.skip();
            Vec::new()
        } else {
            let mut args_acc = Vec::new();
            args_acc.push(self.p_id()?);

            loop {
                self.skip_ws();
                match self.next() {
                    None => return Err(ParseFunError::EndOfInput.into()),
                    Some(')') => break args_acc,
                    Some(',') => {
                        self.skip_ws();
                        args_acc.push(self.p_id()?);
                    }
                    Some(_) => return Err(ParseFunError::CommaOrEnd.into()),
                }
            }
        };

        self.skip_ws();
        if !self.skip_str("->") {
            return Err(ParseFunError::Arrow.into());
        }

        let body = Box::new(self.p_exp(true)?);
        return Ok((Fun(Arc::new(_Fun { args, body })), meta));
    }

    // Non-leftrecursive expressions
    fn p_lexp(&mut self, tail: bool) -> Result<Expression, ParseExpressionError> {
        self.skip_ws();

        match self.peek() {
            None => Err(ParseExpressionError::Empty),
            Some('"') => Ok(self.p_string()?.into()),
            Some('\'') => Ok(self.p_char()?.into()),
            Some('0'...'9') => Ok(self.p_int()?.into()),
            Some('[') => Ok(self.p_seq()?.into()),
            Some('@') => Ok(self.p_set()?.into()),
            Some('{') => Ok(self.p_map()?.into()),
            Some('(') => Ok(self.p_fun()?.into()),
            Some(c) if c.is_ascii_alphabetic() => {
                if self.starts_with_a_kw() {
                    if self.peek_kw("nil") {
                        Ok(self.p_nil().unwrap().into())
                    } else if self.peek_kw("args") {
                        let meta = self.p_args().unwrap();
                        Ok(Expression(_Expression::Args, meta))
                    } else if self.peek_kw("true") {
                        Ok(self.p_bool().unwrap().into())
                    } else if self.peek_kw("false") {
                        Ok(self.p_bool().unwrap().into())
                    } else if self.peek_kw("if") {
                        Ok(self.p_if(tail)?.into())
                    } else if self.peek_kw("let") {
                        Ok(self.p_let(tail)?.into())
                    } else if self.peek_kw("throw") {
                        Ok(self.p_throw(tail)?.into())
                    } else if self.peek_kw("try") {
                        Ok(self.p_try(tail)?.into())
                    } else if self.peek_kw("pause") {
                        Ok(self.p_pause(tail)?.into())
                    } else {
                        Err(ParseExpressionError::NonExpressionKw)
                    }
                } else {
                    Ok(self.p_id()?.into())
                }
            },
            Some(_) => Err(ParseExpressionError::Leading),
        }
    }

    pub fn p_exp(&mut self, tail: bool) -> Result<Expression, ParseExpressionError> {
        let mut left = self.p_lexp(tail)?;
        self.skip_ws();

        match self.peek() {
            None => Ok(left),
            // TODO unset tail on left if not None
            Some('(') => Ok(self.p_app(left, tail)?.into()),
            Some('&') => {
                left.untail();
                let right = Box::new(self.p_land(tail)?);
                let meta = left.1.clone();
                Ok(Expression(_Expression::Land(Box::new(left), right), meta))
            }
            Some('|') => {
                left.untail();
                let right = Box::new(self.p_lor(tail)?);
                let meta = left.1.clone();
                Ok(Expression(_Expression::Lor(Box::new(left), right), meta))
            }
            Some(_) => Ok(left), // TODO error instead, this means trailing garbage (can remove `end` method then)
        }
    }

    fn p_land(&mut self, tail: bool) -> Result<Expression, ParseExpressionError> {
        if !self.skip_str("&&") {
            return Err(ParseLandError::Op.into());
        }
        self.p_exp(tail)
    }

    fn p_lor(&mut self, tail: bool) -> Result<Expression, ParseExpressionError> {
        if !self.skip_str("||") {
            return Err(ParseLorError::Op.into());
        }
        self.p_exp(tail)
    }

    fn p_app(&mut self, mut left: Expression, tail: bool) -> Result<(App, Meta), ParseExpressionError> {
        left.untail();
        let meta = left.1.clone();
        let mut args = Vec::new();

        match self.next() {
            None => return Err(ParseAppError::Empty.into()),
            Some('(') => {}
            Some(_) => return Err(ParseAppError::Leading.into()),
        }

        self.skip_ws();
        if let Some(')') = self.peek() {
            self.skip();
            return Ok((App { fun: Box::new(left), args, tail }, meta));
        }

        args.push(self.p_exp(false)?);

        loop {
            self.skip_ws();
            match self.next() {
                None => return Err(ParseAppError::EndOfInput.into()),
                Some(')') => return Ok((App { fun: Box::new(left), args, tail }, meta)),
                Some(',') => {
                    self.skip_ws();
                    args.push(self.p_exp(false)?);
                }
                Some(_) => return Err(ParseAppError::CommaOrEnd.into()),
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseIntError {
    #[fail(display = "expected integer literal, got end of input")]
    Empty,
    #[fail(display = "expected integer literal, got invalid first digit")]
    Leading,
    #[fail(display = "hex integer literal must contain at least one digit")]
    EmptyHex,
    #[fail(display = "hex integer literal has invalid first digit")]
    LeadingHex,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseCharError {
    #[fail(display = "expected char literal, got end of input")]
    Empty,
    #[fail(display = "expected char literal, got non-quote first character")]
    Leading,
    #[fail(display = "reached end of input while parsing a char literal")]
    EndOfInput,
    #[fail(display = "char literal can only contain a single character")]
    TooLong,
    #[fail(display = "invalid character following the escape `\\` in a char literal")]
    Escape,
    #[fail(display = "unicode escape must begin with `{{`")]
    UnicodeLBrace,
    #[fail(display = "unicode escape must consist of hex digits")]
    UnicodeDigit,
    #[fail(display = "unicode escape may consist of at most six hex digits")]
    UnicodeTooLong,
    #[fail(display = "unicode escape must encode a valid scalar value")]
    UnicodeScalar,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseStringError {
    #[fail(display = "expected string literal, got end of input")]
    Empty,
    #[fail(display = "expected string literal, got non-quote first character")]
    Leading,
    #[fail(display = "reached end of input while parsing a string literal")]
    EndOfInput,
    #[fail(display = "invalid character following the escape `\\` in a string literal")]
    Escape,
    #[fail(display = "unicode escape must begin with `{{`")]
    UnicodeLBrace,
    #[fail(display = "unicode escape must consist of hex digits")]
    UnicodeDigit,
    #[fail(display = "unicode escape may consist of at most six hex digits")]
    UnicodeTooLong,
    #[fail(display = "unicode escape must encode a valid scalar value")]
    UnicodeScalar,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseSeqError {
    #[fail(display = "expected sequence literal, got end of input")]
    Empty,
    #[fail(display = "expected sequence literal, got non-bracket character")]
    Leading,
    #[fail(display = "reached end of input while parsing a sequence literal")]
    EndOfInput,
    #[fail(display = "expected a comma to continue a sequence, or a bracket to terminate it")]
    CommaOrEnd,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseSetError {
    #[fail(display = "expected set literal, got end of input")]
    Empty,
    #[fail(display = "expected set literal, got non-brace character")]
    Leading,
    #[fail(display = "reached end of input while parsing a set literal")]
    EndOfInput,
    #[fail(display = "expected a comma to continue a set, or a closing brace to terminate it")]
    CommaOrEnd,
    #[fail(display = "second character of a set literal must be an opening brace")]
    OpeningBrace,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseMapError {
    #[fail(display = "expected map literal, got end of input")]
    Empty,
    #[fail(display = "expected map literal, got non-brace character")]
    Leading,
    #[fail(display = "reached end of input while parsing a map literal")]
    EndOfInput,
    #[fail(display = "expected a colon to delimit key and value")]
    Colon,
    #[fail(display = "expected a comma to continue a map, or a closing brace to terminate it")]
    CommaOrEnd,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseIfError {
    #[fail(display = "expected if expression, got end of input")]
    Empty,
    #[fail(display = "expected if expression, did not get the `if` keyword")]
    Leading,
    #[fail(display = "expected `then` keyword")]
    Then,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseThrowError {
    #[fail(display = "expected throw expression, got end of input")]
    Empty,
    #[fail(display = "expected throw expression, did not get the `throw` keyword")]
    Leading,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseTryError {
    #[fail(display = "expected try expression, got end of input")]
    Empty,
    #[fail(display = "expected try expression, did not get the `try` keyword")]
    Leading,
    #[fail(display = "expected `catch` keyword")]
    Catch,
    #[fail(display = "invalid pattern for the caught exception")]
    Pattern(#[fail(cause)]ParsePatternError),
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParsePauseError {
    #[fail(display = "expected pause expression, got end of input")]
    Empty,
    #[fail(display = "expected pause expression, did not get the `pause` keyword")]
    Leading,
    #[fail(display = "pause keyword must be followed by a semicolon or the if keyword")]
    EndOfInputAfterKeyword, // separate variant to support repls
    #[fail(display = "pause keyword must be followed by a semicolon or the if keyword")]
    AfterKw,
    #[fail(display = "pause condition must be followed by a semicolon")]
    AfterCond,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseIdError {
    #[fail(display = "expected identifier, got end of input")]
    Empty,
    #[fail(display = "expected identifier, got neither an ascii alphanumeric character nor an underscore")]
    Leading,
    #[fail(display = "expected identifier, got a keyword")]
    Kw
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParsePatternError {
    #[fail(display = "expected pattern, got end of input")]
    Empty,
    #[fail(display = "expected pattern, got invalid first character")]
    Leading,
    #[fail(display = "invalid identifier")]
    Id(#[fail(cause)]ParseIdError),
    #[fail(display = "reached end of input while parsing a pattern")]
    EndOfInput,
    #[fail(display = "expected a comma to continue a pattern, or a closing brace to terminate it")]
    CommaOrEnd,
}

impl From<ParseIdError> for ParsePatternError {
    fn from(err: ParseIdError) -> ParsePatternError {
        ParsePatternError::Id(err)
    }
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseLetError {
    #[fail(display = "expected let expression, got end of input")]
    Empty,
    #[fail(display = "expected let expression, did not get the `let` keyword")]
    Leading,
    #[fail(display = "invalid left-hand side")]
    Pattern(#[fail(cause)]ParsePatternError),
    #[fail(display = "let expression requires `=` after the left-hand side")]
    EqualsSign,
    #[fail(display = "let expression requires `;` after the binding")]
    Semicolon,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseFunError {
    #[fail(display = "expected function literal, got end of input")]
    Empty,
    #[fail(display = "expected function literal, did not get an opening paren")]
    Leading,
    #[fail(display = "reached end of input while parsing a function argument list")]
    EndOfInput,
    #[fail(display = "expected a comma to continue a function argument list, or a closing paren to terminate it")]
    CommaOrEnd,
    #[fail(display = "argument list must be followed by `->`")]
    Arrow,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseAppError {
    #[fail(display = "expected function application, got end of input")]
    Empty,
    #[fail(display = "expected function application, did not get an opening paren")]
    Leading,
    #[fail(display = "reached end of input while parsing the arguments of a function application")]
    EndOfInput,
    #[fail(display = "expected a comma to continue the list of arguments of a function application, or a closing paren to terminate it")]
    CommaOrEnd,
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseLandError {
    #[fail(display = "expected 'and' operator, did not get `&&`")]
    Op
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseLorError {
    #[fail(display = "expected 'or' operator, did not get `||`")]
    Op
}

#[derive(PartialEq, Eq, Debug, Clone, Hash, PartialOrd, Ord, Fail)]
pub enum ParseExpressionError {
    #[fail(display = "expected expression, got end of input")]
    Empty,
    #[fail(display = "expected expression, got invalid first character")]
    Leading,
    #[fail(display = "expected expression, got a keyword that does not start an expression")]
    NonExpressionKw,
    #[fail(display = "erroneous char literal")]
    Char(#[fail(cause)]ParseCharError),
    #[fail(display = "erroneous string literal")]
    String(#[fail(cause)]ParseStringError),
    #[fail(display = "erroneous number literal")]
    Num(#[fail(cause)]ParseIntError),
    #[fail(display = "erroneous sequence literal")]
    Seq(#[fail(cause)]ParseSeqError),
    #[fail(display = "erroneous set literal")]
    Set(#[fail(cause)]ParseSetError),
    #[fail(display = "erroneous map literal")]
    Map(#[fail(cause)]ParseMapError),
    #[fail(display = "erroneous if expression")]
    If(#[fail(cause)]ParseIfError),
    #[fail(display = "erroneous throw expression")]
    Throw(#[fail(cause)]ParseThrowError),
    #[fail(display = "erroneous try expression")]
    Try(#[fail(cause)]ParseTryError),
    #[fail(display = "erroneous pause expression")]
    Pause(#[fail(cause)]ParsePauseError),
    #[fail(display = "erroneous identifier")]
    Id(#[fail(cause)]ParseIdError),
    #[fail(display = "erroneous let expression")]
    Let(#[fail(cause)]ParseLetError),
    #[fail(display = "erroneous function literal")]
    Fun(#[fail(cause)]ParseFunError),
    #[fail(display = "erroneous function application")]
    App(#[fail(cause)]ParseAppError),
    #[fail(display = "erroneous 'and' operator")]
    Land(#[fail(cause)]ParseLandError),
    #[fail(display = "erroneous 'or' operator")]
    Lor(#[fail(cause)]ParseLorError),
}

impl From<ParseCharError> for ParseExpressionError {
    fn from(err: ParseCharError) -> ParseExpressionError {
        ParseExpressionError::Char(err)
    }
}

impl From<ParseStringError> for ParseExpressionError {
    fn from(err: ParseStringError) -> ParseExpressionError {
        ParseExpressionError::String(err)
    }
}

impl From<ParseIntError> for ParseExpressionError {
    fn from(err: ParseIntError) -> ParseExpressionError {
        ParseExpressionError::Num(err)
    }
}

impl From<ParseSeqError> for ParseExpressionError {
    fn from(err: ParseSeqError) -> ParseExpressionError {
        ParseExpressionError::Seq(err)
    }
}

impl From<ParseSetError> for ParseExpressionError {
    fn from(err: ParseSetError) -> ParseExpressionError {
        ParseExpressionError::Set(err)
    }
}

impl From<ParseMapError> for ParseExpressionError {
    fn from(err: ParseMapError) -> ParseExpressionError {
        ParseExpressionError::Map(err)
    }
}

impl From<ParseIfError> for ParseExpressionError {
    fn from(err: ParseIfError) -> ParseExpressionError {
        ParseExpressionError::If(err)
    }
}

impl From<ParseThrowError> for ParseExpressionError {
    fn from(err: ParseThrowError) -> ParseExpressionError {
        ParseExpressionError::Throw(err)
    }
}

impl From<ParseTryError> for ParseExpressionError {
    fn from(err: ParseTryError) -> ParseExpressionError {
        ParseExpressionError::Try(err)
    }
}

impl From<ParsePauseError> for ParseExpressionError {
    fn from(err: ParsePauseError) -> ParseExpressionError {
        ParseExpressionError::Pause(err)
    }
}

impl From<ParseIdError> for ParseExpressionError {
    fn from(err: ParseIdError) -> ParseExpressionError {
        ParseExpressionError::Id(err)
    }
}

impl From<ParseLetError> for ParseExpressionError {
    fn from(err: ParseLetError) -> ParseExpressionError {
        ParseExpressionError::Let(err)
    }
}

impl From<ParseFunError> for ParseExpressionError {
    fn from(err: ParseFunError) -> ParseExpressionError {
        ParseExpressionError::Fun(err)
    }
}

impl From<ParseAppError> for ParseExpressionError {
    fn from(err: ParseAppError) -> ParseExpressionError {
        ParseExpressionError::App(err)
    }
}

impl From<ParseLandError> for ParseExpressionError {
    fn from(err: ParseLandError) -> ParseExpressionError {
        ParseExpressionError::Land(err)
    }
}

impl From<ParseLorError> for ParseExpressionError {
    fn from(err: ParseLorError) -> ParseExpressionError {
        ParseExpressionError::Lor(err)
    }
}
