use std::sync::Arc;

use im::{Vector, OrdSet, OrdMap};
use num::{BigRational, BigInt, ToPrimitive};
use ropey::{Rope, RopeBuilder};

use super::CmpRope;
use super::syntax::{self, Meta, Source, _Source};
use super::evaluate::Env;

/// Runtime representation of an arbitrary lyra value.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Value {
    Nil,
    Bool(bool),
    Usize(usize),
    Isize(isize),
    Int(BigInt),
    Ratio(BigRational),
    Char(char),
    String(Arc<CmpRope>),
    Sequence(Vector<Value>),
    Set(OrdSet<Value>),
    Map(OrdMap<Value, Value>),
    Fun(_Fun),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum _Fun {
    LyraLiteral(syntax::Fun, Arc<Env>),
    // LyraFun(syntax::Fun, Env, UnsafeCell<Weak<syntax::_Fun>>),
    Rust(fn(Vector<Value>) -> Result<Value, Value>),
}

impl Value {
    pub fn truthyness(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Bool(false) => false,
            _ => true,
        }
    }

    pub fn from_meta(meta: &Meta) -> Value {
        let mut map = OrdMap::new();
        map.insert("line".into(), meta.start.line.into());
        map.insert("col".into(), meta.start.col.into());
        map.insert("offset".into(), meta.start.offset.into());
        map.insert("source".into(), Value::from_source(&meta.source));

        Value::Map(map)
    }

    pub fn from_source(src: &Source) -> Value {
        match src.0.as_ref() {
            _Source::File(ref buf) => buf.to_string_lossy().as_ref().into(),
            _Source::Eval(ref meta) => Value::from_meta(meta),
            _Source::Interactive => Value::Nil,
        }
    }

    pub fn to_rope(&self) -> Rope {
        let mut builder = RopeBuilder::new();
        self.to_rope_indent(&mut builder, 0);
        builder.finish()
    }

    fn to_rope_indent(&self, builder: &mut RopeBuilder, indent: usize) {
        match self {
            Value::Nil => builder.append("nil"),
            Value::Bool(true) => builder.append("true"),
            Value::Bool(false) => builder.append("false"),
            Value::Usize(val) => builder.append(&val.to_string()),
            Value::Isize(val) => builder.append(&val.to_string()),
            Value::Int(val) => builder.append(&val.to_str_radix(10)),
            Value::Ratio(val) => {
                builder.append(&val.numer().to_str_radix(10));
                builder.append(" / ");
                builder.append(&val.denom().to_str_radix(10));
            },
            Value::Char(val) => builder.append(&val.escape_default().to_string()), // XXX escape_default isn't ideal (should be consistent with lyra syntax instead)
            Value::String(val) => builder.append(&format!("{:?}", &val)), // XXX use more appropriate escapes
            Value::Sequence(val) => {
                let len = val.len();
                if len == 0 {
                    builder.append("[]");
                } else {
                    builder.append("[\n");

                    let ws = make_ws(indent);
                    builder.append(&ws);

                    for (i, entry) in val.iter().enumerate() {
                        builder.append(&ws);
                        entry.to_rope_indent(builder, indent + 1);
                        if i + 1 < len {
                            builder.append(",\n");
                        }
                    }

                    builder.append("]\n");
                }
            }
            Value::Set(val) => {
                let len = val.len();
                if len == 0 {
                    builder.append("@{}");
                } else {
                    builder.append("@{\n");

                    let ws = make_ws(indent);
                    builder.append(&ws);

                    for (i, entry) in val.iter().enumerate() {
                        builder.append(&ws);
                        entry.to_rope_indent(builder, indent + 1);
                        if i + 1 < len {
                            builder.append(",\n");
                        }
                    }

                    builder.append("}\n");
                }
            }
            Value::Map(val) => {
                let len = val.len();
                if len == 0 {
                    builder.append("{}");
                } else {
                    builder.append("{\n");

                    let ws = make_ws(indent);
                    builder.append(&ws);

                    for (i, (key, value)) in val.iter().enumerate() {
                        builder.append(&ws);
                        key.to_rope_indent(builder, indent + 1);
                        builder.append(": ");
                        value.to_rope_indent(builder, indent + 1);
                        if i + 1 < len {
                            builder.append(",\n");
                        }
                    }

                    builder.append("}\n");
                }
            }
            Value::Fun(..) => builder.append("[Function]"),
        }
    }
}

fn make_ws(indent: usize) -> String {
    let mut ws = String::with_capacity((indent + 1) * 2);
    for _ in 0..=indent {
        ws.push_str("  ");
    }
    ws
}

impl From<()> for Value {
    fn from(_exp: ()) -> Value {
        Value::Nil
    }
}

impl From<bool> for Value {
    fn from(exp: bool) -> Value {
        Value::Bool(exp)
    }
}

impl From<usize> for Value {
    fn from(exp: usize) -> Value {
        Value::Usize(exp)
    }
}

impl From<&BigRational> for Value {
    fn from(exp: &BigRational) -> Value {
        if exp.is_integer() {
            let int = exp.to_integer();
            if let Some(n) = int.to_usize() {
                Value::Usize(n)
            } else if let Some(n) = int.to_isize() {
                Value::Isize(n)
            } else {
                Value::Int(int)
            }
        } else {
            Value::Ratio(exp.clone())
        }
    }
}

impl From<char> for Value {
    fn from(exp: char) -> Value {
        Value::Char(exp)
    }
}

impl From<&str> for Value {
    fn from(exp: &str) -> Value {
        Value::String(Arc::new(CmpRope::from_str(exp)))
    }
}

impl From<Vector<Value>> for Value {
    fn from(exp: Vector<Value>) -> Value {
        Value::Sequence(exp)
    }
}

impl From<OrdSet<Value>> for Value {
    fn from(exp: OrdSet<Value>) -> Value {
        Value::Set(exp)
    }
}

impl From<OrdMap<Value, Value>> for Value {
    fn from(exp: OrdMap<Value, Value>) -> Value {
        Value::Map(exp)
    }
}
