use std::sync::Arc;

use im::{Vector, OrdSet, OrdMap};
use num::{BigRational, BigInt, ToPrimitive};
use ropey::{Rope, RopeBuilder};
use ryu;

use super::{CmpF64, CmpRope};
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
    Float(CmpF64),
    Char(char),
    String(Arc<CmpRope>),
    Sequence(Vector<Object>),
    Set(OrdSet<Object>),
    Map(OrdMap<Object, Object>),
    Fun(_Fun),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum _Fun {
    Lyra(syntax::Fun, Env),
    Rust(fn(Vector<Object>) -> Result<Object, Object>),
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
        map.insert(Object("line".into(), ()), Object(meta.start.line.into(), ()));
        map.insert(Object("col".into(), ()), Object(meta.start.col.into(), ()));
        map.insert(Object("offset".into(), ()), Object(meta.start.offset.into(), ()));
        map.insert(Object("source".into(), ()), Object::from_source(&meta.source));

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
            Value::Float(val) => {
                if f64::is_nan(val.0) {
                    builder.append("NaN");
                } else if val.0 == std::f64::INFINITY {
                    builder.append("Inf");
                } else if val.0 == std::f64::NEG_INFINITY {
                    builder.append("-Inf");
                } else {
                    let mut buffer = ryu::Buffer::new();
                    builder.append(buffer.format(val.0))
                }
            }
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
                        entry.0.to_rope_indent(builder, indent + 1);
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
                        entry.0.to_rope_indent(builder, indent + 1);
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
                        key.0.to_rope_indent(builder, indent + 1);
                        builder.append(": ");
                        value.0.to_rope_indent(builder, indent + 1);
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

impl From<CmpF64> for Value {
    fn from(exp: CmpF64) -> Value {
        Value::Float(exp)
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

impl From<Vector<Object>> for Value {
    fn from(exp: Vector<Object>) -> Value {
        Value::Sequence(exp)
    }
}

impl From<OrdSet<Object>> for Value {
    fn from(exp: OrdSet<Object>) -> Value {
        Value::Set(exp)
    }
}

impl From<OrdMap<Object, Object>> for Value {
    fn from(exp: OrdMap<Object, Object>) -> Value {
        Value::Map(exp)
    }
}

/// An object is a value with an associated prototype.
/// TODO make the prototype an actual thing rather than Unit
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Object(pub Value, pub ());

impl Object {
    pub fn truthyness(&self) -> bool {
        self.0.truthyness()
    }

    pub fn from_meta(meta: &Meta) -> Object {
        Object(Value::from_meta(meta), ())
    }

    pub fn from_source(src: &Source) -> Object {
        Object(Value::from_source(src), ())
    }
}
