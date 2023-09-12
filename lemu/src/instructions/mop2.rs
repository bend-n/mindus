use super::get_num;
use crate::{lexer::Token, memory::LVar};
use std::f64::consts::PI;

super::op_enum! { pub enum MathOp2 {
    Angle,
    Add,
    Sub,
    Mul,
    Div,
    IDiv,
    Mod,
    Pow,
    Equal,
    NotEqual,
    And,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    StrictEqual,
    ShiftLeft,
    ShiftRight,
    BitOr,
    BitAnd,
    ExclusiveOr,
    Max,
    Min,
    AngleDiff,
    Len,
    Noise, // unimplemented
} }

impl MathOp2 {
    pub const fn get_fn<'a>(self) -> fn(LVar<'a>, LVar<'a>) -> LVar<'a> {
        macro_rules! bop {
            ($op: tt) => {
                |a, b| LVar::from(((get_num!(a) as u64) $op (get_num!(b) as u64)) as f64)
            };
        }
        macro_rules! num {
            ($fn:expr) => {{
                |a, b| LVar::from($fn(get_num!(a), get_num!(b)))
            }};
        }
        macro_rules! op {
            ($op:tt) => {
                num!(|a,b| a $op b)
            }
        }
        match self {
            // we kind of interpret strings as numbers so yeah
            Self::Equal | Self::StrictEqual => |a, b| LVar::from(a == b),
            Self::NotEqual => |a, b| (a != b).into(),
            Self::And => num!(|a, b| a != 0.0 && b != 0.0),
            Self::Add => op!(+),
            Self::Sub => op!(-),
            Self::Mul => op!(*),
            Self::IDiv => bop!(/),
            Self::LessThan => op!(<),
            Self::LessThanEq => op!(<=),
            Self::GreaterThan => op!(>),
            Self::GreaterThanEq => op!(>=),
            Self::Div => op!(/),
            Self::Mod => op!(%),
            Self::Pow => num!(f64::powf),
            Self::ShiftLeft => bop!(<<),
            Self::ShiftRight => bop!(>>),
            Self::BitOr => bop!(|),
            Self::BitAnd => bop!(&),
            Self::ExclusiveOr => bop!(^),
            Self::Max => num!(f64::max),
            Self::Min => num!(f64::min),
            Self::AngleDiff => num!(|a, b| {
                let a = a % (360.0 * PI);
                let b = b % (360.0 * PI);
                f64::min(
                    if (a - b) < 0.0 { a - b + 360.0 } else { a - b },
                    if (b - a) < 0.0 { b - a + 360.0 } else { b - a },
                )
            }),
            Self::Len => num!(f64::hypot),
            Self::Noise => |_, _| LVar::Num(9.0),
            Self::Angle => num!(|a: f64, b: f64| {
                let mut x = a.atan2(b) * (180.0 / PI);
                if x < 0.0 {
                    x += 360.0;
                }
                x
            }),
        }
    }
}
