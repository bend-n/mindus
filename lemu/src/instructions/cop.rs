use crate::{lexer::Token, memory::LVar};

super::op_enum! { pub enum ConditionOp {
    Equal,
    NotEqual,
    LessThan,
    LessThanEq,
    GreaterThan,
    GreaterThanEq,
    StrictEqual,
} }

impl ConditionOp {
    pub const fn get_fn<'v>(self) -> fn(LVar<'v>, LVar<'v>) -> bool {
        macro_rules! op {
            ($op:tt) => {
                |a, b| if let LVar::Num(a) = a && let LVar::Num(b) = b { a $op b } else { false }
            };
        }
        match self {
            Self::Equal | Self::StrictEqual => |a, b| a == b,
            Self::NotEqual => |a, b| a != b,
            Self::LessThan => op!(<),
            Self::GreaterThan => op!(>),
            Self::LessThanEq => op!(<=),
            Self::GreaterThanEq => op!(>=),
        }
    }
}
