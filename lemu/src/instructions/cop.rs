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

macro_rules! op {
    ($name: ident $op:tt ) => {
        fn $name<'v>(a: LVar<'v>, b: LVar<'v>) -> bool {
            if let LVar::Num(a) = a && let LVar::Num(b) = b { a $op b } else { false }
        }
    };
}

fn eq<'v>(a: LVar<'v>, b: LVar<'v>) -> bool {
    a == b
}
fn ne<'v>(a: LVar<'v>, b: LVar<'v>) -> bool {
    a != b
}
op!(lt <);
op!(gt >);
op!(le <=);
op!(ge >=);

impl ConditionOp {
    pub const fn get_fn(self) -> for<'f> fn(LVar<'f>, LVar<'f>) -> bool {
        match self {
            Self::Equal | Self::StrictEqual => eq,
            Self::NotEqual => ne,
            Self::LessThan => lt,
            Self::GreaterThan => gt,
            Self::LessThanEq => le,
            Self::GreaterThanEq => ge,
        }
    }
}
