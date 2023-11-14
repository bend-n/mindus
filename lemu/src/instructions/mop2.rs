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

macro_rules! num {
    ($fn:ident $closure:expr) => {
        fn $fn<'v>(a: &LVar<'v>, b: &LVar<'v>) -> f64 {
            f64::from($closure(get_num!(a), get_num!(b)))
        }
    };
}
macro_rules! op {
    ($fn:ident $op:tt) => {
        fn $fn<'v>(a: &LVar<'v>, b: &LVar<'v>) -> f64 {
            f64::from(get_num!(a) $op get_num!(b))
        }
    }
}
macro_rules! bop {
    ($fn: ident $op: tt) => {
        fn $fn<'v>(a: &LVar<'v>, b:& LVar<'v>) -> f64 {
            f64::from(((get_num!(a) as u64) $op (get_num!(b) as u64)) as f64)
        }
    };
}
macro_rules! nofun {
    ($fn:ident $closure:expr) => {
        fn $fn<'v>(a: &LVar<'v>, b: &LVar<'v>) -> f64 {
            f64::from($closure(a, b))
        }
    };
}
nofun!(eq | a, b | a == b);
nofun!(ne | a, b | a != b);
num!(and | a, b | a != 0.0 && b != 0.0);
#[rustfmt::skip]
op!(add +);
op!(sub -);
op!(mul *);
num!(idiv | a: f64 , b: f64 | (a / b).floor());
op!(lt <);
op!(le <=);
op!(gt >);
op!(ge >=);
op!(div /);
op!(rem %);
num!(pow f64::powf);
bop!(shl <<);
bop!(shr >>);
bop!(or |);
bop!(band &);
bop!(xor ^);
num!(max f64::max);
num!(min f64::min);
#[rustfmt::skip]
num!(angle_diff |a, b| {
    let a = a % (360.0 * PI);
    let b = b % (360.0 * PI);
    f64::min(
        if (a - b) < 0.0 { a - b + 360.0 } else { a - b },
        if (b - a) < 0.0 { b - a + 360.0 } else { b - a },
    )
});
num!(len f64::hypot);
nofun!(noise | _, _ | 9.0);
num!(angle |a: f64, b: f64| {
    let mut x = a.atan2(b) * (180.0 / PI);
    if x < 0.0 {
        x += 360.0;
    }
    x
});

super::op_impl!(MathOp2, ptr type = for<'f> fn(&LVar<'f>, &LVar<'f>) -> f64 {
    Equal => eq,
    StrictEqual => eq,
    NotEqual => ne,
    And => and,
    Add => add,
    Sub => sub,
    Mul => mul,
    IDiv => idiv,
    LessThan => lt,
    LessThanEq => le,
    GreaterThan => gt,
    GreaterThanEq => ge,
    Div => div,
    Mod => rem,
    Pow => pow,
    ShiftLeft => shl,
    ShiftRight => shr,
    BitOr => or,
    BitAnd => band,
    ExclusiveOr => xor,
    Max => max,
    Min => min,
    AngleDiff => angle_diff,
    Len => len,
    Noise => noise,
    Angle => angle,
});

// // no macro cuz funky rem
// impl MathOp2 {
//     pub const fn get_fn(self) -> for<'f> fn(&LVar<'f>, &LVar<'f>) -> f64 {
//         match self {
//             // we kind of interpret strings as numbers so yeah

//         }
//     }
// }
