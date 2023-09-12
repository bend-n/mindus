use super::get_num;
use crate::{lexer::Token, memory::LVar};

super::op_enum! { pub enum MathOp1 {
    Floor,
    Not,
    Log,
    Abs,
    Rand,
    Ceil,
    Sqrt,
    Sin,
    Cos,
    Tan,
    ASin,
    ACos,
    ATan,
    Log10,
} }

macro_rules! num {
    ($fn: ident $c:expr) => {
        fn $fn(x: LVar<'_>) -> LVar<'_> {
            LVar::from($c(get_num!(x)))
        }
    };
}

macro_rules! flbop {
    ($f: expr, $fn: expr) => {
        $fn($f as u64) as f64
    };
}

num!(floor f64::floor);
fn not(x: LVar<'_>) -> LVar<'_> {
    match x {
        LVar::Num(n) => LVar::Num(flbop!(n, |n: u64| !n)),
        LVar::String(_) => LVar::null(),
    }
}
num!(log f64::ln);
num!(abs f64::abs);
const fn rand(_: LVar<'_>) -> LVar<'_> {
    LVar::Num(4.0)
}
num!(ceil f64::ceil);
num!(sqrt f64::sqrt);
num!(sin f64::sin);
num!(cos f64::cos);
num!(tan f64::tan);
num!(asin f64::asin);
num!(acos f64::acos);
num!(atan f64::atan);
num!(log10 f64::log10);

impl MathOp1 {
    pub const fn get_fn(self) -> for<'f> fn(LVar<'f>) -> LVar<'f> {
        match self {
            Self::Floor => floor,
            Self::Not => not,
            Self::Log => log,
            Self::Abs => abs,
            Self::Rand => rand,
            Self::Ceil => ceil,
            Self::Sqrt => sqrt,
            Self::Sin => sin,
            Self::Cos => cos,
            Self::Tan => tan,
            Self::ASin => asin,
            Self::ACos => acos,
            Self::ATan => atan,
            Self::Log10 => log10,
        }
    }
}
