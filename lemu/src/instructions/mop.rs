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
        fn $fn(x: &LVar<'_>) -> f64 {
            f64::from($c(get_num!(x)))
        }
    };
    (deg $fn: ident $c:expr) => {
        fn $fn(x: &LVar<'_>) -> f64 {
            f64::from($c(get_num!(x).to_radians()))
        }
    };
    (to_deg $fn: ident $c:expr) => {
        fn $fn(x: &LVar<'_>) -> f64 {
            f64::from($c(get_num!(x))).to_radians()
        }
    };
}

macro_rules! flbop {
    ($f: expr, $fn: expr) => {
        $fn($f as u64) as f64
    };
}

num!(floor f64::floor);
fn not(x: &LVar<'_>) -> f64 {
    flbop!(get_num!(x), |n: u64| !n)
}
num!(log f64::ln);
num!(abs f64::abs);
const fn rand(_: &LVar<'_>) -> f64 {
    4.0
}
num!(ceil f64::ceil);
num!(sqrt f64::sqrt);
num!(deg sin f64::sin);
num!(deg cos f64::cos);
num!(deg tan f64::tan);
num!(to_deg asin f64::asin);
num!(to_deg acos f64::acos);
num!(to_deg atan f64::atan);
num!(log10 f64::log10);

super::op_impl!(MathOp1, ptr type = for<'v> fn(&LVar<'v>) -> f64 {
    Floor => floor,
    Not => not,
    Log => log,
    Abs => abs,
    Rand => rand,
    Ceil => ceil,
    Sqrt => sqrt,
    Sin => sin,
    Cos => cos,
    Tan => tan,
    ASin => asin,
    ACos => acos,
    ATan => atan,
    Log10 => log10,
});
