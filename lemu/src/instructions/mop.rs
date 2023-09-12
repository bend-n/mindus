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
}}

impl MathOp1 {
    pub const fn get_fn(self) -> fn(LVar<'_>) -> LVar<'_> {
        macro_rules! num {
            ($c:expr) => {
                |v| LVar::from($c(get_num!(v)))
            };
        }
        macro_rules! flbop {
            ($f: expr, $fn: expr) => {
                $fn($f as u64) as f64
            };
        }
        match self {
            Self::Floor => num!(f64::floor),
            Self::Not => |v| match v {
                LVar::Num(n) => LVar::Num(flbop!(n, |n: u64| !n)),
                _ => LVar::null(),
            },
            Self::Log => num!(f64::ln),
            Self::Abs => num!(f64::abs),
            Self::Rand => |_| LVar::Num(4.0),
            Self::Ceil => num!(f64::ceil),
            Self::Sqrt => num!(f64::sqrt),
            Self::Sin => num!(f64::sin),
            Self::Cos => num!(f64::cos),
            Self::Tan => num!(f64::tan),
            Self::ASin => num!(f64::asin),
            Self::ACos => num!(f64::acos),
            Self::ATan => num!(f64::atan),
            Self::Log10 => num!(f64::log10),
        }
    }
}
