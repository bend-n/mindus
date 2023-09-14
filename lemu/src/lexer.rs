use beef::lean::Cow;
use logos::{Lexer as RealLexer, Logos, Span};

macro_rules! instrs {
    ($($z:literal => $v:ident,)+) => {
        #[derive(Logos, Debug, PartialEq, Clone)]
        #[logos(skip r"[ \t]+")]
        pub enum Token<'strings> {
            #[token("\n")]
            Newline,
            #[regex("#[^\n]+")]
            Comment(&'strings str),
            #[regex(r"[0-9]+(\.[0-9]+)?", |lex| lex.slice().parse().ok())]
            #[regex(r"(true)|(false)", |lex| lex.slice().parse::<bool>().ok().map(f64::from))]
            #[regex(r#"0[xX][0-9a-fA-F]+"#, |lex| u64::from_str_radix(&lex.slice()[2..], 16).map(|v| v as f64).ok())]
            #[regex(r#"0[bB][01]+"#, |lex| u64::from_str_radix(&lex.slice()[2..], 2).map(|v| v as f64).ok())]
            #[regex(r#""[0-9]+(\.[0-9]+)?""#, callback = |lex| lex.slice()[1..lex.slice().len()-1].parse().ok(), priority = 6)]
            Num(f64),
            #[regex(r#""([^\\"\n])*""#, callback = |lex| Cow::from(&lex.slice()[1..lex.slice().len()-1]), priority = 5)]
            #[regex(r#"@[^ "\n]*"#, |lex| Cow::from(&lex.slice()[1..]))]
            #[regex(r#""[^"]*""#, |lex| Cow::from(lex.slice()[1..lex.slice().len()-1].replace(r"\n", "\n")))]
            String(Cow<'strings, str>),
            #[regex("[^0-9 \t\n]+")]
            Ident(&'strings str),

            $(#[token($z)] $v,)+
        }

        impl std::fmt::Display for Token<'_> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
                match self {
                    $(Self::$v => write!(f, $z,),)+
                    Self::Ident(s)| Self::Comment(s) => write!(f, "{s}"),
                    Self::String(s) => write!(f, "{s}"),
                    Self::Num(n) => write!(f, "{n}"),
                    Self::Newline => write!(f, "\n"),
                }
            }
        }
    }
}

instrs! {
    "getlink" => GetLink,
    "read" => Read,
    "write" => Write,
    "set" => Set,
    "op" => Op,
    "end" => End,
    "drawflush" => DrawFlush,
    "draw" => Draw,
    "print" => Print,
    "packcolor" => PackColor,
    "jump" => Jump,
    "stop" => Stop,
    "@counter" => Counter,
    "equal" => Equal,
    "notEqual" => NotEqual,
    "lessThan" => LessThan,
    "lessThanEq" => LessThanEq,
    "greaterThan" => GreaterThan,
    "greaterThanEq" => GreaterThanEq,
    "strictEqual" => StrictEqual,
    "always" => Always,
    "add" => Add,
    "sub" => Sub,
    "mul" => Mul,
    "div" => Div,
    "idiv" => IDiv,
    "mod" => Mod,
    "pow" => Pow,
    "land" => And,
    "not" => Not,
    "shl" => ShiftLeft,
    "shr" => ShiftRight,
    "or" => BitOr,
    "and" => BitAnd,
    "xor" => ExclusiveOr,
    "max" => Max,
    "min" => Min,
    "angle" => Angle,
    "angleDiff" => AngleDiff,
    "len" => Len,
    "noise" => Noise,
    "abs" => Abs,
    "log" => Log,
    "log10" => Log10,
    "floor" => Floor,
    "ceil" => Ceil,
    "sqrt" => Sqrt,
    "rand" => Rand,
    "sin" => Sin,
    "cos" => Cos,
    "tan" => Tan,
    "asin" => ASin,
    "acos" => ACos,
    "atan" => ATan,
}

pub fn lex(s: &str) -> Lexer {
    Lexer {
        inner: Token::lexer(s),
    }
}

pub struct Lexer<'s> {
    inner: RealLexer<'s, Token<'s>>,
}

impl<'s> Lexer<'s> {
    pub fn next(&mut self) -> Option<Token<'s>> {
        self.inner.find_map(Result::ok)
    }

    pub fn span(&self) -> Span {
        self.inner.span()
    }
}

#[allow(dead_code)]
pub fn print_stream<'s>(mut stream: impl Iterator<Item = Token<'s>>) {
    print!("[");
    let Some(tok) = stream.next() else {
        println!("]");
        return;
    };
    print!("{tok:?}");
    for token in stream {
        print!(", {token:?}");
    }
    println!("]");
}

#[test]
fn lexer() {
    let mut lex = lex(r#"
    start:
        print "xd"
        jump start always
        set x "4""#);
    macro_rules! test {
        ($($tok:ident$(($var:literal))?),+ $(,)?) => {{
            $(assert_eq!(lex.next(), Some(Token::$tok$(($var.into()))?));)+
            assert_eq!(lex.next(), None);
        }}
    }
    test![
        Newline,
        Ident("start:"),
        Newline,
        Print,
        String("xd"),
        Newline,
        Jump,
        Ident("start"),
        Always,
        Newline,
        Set,
        Ident("x"),
        Num(4),
    ];
}
