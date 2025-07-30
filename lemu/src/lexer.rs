use beef::lean::Cow;
use logos::{Lexer as RealLexer, Logos, Span};

macro_rules! instrs {
    ($($z:literal => $v:ident,)+) => {
        #[derive(Logos, Debug, PartialEq, Clone)]
        #[logos(skip r"[ \t]+")]
        pub enum Token<'strings> {
            #[token("\n")]
            #[token(";")]
            Newline,
            #[regex("#[^\n]+", priority = 8)]
            Comment(&'strings str),
            #[regex(r"-?[0-9]+(\.[0-9]+)?", |lex| lex.slice().parse().ok())]
            #[regex(r"(true)|(false)", |lex| lex.slice().parse::<bool>().ok().map(f64::from), priority = 10)]
            #[regex(r#"0[xX][0-9a-fA-F]+"#, |lex| u64::from_str_radix(&lex.slice()[2..], 16).map(|v| v as f64).ok())]
            #[regex(r#"0[bB][01]+"#, |lex| u64::from_str_radix(&lex.slice()[2..], 2).map(|v| v as f64).ok())]
            #[regex(r#""[0-9]+(\.[0-9]+)?""#, callback = |lex| lex.slice()[1..lex.slice().len()-1].parse().ok(), priority = 13)]
            Num(f64), // TODO have bool and integer tokens, and parser converts
            #[regex(r#""([^\\"\n])*""#, callback = |lex| Cow::from(&lex.slice()[1..lex.slice().len()-1]), priority = 12)]
            #[regex(r#"@[^ "\n]*"#, |lex| Cow::from(lex.slice()))]
            #[regex(r#""[^"]*""#, callback = |lex| Cow::from(lex.slice()[1..lex.slice().len()-1].replace(r"\n", "\n")), priority = 8)]
            String(Cow<'strings, str>),
            #[regex("[^@0-9- \t\n][^ \t\n]*", priority = 7)]
            Ident(&'strings str),

            $(#[token($z, priority = 8)] $v,)+
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
    "null" => Null,
    "read" => Read,
    "write" => Write,
    "set" => Set,
    "op" => Op,
    "end" => End,
    "packcolor" => PackColor,
    "drawflush" => DrawFlush,
    "draw" => Draw,
    "select" => Select,
    "print" => Print,
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

pub fn lex(s: &str) -> Lexer<'_> {
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
pub fn print_stream(mut stream: Lexer) {
    print!("[");
    let Some(tok) = stream.next() else {
        println!("]");
        return;
    };
    print!("{tok:?}");
    while let Some(tok) = stream.next() {
        print!(", {tok:?}");
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
