use logos::{Lexer as RealLexer, Logos, Span};
use std::fmt::Write;
macro_rules! instrs {
    ($($z:literal => $v:ident,)+) => {
        #[derive(Logos, Debug, PartialEq, Copy, Clone)]
        #[logos(skip r"[ \t]+")]
        pub enum Token<'strings> {
            #[token("\n")]
            Newline,
            #[regex("#[^\n]+")]
            Comment(&'strings str),
            #[regex(r"[0-9]+(\.[0-9]+)?", |lex| lex.slice().parse().ok())]
            #[regex(r"(true)|(false)", |lex| lex.slice().parse::<bool>().ok().map(f64::from))]
            #[regex(r#""[0-9]+(\.[0-9]+)?""#, |lex| lex.slice()[1..lex.slice().len()-1].parse().ok())]
            Num(f64),
            #[regex(r#""[^"]*""#, |lex| &lex.slice()[1..lex.slice().len()-1])]
            #[regex(r#"@[^ "\n]*"#, |lex| &lex.slice()[1..])]
            String(&'strings str),
            #[regex("[^0-9 \t\n]+")]
            Ident(&'strings str),

            $(#[token($z)] $v,)+
        }

        impl<'v> Token<'v> {
            pub fn write(self, w: &mut impl Write) -> std::fmt::Result {
                match self {
                    $(Self::$v => write!(w, $z,),)+
                    Self::String(s) | Self::Ident(s)| Self::Comment(s) => write!(w, "{s}"),
                    Self::Num(n) => write!(w, "{n}"),
                    Self::Newline => write!(w, "\n"),
                }?;
                Ok(())
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
            $(assert_eq!(lex.next(), Some(Token::$tok$(($var))?));)+
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
        Num(4.0),
    ];
}
