use logos::Logos;
#[derive(Logos, Debug, PartialEq, Copy, Clone)]
#[logos(skip r"[ \t]+")]
pub enum Token<'strings> {
    // instructions
    #[token("getlink")]
    GetLink,
    #[token("read")]
    Read,
    #[token("write")]
    Write,
    #[token("set")]
    Set,
    #[token("op")]
    Op,
    #[token("end")]
    End,
    #[token("drawflush")]
    DrawFlush,
    #[token("print")]
    Print,
    #[token("packcolor")]
    PackColor,
    #[token("jump")]
    Jump,
    #[token("stop")]
    Stop,
    #[token("@counter")]
    Counter,
    #[token("equal")]
    Equal,
    #[token("notEqual")]
    NotEqual,
    #[token("lessThan")]
    LessThan,
    #[token("lessThanEq")]
    LessThanEq,
    #[token("greaterThan")]
    GreaterThan,
    #[token("greaterThanEq")]
    GreaterThanEq,
    #[token("strictEqual")]
    StrictEqual,
    #[token("always")]
    Always,
    #[token("add")]
    Add,
    #[token("sub")]
    Sub,
    #[token("mul")]
    Mul,
    #[token("div")]
    Div,
    #[token("idiv")]
    IDiv,
    #[token("mod")]
    Mod,
    #[token("pow")]
    Pow,
    #[token("land")]
    And,
    #[token("not")]
    Not,
    #[token("shl")]
    ShiftLeft,
    #[token("shr")]
    ShiftRight,
    #[token("or")]
    BitOr,
    #[token("and")]
    BitAnd,
    #[token("xor")]
    ExclusiveOr,
    #[token("max")]
    Max,
    #[token("min")]
    Min,
    #[token("angle")]
    Angle,
    #[token("angleDiff")]
    AngleDiff,
    #[token("len")]
    Len,
    #[token("noise")]
    Noise,
    #[token("abs")]
    Abs,
    #[token("log")]
    Log,
    #[token("log10")]
    Log10,
    #[token("floor")]
    Floor,
    #[token("ceil")]
    Ceil,
    #[token("sqrt")]
    Sqrt,
    #[token("rand")]
    Rand,
    #[token("sin")]
    Sin,
    #[token("cos")]
    Cos,
    #[token("tan")]
    Tan,
    #[token("asin")]
    ASin,
    #[token("acos")]
    ACos,
    #[token("atan")]
    ATan,

    // tokens
    #[token("\n")]
    Newline,
    #[regex("#[^\n]+")]
    Comment(&'strings str),
    #[regex(r"[0-9]+(\.[0-9]+)?", |lex| lex.slice().parse().ok())]
    #[regex(r"(true)|(false)", |lex| lex.slice().parse::<bool>().ok().map(f64::from))]
    #[regex(r#""[0-9]+(\.[0-9]+)?""#, |lex| lex.slice()[1..lex.slice().len()-1].parse().ok())]
    Num(f64),
    #[regex(r#""[^"]*""#, |lex| &lex.slice()[1..lex.slice().len()-1])]
    #[regex(r#"@.+"#, |lex| &lex.slice()[1..])]
    String(&'strings str),
    #[regex("[^0-9 \t\n]+")]
    Ident(&'strings str),
}

pub fn lex<'source>(s: &'source str) -> impl Iterator<Item = Token> {
    Token::lexer(s).filter_map(Result::ok)
}

pub fn print_stream<'s>(mut stream: impl Iterator<Item = Token<'s>>) {
    print!("[");
    let Some(tok) = stream.next() else {
        println!("]");
        return;
    };
    print!("{:?}", tok);
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
