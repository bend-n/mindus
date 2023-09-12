use std::io::Write as Wr;

use logos::Span;

use super::{
    executor::{ExecutorBuilderInternal, Instruction, UPInstr},
    instructions::{
        draw::{
            Clear, Flush, Line, RectBordered, RectFilled, SetColorConst, SetColorDyn, SetStroke,
            Triangle,
        },
        io::{Print, Read, Write},
        AlwaysJump, ConditionOp, DynJump, End, Instr, Jump, MathOp1, MathOp2, Op1, Op2, Set, Stop,
    },
    lexer::{Lexer, Token},
    memory::LAddress,
};

/// Errors returned when parsing fails.
#[derive(thiserror::Error, Debug)]
pub enum Error<'s> {
    /// Occurs from eg `set x`. (needs a value to set to)
    #[error("unexpected end of stream")]
    UnexpectedEof,
    /// Occurs from eg `op add\n...` (needs a variable)
    #[error("expected variable, got {0:?}")]
    ExpectedVar(Token<'s>, Span),
    /// Occurs from eg `draw 4` (needs a ident of the type of drawing)
    #[error("expected identifier, got {0:?}")]
    ExpectedIdent(Token<'s>, Span),
    /// Occurs from eg `jump house` (assuming house isnt a label).
    #[error("expected jump target, got {0:?}")]
    ExpectedJump(Token<'s>, Span),
    /// Occurs from eg `op add "three" "four"`
    #[error("expected number, got {0:?}")]
    ExpectedNum(Token<'s>, Span),
    /// Occurs from eg `op 4` (4 is not add/mul/...)
    #[error("expected operator, got {0:?}")]
    ExpectedOp(Token<'s>, Span),
    /// Occurs from eg `write cell1 5.5` (5.5 is not int)
    #[error("expected integer, got {0:?}")]
    ExpectedInt(Token<'s>, Span),
    /// Occurs from eg `4.0 add 5.0`
    #[error("expected instruction, got {0:?}")]
    ExpectedInstr(Token<'s>, Span),
    /// Occurs from eg
    /// ```text
    /// lable:
    ///     jump label always
    /// ```
    /// (typo: lable not label)
    #[error("unable to find label {0:?}")]
    LabelNotFound(&'s str, Span),
    /// Occurs from eg `jump 4910294029 always`
    #[error("unable to jump to instruction {0:?}")]
    InvalidJump(Instruction, Span),
    /// Occurs from eg `read bank9223372036854775807 5` (only `126` banks can exist)
    #[error("cannot get cell>{0:?}")]
    MemoryTooFar(usize, Span),
    /// Occurs from `read register1`
    #[error("unknown memory type {0:?}, expected (cell)|(bank)")]
    InvalidMemoryType(&'s str, Span),
    /// Occurs from `drawflush bank1`
    #[error("unknown display type {0:?}, expected 'display'")]
    InvalidDisplayType(&'s str, Span),
    /// Occurs from `draw house` (or `draw image`, a valid but unsupported instruction here)
    #[error("unknown image operation {0:?}")]
    UnsupportedImageOp(&'s str, Span),
    #[error("couldnt get display #{0:?}.")]
    /// Occurs from eg `display 50`.
    ///
    /// call `display` 50 more times to have more display options:
    /// ```rust,ignore
    /// executor
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display()
    ///     .display();
    /// ```
    NoDisplay(usize, Span),
}

macro_rules! tokstr {
    ($tok:expr) => {
        match $tok {
            Token::Ident(i) => Some(i),
            Token::GetLink => Some("getlink"),
            Token::Read => Some("read"),
            Token::Write => Some("write"),
            Token::Set => Some("set"),
            Token::Op => Some("op"),
            Token::End => Some("end"),
            Token::Draw => Some("draw"),
            Token::DrawFlush => Some("drawflush"),
            Token::Print => Some("print"),
            Token::PackColor => Some("packcolor"),
            Token::Jump => Some("jump"),
            Token::Stop => Some("stop"),
            Token::Counter => Some("@counter"),
            Token::Equal => Some("equal"),
            Token::NotEqual => Some("notEqual"),
            Token::LessThan => Some("lessThan"),
            Token::LessThanEq => Some("lessThanEq"),
            Token::GreaterThan => Some("greaterThan"),
            Token::GreaterThanEq => Some("greaterThanEq"),
            Token::StrictEqual => Some("strictEqual"),
            Token::Always => Some("always"),
            Token::Add => Some("add"),
            Token::Sub => Some("sub"),
            Token::Mul => Some("mul"),
            Token::Div => Some("div"),
            Token::IDiv => Some("idiv"),
            Token::Mod => Some("mod"),
            Token::Pow => Some("pow"),
            Token::And => Some("land"),
            Token::Not => Some("not"),
            Token::ShiftLeft => Some("shl"),
            Token::ShiftRight => Some("shr"),
            Token::BitOr => Some("or"),
            Token::BitAnd => Some("and"),
            Token::ExclusiveOr => Some("xor"),
            Token::Max => Some("max"),
            Token::Min => Some("min"),
            Token::Angle => Some("angle"),
            Token::AngleDiff => Some("angleDiff"),
            Token::Len => Some("len"),
            Token::Noise => Some("noise"),
            Token::Abs => Some("abs"),
            Token::Log => Some("log"),
            Token::Log10 => Some("log10"),
            Token::Floor => Some("floor"),
            Token::Ceil => Some("ceil"),
            Token::Sqrt => Some("sqrt"),
            Token::Rand => Some("rand"),
            Token::Sin => Some("sin"),
            Token::Cos => Some("cos"),
            Token::Tan => Some("tan"),
            Token::ASin => Some("asin"),
            Token::ACos => Some("acos"),
            Token::ATan => Some("atan"),
            _ => None,
        }
    };
}

impl Error<'_> {
    /// Produces a [`Diagnostic`] from this error.
    #[cfg(feature = "diagnose")]
    pub fn diagnose<'s>(
        &self,
        source: &'s str,
        fname: Option<&'s str>,
    ) -> yumy::Diagnostic<yumy::Source<'s>> {
        use yumy::{
            owo_colors::{OwoColorize, Style},
            Diagnostic, Label, Source, SourceSpan,
        };

        let error = "error".red();
        let note = "note".yellow();
        let help = "help".bright_green();
        let e_sty = Style::new().bright_red();
        macro_rules! err {
            ($span:expr, $msg:literal $(, $args:expr)* $(,)?) => {
                Label::styled(SourceSpan::new($span.start as u32, $span.end as u32), format!($msg $(, $args)*), e_sty)
            };
        }
        macro_rules! dig {
            ($ms:literal $(, $args:expr)* $(,)?) => {
                Diagnostic::new(format!($ms $(, $args)*)).with_source(Source::new(source, fname))
            };
        }
        let mut d;
        match self {
            Error::UnexpectedEof => {
                d = dig!("{error}: wasnt able to finish read").with_label(err!(
                    source.len() - 1..source.len() - 1,
                    "there was supposed to be another token here"
                ));
            }
            Error::ExpectedVar(_, s) => {
                d = dig!("{error}: expected a variable")
                    .with_label(err!(s, "this was supposed to be a variable"));
            }
            Error::ExpectedIdent(_, s) => {
                d = dig!("{error}: expected a identifier")
                    .with_label(err!(s, "this was supposed to be a identifier"));
            }
            Error::ExpectedJump(t, s) => {
                d = dig!("{error}: expected jump target")
                    .with_label(err!(s, "this was supposed to be a jump target"))
                    .with_footnote(
                        format!("{note}: a jump target is a label(ident), or a line number in integer form (not a float)"),
                    );
                if let Token::Num(n) = t {
                    d.add_footnote(format!("{help}: remove the fractional part: {n:.0}"));
                }
            }
            Error::ExpectedNum(_, s) => {
                d = dig!("{error}: expected number")
                    .with_label(err!(s, "this was supposed to be a number"));
            }
            Error::ExpectedOp(t, s) => {
                d = dig!("{error}: expected operator")
                    .with_label(err!(s, "this was supposed to be a operator"));
                if let Some(i) = tokstr!(*t) && let Some((mat,score)) = rust_fuzzy_search::fuzzy_search_best_n(i, crate::instructions::OPS, 1).first() && *score > 0.5 {
                    d.add_footnote(format!("{help}: maybe you meant {mat}"));
                }
            }
            Error::ExpectedInt(t, s) => {
                d = dig!("{error}: expected integer")
                    .with_label(err!(s, "this was supposed to be a integer"));
                if let Token::Num(n) = t {
                    d.add_footnote(format!("{help}: remove the fractional part: {n:.0}"));
                }
            }
            Error::ExpectedInstr(_, s) => {
                d = dig!("{error}: expected instruction")
                    .with_label(err!(s, "this was supposed to be a instruction"));
                // it occurs to me that this wont ever be a string, as idents are turned into `Code`
                // if let Some(i) = tokstr!(t.clone()) && let Some((mat,score)) = rust_fuzzy_search::fuzzy_search_best_n(i, crate::instructions::INSTRS, 1).get(0) && *score > 0.5 {
                //     d.add_footnote(format!("{help}: maybe you meant {mat}"));
                // }
            }
            Error::LabelNotFound(_, s) => {
                d = dig!("{error}: label not found")
                    .with_label(err!(s, "this was supposed to be a (existing) label"));
            }
            Error::InvalidJump(Instruction(target), s) => {
                d = dig!("{error}: invalid jump")
                    .with_label(err!(s, "line#{target} is not in the program"))
                    .with_footnote(format!(
                        "{help}: there are 0..{} available lines",
                        source.lines().count()
                    ));
            }
            Error::MemoryTooFar(b, s) => {
                d = dig!("{error}: invalid memory cell/bank")
                    .with_label(err!(s, "cant get cell/bank#{b}"))
                    .with_footnote(format!("{note}: only 126 cells/banks are allowed"));
            }
            Error::InvalidMemoryType(t, s) => {
                d = dig!("{error}: invalid memory type")
                    .with_label(err!(s, "cant get {t}"))
                    .with_footnote(format!("{note}: only banks/cells are allowed"));
            }
            Error::InvalidDisplayType(disp, s) => {
                d = dig!("{error}: invalid display type")
                    .with_label(err!(s, "cant get {disp}"))
                    .with_footnote(format!("{help}: change this to 'display'"));
            }
            Error::UnsupportedImageOp(op, s) => {
                d = dig!("{error}: invalid image op").with_label(err!(
                    s,
                    "must be one of {{clear, color, col, stroke, line, rect, lineRect, triangle}}"
                ));
                if let Some((mat,score)) = rust_fuzzy_search::fuzzy_search_best_n(op, crate::instructions::draw::INSTRS, 1).first() && *score > 0.5 {
                    d.add_footnote(format!("{help}: you may have meant {mat}"));
                }
            }
            Error::NoDisplay(disp, s) => {
                d = dig!("{error}: no display allocated").with_label(err!(s, "display#{disp} has not been created")).with_footnote(format!("{note}: it is impossible for me to dynamically allocate displays, as 'display1' could be large or small"));
            }
        };
        d
    }
}

#[derive(Debug)]
enum UJump<'v> {
    Sometimes {
        a: LAddress<'v>,
        b: LAddress<'v>,
        op: ConditionOp,
    },
    Always,
}

pub fn parse<'source, W: Wr>(
    mut tokens: Lexer<'source>,
    executor: &mut ExecutorBuilderInternal<'source, W>,
) -> Result<(), Error<'source>> {
    let mut mem = Vec::new(); // maps &str to usize
                              // maps "start" to 0
    let mut labels = Vec::new();
    let mut unfinished_jumps = Vec::new();
    macro_rules! tok {
        () => {
            tokens.next().ok_or(Error::UnexpectedEof)
        };
    }
    macro_rules! err {
        ($e:ident($($stuff:expr)+)) => {
            Error::$e($($stuff,)+ tokens.span())
        }
    }
    macro_rules! yeet {
        ($e:ident($($stuff:expr)+)) => {
            return Err(Error::$e($($stuff,)+ tokens.span()))
        };
    }
    #[rustfmt::skip]
    macro_rules! nextline {
        () => {
            while let Some(tok) = tokens.next() && tok != Token::Newline { }
        };
    }
    macro_rules! take_int {
        ($tok:expr) => {
            match $tok {
                Token::Num(n) if n.fract() == 0.0 && n >= 0.0 => Ok(n as usize),
                t => Err(err!(ExpectedInt(t))),
            }
        };
    }
    macro_rules! take_memory {
        () => {{
            let container = take_ident!(tok!()?)?;
            let cell_n = take_int!(tok!()?)?;
            if cell_n > 126 || cell_n == 0 {
                yeet!(MemoryTooFar(cell_n));
            }
            match container {
                "bank" => executor.bank(cell_n),
                "cell" => executor.cell(cell_n),
                _ => yeet!(InvalidMemoryType(container)),
            }
        }};
    }
    macro_rules! addr {
        ($n:expr) => {{
            let n = $n;
            match mem
                .iter()
                .enumerate()
                .find(|(_, &v)| v == n)
                .map(|(i, _)| i)
            {
                // SAFETY: we tell it the size is mem.len(); i comes from mem, this is fine
                Some(i) => unsafe { LAddress::addr(i) },
                None => {
                    mem.push(n);
                    // SAFETY: see above
                    unsafe { LAddress::addr(mem.len() - 1) }
                }
            }
        }};
    }
    macro_rules! take_ident {
        ($tok:expr) => {{
            let tok = $tok;
            tokstr!(tok).ok_or(err!(ExpectedIdent(tok)))
        }};
    }
    macro_rules! take_var {
        ($tok:expr) => {{
            let tok = $tok;
            if let Some(i) = tokstr!(tok) {
                Ok(addr!(i))
            } else {
                match tok {
                    Token::Num(n) => Ok(LAddress::cnst(n)),
                    Token::String(s) => Ok(LAddress::cnst(s)),
                    t => Err(err!(ExpectedVar(t))),
                }
            }
        }};
    }
    macro_rules! take_numvar {
        ($tok:expr) => {{
            let tok = $tok;
            if let Some(i) = tokstr!(tok) {
                Ok(addr!(i))
            } else {
                match tok {
                    Token::Num(n) => Ok(LAddress::cnst(n)),
                    t => Err(err!(ExpectedNum(t))),
                }
            }
        }};
    }
    while let Some(token) = tokens.next() {
        match token {
            // # omg
            Token::Comment(_) => {
                executor.noop();
            }
            // label:
            Token::Ident(v) if v.ends_with(':') => {
                labels.push((&v[..v.len() - 1], executor.next()));
            }
            // print "5"
            Token::Print => {
                let val = take_var!(tok!()?)?;
                executor.add(Print { val });
            }
            // set x 4
            Token::Set => {
                let from = tok!()?;
                if from == Token::Counter {
                    let to = take_numvar!(tok!()?)?;
                    executor.add(DynJump { to, proglen: 0 });
                } else {
                    let from = addr!(take_ident!(from)?);
                    let to = take_var!(tok!()?)?;
                    executor.add(Set { from, to });
                }
            }
            // stop
            Token::Stop => {
                executor.add(Stop {});
            }
            // jump start equal a b
            Token::Jump => {
                let tok = tok!()?;
                // label jump
                if let Some(i) = tokstr!(tok) {
                    let op = tok!()?;
                    if op == Token::Always {
                        executor.jmp();
                        unfinished_jumps.push((UJump::Always, i, executor.last()));
                    } else {
                        let op = op.try_into().map_err(|()| err!(ExpectedOp(op)))?;
                        let a = take_var!(tok!()?)?;
                        let b = take_var!(tok!()?)?;
                        executor.jmp();
                        unfinished_jumps.push((UJump::Sometimes { a, b, op }, i, executor.last()));
                    }
                } else if let Ok(n) = take_int!(tok) {
                    let to = Instruction(n);
                    let op = tok!()?;
                    if op == Token::Always {
                        executor.add(AlwaysJump { to });
                    } else {
                        let op = op.try_into().map_err(|()| err!(ExpectedOp(op)))?;
                        let a = take_var!(tok!()?)?;
                        let b = take_var!(tok!()?)?;
                        executor.add(Jump::new(op, to, a, b));
                    }
                } else {
                    yeet!(ExpectedJump(tok));
                };
            }
            // op add c 1 2
            Token::Op => {
                let op = tok!()?;
                if let Ok(op) = MathOp1::try_from(op) {
                    // assigning to a var is useless but legal
                    let out = take_numvar!(tok!()?)?;
                    let x = take_numvar!(tok!()?)?;
                    executor.add(Op1::new(op, x, out));
                } else if let Ok(op) = MathOp2::try_from(op) {
                    let out = take_numvar!(tok!()?)?;
                    let a = take_numvar!(tok!()?)?;
                    let b = take_numvar!(tok!()?)?;
                    executor.add(Op2::new(op, a, b, out));
                } else {
                    yeet!(ExpectedOp(op));
                }
            }
            // write 5.0 bank1 4 (aka bank1[4] = 5.0)
            Token::Write => {
                let set = take_numvar!(tok!()?)?;
                let container = take_memory!();
                let index = container.limit(take_int!(tok!()?)?);

                executor.add(Write {
                    index,
                    set,
                    container,
                });
            }
            // read result cell1 4 (aka result = cell1[4])
            Token::Read => {
                let output = take_var!(tok!()?)?;
                let container = take_memory!();
                let index = container.limit(take_int!(tok!()?)?);
                executor.add(Read {
                    index,
                    output,
                    container,
                });
            }
            Token::Draw => {
                let dty = tok!()?;
                let Token::Ident(instr) = dty else {
                    yeet!(ExpectedIdent(dty));
                };
                #[rustfmt::skip]
                macro_rules! four { ($a:expr) => { ($a, $a, $a, $a) }; }
                #[rustfmt::skip]
                macro_rules! six { ($a:expr) => { ($a, $a, $a, $a, $a, $a) }; }
                match instr {
                    "clear" => {
                        let (r, g, b, a) = four! { take_numvar!(tok!()?)? };
                        executor.draw(Clear { r, g, b, a });
                    }
                    "color" => {
                        let (r, g, b, a) = four! { take_numvar!(tok!()?)? };
                        executor.draw(SetColorDyn { r, g, b, a });
                    }
                    "col" => {
                        let col = take_int!(tok!()?)?;
                        let r = (col & 0xff00_0000 >> 24) as u8;
                        let g = (col & 0x00ff_0000 >> 16) as u8;
                        let b = (col & 0x0000_ff00 >> 8) as u8;
                        let a = (col & 0x0000_00ff) as u8;
                        executor.draw(SetColorConst { r, g, b, a });
                    }
                    "stroke" => {
                        let size = take_numvar!(tok!()?)?;
                        executor.draw(SetStroke { size });
                    }
                    "line" => {
                        let (x, y, x2, y2) = four! { take_numvar!(tok!()?)? };
                        executor.draw(Line {
                            point_a: (x, y),
                            point_b: (x2, y2),
                        });
                    }
                    "rect" => {
                        let (x, y, width, height) = four! { take_numvar!(tok!()?)? };
                        executor.draw(RectFilled {
                            position: (x, y),
                            width,
                            height,
                        });
                    }
                    "lineRect" => {
                        let (x, y, width, height) = four! { take_numvar!(tok!()?)? };
                        executor.draw(RectBordered {
                            position: (x, y),
                            width,
                            height,
                        });
                    }
                    "triangle" => {
                        let (x, y, x2, y2, x3, y3) = six! { take_numvar!(tok!()?)? };
                        executor.draw(Triangle {
                            points: ((x, y), (x2, y2), (x3, y3)),
                        });
                    }
                    // poly is TODO, image is WONTFIX
                    i => yeet!(UnsupportedImageOp(i)),
                }
            }
            Token::DrawFlush => {
                let screen = take_ident!(tok!()?)?;
                if screen != "display" {
                    yeet!(InvalidDisplayType(screen));
                }
                let display = executor
                    .display(take_int!(tok!()?)?)
                    .map_err(|n| err!(NoDisplay(n)))?;
                executor.add(Flush { display });
            }
            // end
            Token::End => {
                executor.add(End {});
            }
            // starting newline, simply skip. continue, so as not to to trigger the nextline!()
            Token::Newline => continue,
            // unknown instruction
            Token::Ident(i) => {
                let mut c = String::from(i);
                while let Some(tok) = tokens.next() && tok != Token::Newline {
                    use std::fmt::Write;
                    write!(c, " {tok}").expect("didnt know writing to a string could fail");
                }
                executor.code(c);
                // we take the newline here
                continue;
            }
            t => yeet!(ExpectedInstr(t)),
        }
        nextline!();
    }

    for (j, l, Instruction(i)) in unfinished_jumps {
        let to = labels
            .iter()
            .find(|(v, _)| v == &l)
            .ok_or_else(|| err!(LabelNotFound(l)))?
            .1;
        executor.program[i] = UPInstr::Instr(match j {
            UJump::Always => Instr::from(AlwaysJump { to }),
            UJump::Sometimes { a, b, op } => Instr::from(Jump::new(op, to, a, b)),
        });
    }

    // check jump validity
    for i in &executor.program {
        if let UPInstr::Instr(Instr::Jump(Jump { to, .. }) | Instr::AlwaysJump(AlwaysJump { to })) =
            i
        {
            if !executor.valid(*to) {
                yeet!(InvalidJump(*to));
            }
        }
    }

    // set dynjumps
    let len = executor.program.len();
    for i in &mut executor.program {
        if let UPInstr::Instr(Instr::DynJump(DynJump { proglen, .. })) = i {
            *proglen = len;
        }
    }

    executor.mem(mem.len());

    Ok(())
}
