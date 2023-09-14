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
    memory::{LAddress, LVar},
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
    #[error("unable to find label {0}")]
    LabelNotFound(&'s str, Span),
    /// Occurs from eg `jump 4910294029 always`
    #[error("unable to jump to instruction {0:?}")]
    InvalidJump(Instruction, Span),
    /// Occurs from eg `read bank9223372036854775807 5` (only `126` banks can exist)
    #[error("cannot get cell>{0}")]
    MemoryTooFar(usize, Span),
    /// Occurs from eg `read bank1 512`
    #[error("index {0} out of bounds ({1} max)")]
    IndexOutOfBounds(usize, usize, Span),
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
    /// Produces a [`Error`](lerr::Error) from this error.
    #[cfg(feature = "diagnose")]
    pub fn diagnose<'s>(&self, source: &'s str) -> lerr::Error<'s> {
        use lerr::Error;

        let error = "[1;34;31merror[0m";
        let note = "[1;34;34mnote[0m";
        let help = "[1;34;32mhelp[0m";
        macro_rules! err {
            ($span:expr, $msg:literal $(, $args:expr)* $(,)?) => {
                (($span.clone(), format!($msg $(, $args)*)))
            };
        }
        let mut e = Error::new(source);
        macro_rules! msg {
            ($ms:literal $(, $args:expr)* $(,)?) => {
                e.message(format!($ms $(, $args)*))
            };
        }
        match self {
            Self::UnexpectedEof => {
                msg!("{error}: wasnt able to finish read").label(err!(
                    source.len()..source.len(),
                    "there was supposed to be another token here"
                ));
            }
            Self::ExpectedVar(_, s) => {
                msg!("{error}: expected variable")
                    .label(err!(s, "this was supposed to be a variable"));
            }
            Self::ExpectedIdent(_, s) => {
                msg!("{error}: expected identifier")
                    .label(err!(s, "this was supposed to be a identifier"));
            }
            Self::ExpectedJump(t, s) => {
                msg!("{error}: expected jump target")
                    .label(err!(s, "this was supposed to be a jump target"))
                    .note(
                        format!("{note}: a jump target is a label(ident), or a line number in integer form (not a float)"),
                    );
                if let Token::Num(n) = t {
                    e.note(format!("{help}: remove the fractional part: {n:.0}"));
                }
            }
            Self::ExpectedNum(_, s) => {
                msg!("{error}: expected number").label(err!(s, "this was supposed to be a number"));
            }
            Self::ExpectedOp(t, s) => {
                msg!("{error}: expected operator")
                    .label(err!(s, "this was supposed to be a operator"));
                if let Some(i) = tokstr!(*t) && let Some((mat,score)) = rust_fuzzy_search::fuzzy_search_best_n(i, crate::instructions::OPS, 1).first() && *score > 0.5 {
                    e.note(format!("{help}: maybe you meant {mat}"));
                }
            }
            Self::ExpectedInt(t, s) => {
                msg!("{error}: expected integer")
                    .label(err!(s, "this was supposed to be a integer"));
                if let Token::Num(n) = t {
                    e.note(format!("{help}: remove the fractional part: {n:.0}"));
                }
            }
            Self::ExpectedInstr(_, s) => {
                msg!("{error}: expected instruction")
                    .label(err!(s, "this was supposed to be a instruction"));
                // it occurs to me that this wont ever be a string, as idents are turned into `Code`
                // if let Some(i) = tokstr!(t.clone()) && let Some((mat,score)) = rust_fuzzy_search::fuzzy_search_best_n(i, crate::instructions::INSTRS, 1).get(0) && *score > 0.5 {
                //     e.note(format!("{help}: maybe you meant {mat}"));
                // }
            }
            Self::LabelNotFound(_, s) => {
                msg!("{error}: label not found")
                    .label(err!(s, "this was supposed to be a (existing) label"));
            }
            Self::InvalidJump(Instruction(target), s) => {
                msg!("{error}: invalid jump")
                    .label(err!(s, "line#{target} is not in the program"))
                    .note(format!(
                        "{help}: there are 0..{} available lines",
                        source.lines().count()
                    ));
            }
            Self::MemoryTooFar(b, s) => {
                msg!("{error}: invalid memory cell/bank")
                    .label(err!(s, "cant get cell/bank#{b}"))
                    .note(format!("{note}: only 126 cells/banks are allowed"));
            }
            Self::InvalidMemoryType(t, s) => {
                msg!("{error}: invalid memory type")
                    .label(err!(s, "cant get {t}"))
                    .note(format!("{note}: only banks/cells are allowed"));
            }
            Self::InvalidDisplayType(disp, s) => {
                msg!("{error}: invalid display type")
                    .label(err!(s, "cant get {disp}"))
                    .note(format!("{help}: change this to 'display'"));
            }
            Self::UnsupportedImageOp(op, s) => {
                msg!("{error}: invalid image op").label(err!(
                    s,
                    "must be one of {{clear, color, col, stroke, line, rect, lineRect, triangle}}"
                ));
                if let Some((mat,score)) = rust_fuzzy_search::fuzzy_search_best_n(op, crate::instructions::draw::INSTRS, 1).first() && *score > 0.5 {
                    e.note(format!("{help}: you may have meant {mat}"));
                }
            }
            Self::NoDisplay(disp, s) => {
                msg!("{error}: no display allocated").label(err!(s, "display#{disp} has not been created")).note(format!("{note}: it is impossible for me to dynamically allocate displays, as 'display1' could be large or small"));
            }
            Self::IndexOutOfBounds(index, size, s) => {
                msg!("{error}: index {index} out of bounds")
                    .label(err!(s, "memory has only {size} elements"));
            }
        };
        e
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
        ($e:ident($($stuff:expr),+)) => {
            Error::$e($($stuff,)+ tokens.span())
        }
    }
    macro_rules! yeet {
        ($e:ident($($stuff:expr),+)) => {
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
                        let op = op.try_into().map_err(|op| err!(ExpectedOp(op)))?;
                        let a = take_var!(tok!()?)?;
                        let b = take_var!(tok!()?)?;
                        executor.jmp();
                        unfinished_jumps.push((UJump::Sometimes { a, b, op }, i, executor.last()));
                    }
                } else if let Ok(n) = take_int!(tok.clone()) {
                    let to = Instruction(n);
                    let op = tok!()?;
                    if op == Token::Always {
                        executor.add(AlwaysJump { to });
                    } else {
                        let op = op.try_into().map_err(|op| err!(ExpectedOp(op)))?;
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
                if let Ok(op) = MathOp1::try_from(op.clone()) {
                    // assigning to a var is useless but legal
                    let out = take_numvar!(tok!()?)?;
                    let x = take_numvar!(tok!()?)?;
                    executor.add(Op1::new(op, x, out));
                } else if let Ok(op) = MathOp2::try_from(op.clone()) {
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
                let index = take_numvar!(tok!()?)?;
                match index {
                    LAddress::Const(LVar::Num(v)) => {
                        if !container.fits(v.round() as usize) {
                            yeet!(IndexOutOfBounds(v.round() as usize, container.size()));
                        }
                    }
                    _ => {}
                }

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
                let index = take_numvar!(tok!()?)?;
                match index {
                    LAddress::Const(LVar::Num(v)) => {
                        if !container.fits(v.round() as usize) {
                            yeet!(IndexOutOfBounds(v.round() as usize, container.size()));
                        }
                    }
                    _ => {}
                }
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
                let t = tok!();
                if let Ok(t) = t && t != Token::Newline {
                    let screen = take_ident!(t)?;
                    if screen != "display" {
                        yeet!(InvalidDisplayType(screen));
                    }
                    let display = executor
                        .display(take_int!(tok!()?)?)
                        .map_err(|n| err!(NoDisplay(n)))?;
                    executor.add(Flush { display });
                } else {
                    executor.add(Flush::default())
                }
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
