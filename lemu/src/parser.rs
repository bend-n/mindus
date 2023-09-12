use std::io::Write as Wr;

use super::{
    executor::{ExecutorBuilder, Instruction, UPInstr},
    instructions::{
        AlwaysJump, Clear, ConditionOp, DrawFlush, DrawLine, DrawRectBordered, DrawRectFilled,
        DrawTriangle, End, Instr, Jump, MathOp1, MathOp2, Op1, Op2, Print, Read, Set,
        SetColorConst, SetColorDyn, SetStroke, Stop, Write,
    },
    lexer::Token,
    memory::LAddress,
};
#[derive(thiserror::Error, Debug)]
pub enum ParserError<'s> {
    #[error("unexpected end of stream")]
    UnexpectedEof,
    #[error("expected variable, got {0:?}")]
    ExpectedVar(Token<'s>),
    #[error("expected identifier, got {0:?}")]
    ExpectedIdent(Token<'s>),
    #[error("expected jump target, got {0:?}")]
    ExpectedJump(Token<'s>),
    #[error("expected number, got {0:?}")]
    ExpectedNum(Token<'s>),
    #[error("expected operator, got {0:?}")]
    ExpectedOp(Token<'s>),
    #[error("expected integer, got {0:?}")]
    ExpectedInt(Token<'s>),
    #[error("unable to find lable {0:?}")]
    LabelNotFound(&'s str),
    #[error("unable to jump to instruction {0:?}")]
    InvalidJump(Instruction),
    // no, you cannot have bank9223372036854775807
    #[error("cannot get cell>{0:?}")]
    MemoryTooFar(usize),
    #[error("unknown memory type {0:?}, expected (cell)|(bank)")]
    InvalidMemoryType(&'s str),
    #[error("unknown display type {0:?}, expected 'display'")]
    InvalidDisplayType(&'s str),
    #[error("unknown image operation {0:?}")]
    UnsupportedImageOp(&'s str),
    #[error("couldnt get display #{0:?}.")]
    /// call `display` more to have more display options
    NoDisplay(usize),
    #[error("expected u8, found {0}")]
    ExpectedU8(usize),
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
    mut tokens: impl Iterator<Item = Token<'source>>,
    executor: &mut ExecutorBuilder<'source, W>,
) -> Result<(), ParserError<'source>> {
    let mut mem = Vec::new(); // maps &str to usize
                              // maps "start" to 0
    let mut labels = Vec::new();
    let mut unfinished_jumps = Vec::new();
    macro_rules! tok {
        () => {
            tokens.next().ok_or(ParserError::UnexpectedEof)
        };
    }
    #[rustfmt::skip]
    macro_rules! nextline {
        () => {
            while let Some(tok) = tokens.next() && tok != Token::Newline { }
        };
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
    macro_rules! take_int {
        ($tok:expr) => {
            match $tok {
                Token::Num(n) if n.fract() == 0.0 && n >= 0.0 => Ok(n as usize),
                t => Err(ParserError::ExpectedInt(t)),
            }
        };
    }
    macro_rules! take_memory {
        () => {{
            let container = take_ident!(tok!()?)?;
            let cell_n = take_int!(tok!()?)?;
            if cell_n > 126 || cell_n == 0 {
                return Err(ParserError::MemoryTooFar(cell_n));
            }
            match container {
                "bank" => executor.bank(cell_n),
                "cell" => executor.cell(cell_n),
                _ => return Err(ParserError::InvalidMemoryType(container)),
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
            tokstr!(tok).ok_or(ParserError::ExpectedIdent(tok))
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
                    t => Err(ParserError::ExpectedVar(t)),
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
                    t => Err(ParserError::ExpectedNum(t)),
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
                let from = addr!(take_ident!(tok!()?)?);
                let to = take_var!(tok!()?)?;
                executor.add(Set { from, to });
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
                        let op = op.try_into().map_err(|()| ParserError::ExpectedOp(op))?;
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
                        let op = op.try_into().map_err(|()| ParserError::ExpectedOp(op))?;
                        let a = take_var!(tok!()?)?;
                        let b = take_var!(tok!()?)?;
                        executor.add(Jump::new(op, to, a, b));
                    }
                } else {
                    return Err(ParserError::ExpectedJump(tok));
                };
            }
            // op add c 1 2
            Token::Op => {
                let op = tok!()?;
                if let Ok(op) = MathOp1::try_from(op) {
                    // assigning to a var is useless but legal
                    let out = take_var!(tok!()?)?;
                    let x = take_var!(tok!()?)?;
                    executor.add(Op1::new(op, x, out));
                } else if let Ok(op) = MathOp2::try_from(op) {
                    let out = take_var!(tok!()?)?;
                    let a = take_var!(tok!()?)?;
                    let b = take_var!(tok!()?)?;
                    executor.add(Op2::new(op, a, b, out));
                } else {
                    return Err(ParserError::ExpectedOp(op));
                }
            }
            // write 5.0 bank1 4 (aka bank1[4] = 5.0)
            Token::Write => {
                let set = take_var!(tok!()?)?;
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
                    return Err(ParserError::ExpectedIdent(dty));
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
                        executor.draw(DrawLine {
                            point_a: (x, y),
                            point_b: (x2, y2),
                        });
                    }
                    "rect" => {
                        let (x, y, width, height) = four! { take_numvar!(tok!()?)? };
                        executor.draw(DrawRectFilled {
                            position: (x, y),
                            width,
                            height,
                        });
                    }
                    "lineRect" => {
                        let (x, y, width, height) = four! { take_numvar!(tok!()?)? };
                        executor.draw(DrawRectBordered {
                            position: (x, y),
                            width,
                            height,
                        });
                    }
                    "triangle" => {
                        let (x, y, x2, y2, x3, y3) = six! { take_numvar!(tok!()?)? };
                        executor.draw(DrawTriangle {
                            points: ((x, y), (x2, y2), (x3, y3)),
                        });
                    }
                    // poly is TODO, image is WONTFIX
                    i => return Err(ParserError::UnsupportedImageOp(i)),
                }
            }
            Token::DrawFlush => {
                let screen = take_ident!(tok!()?)?;
                if screen != "display" {
                    return Err(ParserError::InvalidDisplayType(screen));
                }
                let display = executor
                    .display(take_int!(tok!()?)?)
                    .map_err(ParserError::NoDisplay)?;
                executor.add(DrawFlush { display });
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
                    write!(c, " ").unwrap();
                    tok.write(&mut c).expect("didnt know writing to a string could fail");
                }
                executor.code(c);
                // we take the newline here
                continue;
            }
            _ => todo!(),
        }
        nextline!();
    }

    for (j, l, Instruction(i)) in unfinished_jumps {
        let to = labels
            .iter()
            .find(|(v, _)| v == &l)
            .ok_or(ParserError::LabelNotFound(l))?
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
                return Err(ParserError::InvalidJump(*to));
            }
        }
    }

    executor.mem(mem.len());

    Ok(())
}
