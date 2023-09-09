use crate::logic::{executor::LAddress, instructions::ConditionOp};

use super::{
    executor::{Instruction, LogicExecutor, ProgramInstruction},
    instructions::{AlwaysJump, End, Instr, Jump, MathOp1, MathOp2, Op1, Op2, Print, Set, Stop},
    lexer::Token,
};
#[derive(thiserror::Error, Debug)]
pub enum ParserError<'s> {
    #[error("unexpected end of stream")]
    UnexpectedEof,
    #[error("expected variable, got {0:?}")]
    ExpectedVariable(Token<'s>),
    #[error("expected identifier, got {0:?}")]
    ExpectedIdentifier(Token<'s>),
    #[error("expected jump target, got {0:?}")]
    ExpectedJump(Token<'s>),
    #[error("unable to find lable {0:?}")]
    LabelNotFound(&'s str),
    #[error("expected operator, found {0:?}")]
    ExpectedOperator(Token<'s>),
    #[error("unable to jump to instruction {0:?}")]
    InvalidJump(Instruction),
}

pub fn parse<'source>(
    mut tokens: impl Iterator<Item = Token<'source>>,
    executor: &mut LogicExecutor<'source>,
) -> Result<(), ParserError<'source>> {
    // maps start to 0
    let mut labels = Vec::new();
    #[derive(Debug)]
    enum UJump<'v> {
        Sometimes {
            a: LAddress<'v>,
            b: LAddress<'v>,
            op: ConditionOp,
        },
        Always,
    }
    let mut unfinished_jumps = Vec::new();
    macro_rules! tok {
        () => {
            dbg!(tokens.next()).ok_or(ParserError::UnexpectedEof)
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
    macro_rules! take_ident {
        ($tok:expr) => {{
            let tok = $tok;
            tokstr!(tok).ok_or(ParserError::ExpectedIdentifier(tok))
        }};
    }
    macro_rules! take_var {
        ($tok:expr) => {{
            let tok = $tok;
            if let Some(i) = tokstr!(tok) {
                Ok(executor.addr(i))
            } else {
                match tok {
                    Token::Num(n) => Ok(executor.add_const(n)),
                    Token::String(s) => Ok(executor.add_const(s)),
                    t => Err(ParserError::ExpectedVariable(t)),
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
                let from = executor.addr(take_ident!(tok!()?)?);
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
                        executor.program.push(ProgramInstruction::UnfinishedJump);
                        unfinished_jumps.push((UJump::Always, i, executor.last()));
                    } else {
                        let op = op
                            .try_into()
                            .map_err(|_| ParserError::ExpectedOperator(op))?;
                        let a = take_var!(tok!()?)?;
                        let b = take_var!(tok!()?)?;
                        executor.program.push(ProgramInstruction::UnfinishedJump);
                        unfinished_jumps.push((UJump::Sometimes { a, b, op }, i, executor.last()));
                    }
                } else if let Token::Num(n) = tok {
                    if n.fract() != 0.0 {
                        return Err(ParserError::ExpectedJump(Token::Num(n)));
                    }
                    let to = Instruction(n as usize);
                    let op = tok!()?;
                    if op == Token::Always {
                        executor.add(AlwaysJump { to });
                    } else {
                        let op = op
                            .try_into()
                            .map_err(|_| ParserError::ExpectedOperator(op))?;
                        let a = take_var!(tok!()?)?;
                        let b = take_var!(tok!()?)?;
                        executor.add(Jump { op, a, b, to });
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
                    executor.add(Op1 { x, op, out });
                } else if let Ok(op) = MathOp2::try_from(op) {
                    let out = take_var!(tok!()?)?;
                    let a = take_var!(tok!()?)?;
                    let b = take_var!(tok!()?)?;
                    executor.add(Op2 { a, b, op, out });
                } else {
                    return Err(ParserError::ExpectedOperator(op));
                }
            }
            // end
            Token::End => {
                executor.add(End {});
            }
            // starting newline, simply skip. continue, so as not to to trigger the nextline!()
            Token::Newline => continue,
            t => todo!("{t:?}"),
        }
        nextline!();
    }

    for (j, l, Instruction(i)) in unfinished_jumps {
        let to = labels
            .iter()
            .find(|(v, _)| v == &l)
            .ok_or(ParserError::LabelNotFound(l))?
            .1;
        executor.program[i] = ProgramInstruction::Instr(match j {
            UJump::Always => Instr::from(AlwaysJump { to }),
            UJump::Sometimes { a, b, op } => Instr::from(Jump { a, b, op, to }),
        });
    }

    // check jump validity
    for i in &executor.program {
        if let ProgramInstruction::Instr(
            Instr::Jump(Jump { to, .. }) | Instr::AlwaysJump(AlwaysJump { to }),
        ) = i
        {
            if !executor.valid(*to) {
                return Err(ParserError::InvalidJump(*to));
            }
        }
    }
    Ok(())
}
