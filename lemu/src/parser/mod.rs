use std::io::Write as Wr;

mod error;
pub use error::Error;

use super::{
    debug::info::{VarData, VarInfo},
    executor::{ExecutorBuilderInternal, Instruction, UPInstr},
    instructions::{
        draw::{
            Clear, Flush, Line, LinePoly, Poly, RectBordered, RectFilled, SetColor, SetStroke,
            Triangle,
        },
        io::{Print, Read, Write},
        AlwaysJump, ConditionOp, DynJump, End, Instr, Jump, MathOp1, MathOp2, Op1, Op2, Set, Stop,
    },
    lexer::{Lexer, Token},
    memory::{LAddress, LVar},
};

macro_rules! tokstr {
    ($tok:expr) => {
        match $tok {
            Token::Ident(i) => Some(i),
            Token::Null => Some("null"),
            Token::Read => Some("read"),
            Token::Write => Some("write"),
            Token::Set => Some("set"),
            Token::Op => Some("op"),
            Token::End => Some("end"),
            Token::Draw => Some("draw"),
            Token::DrawFlush => Some("drawflush"),
            Token::Print => Some("print"),
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
use tokstr;

#[derive(Debug)]
enum UJump {
    Sometimes {
        a: LAddress,
        b: LAddress,
        op: ConditionOp,
    },
    Always,
}

pub fn parse<'source, W: Wr>(
    mut tokens: Lexer<'source>,
    executor: &mut ExecutorBuilderInternal<'source, W>,
) -> Result<(), Error<'source>> {
    let mut used = 0u32;
    let mut mem: Vec<Option<&str>> = Vec::with_capacity(64); // maps &str to usize 
    let mut dbg_info: Vec<VarInfo> = Vec::with_capacity(64);
    macro_rules! push {
        // push a ident
        ($var:expr) => {{
            let v = $var;
            dbg_info.push(VarInfo {
                data: VarData::Variable(v),
                span: tokens.span(),
            });
            executor.mem.push(LVar::null());
            mem.push(Some(v));
            used = used
                .checked_add(1)
                .ok_or(Error::TooManyVariables(tokens.span()))?;
            // SAFETY: just initialized executor.mem
            unsafe { Ok(LAddress::addr(used - 1)) }
        }};
        (const $var:expr) => {{
            let v = $var;
            dbg_info.push(VarInfo {
                data: VarData::Constant(v.clone().into()),
                span: tokens.span(),
            });
            executor.mem.push(LVar::from(v));
            mem.push(None);
            used = used
                .checked_add(1)
                .ok_or(Error::TooManyVariables(tokens.span()))?;
            unsafe { Ok(LAddress::addr(used - 1)) }
        }};
    }
    macro_rules! addr {
        ($val:expr) => {{
            let val = $val;
            match mem
                .iter()
                .zip(0..used)
                .find(|(&v, _)| v == Some(val))
                .map(|(_, i)| i)
            {
                Some(n) => unsafe { Ok(LAddress::addr(n)) },
                None => push!(val),
            }
        }};
    }

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
            let t = tok!()?;
            let container = take_ident!(t.clone())?;
            let mut out = String::new();
            for ch in container.chars() {
                if matches!(ch, '0'..='9') {
                    out.push(ch);
                    continue;
                }
                if out.len() != 0 {
                    yeet!(InvalidMemoryType(container));
                }
            }
            let container = &container[..container.len() - out.len()];
            let n_span = tokens.span().start + container.len()..tokens.span().end;
            let cell_n = out
                .parse::<usize>()
                .map_err(|_| Error::ExpectedInt(t, n_span.clone()))?;
            if cell_n > 126 || cell_n == 0 {
                return Err(Error::MemoryTooFar(cell_n, n_span));
            }
            match container {
                "bank" => executor.bank(cell_n),
                "cell" => executor.cell(cell_n),
                _ => {
                    return Err(Error::InvalidMemoryType(
                        container,
                        tokens.span().start..tokens.span().end - out.len(),
                    ));
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
                addr!(i)
            } else {
                match tok {
                    Token::Num(n) => push!(const n),
                    Token::String(s) => push!(const s),
                    t => Err(err!(ExpectedVar(t))),
                }
            }
        }};
    }
    macro_rules! take_numvar {
        ($tok:expr) => {{
            let tok = $tok;
            if let Some(i) = tokstr!(tok) {
                addr!(i)
            } else {
                match tok {
                    Token::Num(n) => push!(const n),
                    t => Err(err!(ExpectedNum(t))),
                }
            }
        }};
    }
    macro_rules! num_or_255 {
        ($tok:expr) => {
            match $tok {
                Ok(t) => match take_numvar!(t) {
                    Err(_) => push!(const 255),
                    n => n,
                },
                Err(_) => push!(const 255),
            }
        }
    }
    while let Some(token) = tokens.next() {
        match token {
            // # omg
            Token::Comment(c) => executor.program.push(UPInstr::Comment(c)),
            // label:
            Token::Ident(v) if v.ends_with(':') => executor
                .debug_info
                .labels
                .push((&v[..v.len() - 1], executor.next())),
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
                    let from = addr!(take_ident!(from)?)?;
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
                    let span = tokens.span();
                    let op = tok!()?;
                    if op == Token::Always {
                        executor.jmp();
                        unfinished_jumps.push((UJump::Always, (i, span), executor.last()));
                    } else {
                        let op = op.try_into().map_err(|op| err!(ExpectedOp(op)))?;
                        let a = take_var!(tok!()?)?;
                        let b = take_var!(tok!()?)?;
                        executor.jmp();
                        unfinished_jumps.push((
                            UJump::Sometimes { a, b, op },
                            (i, span),
                            executor.last(),
                        ));
                    }
                } else if let Ok(n) = take_int!(tok.clone()) {
                    // SAFETY: we check at the end of the block that it is valid
                    let to = unsafe { Instruction::new(n) };
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
                // this is the parser so i wont bother getting unchecked
                if let LVar::Num(v) = executor.mem[index.address as usize]
                    && !container.fits(v.round() as usize)
                {
                    yeet!(IndexOutOfBounds(v.round() as usize, container.size()));
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
                if let LVar::Num(v) = executor.mem[index.address as usize]
                    && !container.fits(v.round() as usize)
                {
                    yeet!(IndexOutOfBounds(v.round() as usize, container.size()));
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
                macro_rules! three { ($a:expr) => { ($a, $a, $a) }; }
                #[rustfmt::skip]
                macro_rules! four { ($a:expr) => { ($a, $a, $a, $a) }; }
                #[rustfmt::skip]
                macro_rules! five { ($a:expr) => { ($a, $a, $a, $a, $a) }; }
                #[rustfmt::skip]
                macro_rules! six { ($a:expr) => { ($a, $a, $a, $a, $a, $a) }; }
                match instr {
                    "clear" => {
                        let (r, g, b) = three! { num_or_255!(tok!())? };
                        executor.draw(Clear { r, g, b });
                    }
                    "color" => {
                        let (r, g, b, a) = four! { num_or_255!(tok!())? };
                        executor.draw(SetColor { r, g, b, a });
                    }
                    "col" => {
                        let col = take_int!(tok!()?)?;
                        let r = col & 0xff00_0000 >> 24;
                        let g = col & 0x00ff_0000 >> 16;
                        let b = col & 0x0000_ff00 >> 8;
                        let a = col & 0x0000_00ff;
                        let i = SetColor {
                            r: push!(const r)?,
                            g: push!(const g)?,
                            b: push!(const b)?,
                            a: push!(const a)?,
                        };
                        executor.draw(i);
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
                    "poly" => {
                        let (x, y, sides, radius, rot) = five! { take_numvar!(tok!()?)? };
                        executor.draw(Poly {
                            pos: (x, y),
                            sides,
                            radius,
                            rot,
                        })
                    }
                    "linePoly" => {
                        let (x, y, sides, radius, rot) = five! { take_numvar!(tok!()?)? };
                        executor.draw(LinePoly {
                            pos: (x, y),
                            sides,
                            radius,
                            rot,
                        })
                    }
                    // image is WONTFIX
                    i => yeet!(UnsupportedImageOp(i)),
                }
            }
            Token::DrawFlush => {
                let t = tok!();
                if let Ok(t) = t
                    && t != Token::Newline
                {
                    let screen = take_ident!(t.clone())?;
                    let mut out = String::new();
                    for ch in screen.chars() {
                        if ch.is_ascii_digit() {
                            out.push(ch);
                            continue;
                        }
                        if !out.is_empty() {
                            yeet!(InvalidDisplayType(screen));
                        }
                    }
                    let screen = &screen[..screen.len() - out.len()];
                    if screen != "display" {
                        yeet!(InvalidDisplayType(screen));
                    }
                    let n_span = tokens.span().start + screen.len()..tokens.span().end;
                    let screen_n = out
                        .parse::<usize>()
                        .map_err(|_| Error::ExpectedInt(t, n_span.clone()))?;
                    let display = executor
                        .display(screen_n)
                        .map_err(|n| Error::NoDisplay(n, n_span))?;
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
                macro_rules! all {
                    ($is:expr) => {
                        |b| {
                            for var in &mut *b {
                                *var = $is;
                            }
                            Ok(())
                        }
                    };
                }
                macro_rules! take {
                    (@ $b:ident $n:ident skip) => {
                        $b[$n]=tok!()?;

                    };
                    (@ $b:ident $n:ident skip, $($rest:tt)*) => {
                        $b[$n] = tok!()?;$n+=1; take!(@ $b $n $($rest)*);
                    };
                    (@ $b:ident $n:ident $v: expr) => {
                        $b[$n] = $v;
                    };
                    (@ $b:ident $n:ident $v: expr, $($rest:tt)*) => {
                        $b[$n] = $v;
                        $n += 1;
                        take!(@ $b $n $($rest)*);
                    };
                    [$($this:tt)*] => {
                        |b| {
                            let mut n = 0;
                            take!(@ b n $($this)*);
                            Ok(())
                        }
                    };
                }
                macro_rules! num {
                    () => {{
                        let tok = tok!()?;
                        if let Some(i) = tokstr!(tok) {
                            Token::Ident(i)
                        } else {
                            match tok {
                                Token::Num(n) => Token::Num(n),
                                t => yeet!(ExpectedNum(t)),
                            }
                        }
                    }};
                }
                macro_rules! bool {
                    () => {{
                        let tok = tok!()?;
                        if let Some(i) = tokstr!(tok) {
                            Token::Ident(i)
                        } else {
                            match tok {
                                Token::Num(n) => Token::Num(n),
                                t => yeet!(ExpectedBool(t)),
                            }
                        }
                    }};
                }
                macro_rules! ident {
                    () => {
                        take_ident!(tok!()?).map(|v| Token::Ident(v))?
                    };
                }
                #[rustfmt::skip]
                macro_rules! build { () => { ident!() }}
                macro_rules! str {
                    () => {
                        match tok!()? {
                            Token::String(s) => Token::String(s),
                            t => yeet!(ExpectedString(t)),
                        }
                    };
                }
                macro_rules! var {
                    () => {{
                        let tok = tok!()?;
                        if let Some(i) = tokstr!(tok) {
                            Token::Ident(i)
                        } else {
                            match tok {
                                Token::Num(n) => Token::Num(n),
                                Token::String(s) => Token::String(s),
                                t => yeet!(ExpectedVar(t)),
                            }
                        }
                    }};
                }
                macro_rules! instr {
                    (($argc:literal) => $block:expr) => {{
                        let mut v: Box<[_; $argc + 1]> =
                            Box::new(std::array::from_fn(|_| Token::Newline));
                        v[0] = Token::Ident(i);
                        const fn castor<
                            'source,
                            F: FnMut(&mut [Token<'source>; $argc]) -> Result<(), Error<'source>>,
                        >(
                            f: F,
                        ) -> F {
                            f
                        }
                        castor($block)((&mut v[1..]).try_into().unwrap())?;
                        executor.code(v as Box<[Token<'source>]>);
                        nextline!();
                        continue;
                    }};
                }
                macro_rules! minstr {
                    ($($sub:ident($argc:literal) => $block:expr)+ => $err:expr $(,)?) => {{
                        let t = tok!()?;
                        let idnt = take_ident!(t.clone())?;
                        $(if idnt == stringify!($sub) {
                            let mut v: Box<[_; $argc + 2]> = Box::new(std::array::from_fn(|_| Token::Newline));
                            v[0]=Token::Ident(i);
                            v[1]=Token::Ident(stringify!($sub));
                            const fn castor<'source, F: FnMut(&mut [Token<'source>; $argc]) -> Result<(), Error<'source>>>(f: F) -> F { f }
                            castor($block)((&mut v[2..]).try_into().unwrap())?;
                            executor.code(v);
                            nextline!();
                            continue;
                        })+
                        return Err($err(idnt, t));
                    }};
                }
                match i {
                    "printflush" => instr! {
                        (1) => |b| {
                            let t = tok!()?;
                            if let Some(t) = tokstr!(t) {
                                b[0] = Token::Ident(t);
                            } else if t == Token::Null {
                                b[0] = Token::Null;
                            } else {
                                b[0] = Token::Ident("message1");
                            }
                            Ok(())
                        }
                    },
                    "getlink" => instr! { (2) => take![var!(), num!()] },
                    "control" => minstr! {
                        enabled(2) => take![build!(), bool!()]
                        shoot(4) => take![build!(), num!(), num!(), bool!()]
                        shootp(3) => take![build!(), str!(), bool!()]
                        config(2) => take![build!(), tok!()?]
                        color(4) => take![build!(), num!(), num!(), num!()]
                        => |t, _| { err!(UnknownControlOp(t)) },
                    },
                    "radar" => {
                        instr! { (7) => take![ident!(), ident!(), ident!(), ident!(), build!(), num!(), var!()] }
                    }
                    "sensor" => instr! { (3) => take![var!(), tok!()?, tok!()?] },
                    "wait" => instr! { (1) => all!(num!()) },
                    "lookup" => instr! { (3) => take![ident!(), var!(), num!()] },
                    "packcolor" => instr! { (4) => all!(num!()) },
                    "ubind" => instr! { (1) => |b| {
                        let t = tok!()?;
                        if tokstr!(t).is_some() || matches!(t, Token::Null) {
                            b[0] = t;
                        } else {
                            yeet!(ExpectedString(t));
                        };
                        Ok(())
                    } },
                    "ucontrol" => minstr! {
                        idle(0) => |_| Ok(())
                        stop(0) => |_| Ok(())
                        move(2) => all!(num!())
                        approach(3) => all!(num!())
                        boost(1) => all!(bool!())
                        pathfind(2) => all!(num!())
                        target(3) => take![num!(), num!(), bool!()]
                        targetp(2) => take![str!(), bool!()]
                        itemDrop(2) => take![build!(), num!()]
                        itemTake(3) => take![build!(), str!(), num!()]
                        payDrop(0) => |_| Ok(())
                        payTake(1) => all!(bool!())
                        payEnter(0) => |_| Ok(())
                        mine(2) => all!(num!())
                        flag(1) => all!(num!())
                        build(5) => take![num!(), num!(), build!(), num!(), tok!()?]
                        getBlock(4) => take![num!(), num!(), build!(), build!()]
                        within(4) => take![num!(), num!(), num!(), var!()]
                        => |t, _| { err!(UnknownUnitControlOp(t)) }
                    },
                    "uradar" => {
                        instr! { (7) => take![ident!(), ident!(), ident!(), ident!(), build!(), num!(), var!()] }
                    }
                    "ulocate" => {
                        minstr! {
                            building(7) => take![build!(), bool!(), skip, num!(), num!(), bool!(), build!()]
                            spawn(6) => take![skip, skip, skip, num!(), num!(), bool!()]
                            damaged(7) => take![skip, skip, skip, num!(), num!(), var!(), var!()]
                            ore(6) => take![skip, skip, tok!()?, num!(), num!(), bool!()]
                            => |t, _| { err!(UnknownUnitLocateOp(t)) }
                        }
                    }
                    "getblock" => minstr! {
                        floor(3) => take![tok!()?, num!(), num!()]
                        ore(3) => take![tok!()?, num!(), num!()]
                        block(3) => take![tok!()?, num!(), num!()]
                        building(3) => take![tok!()?, num!(), num!()]
                        => |t, _| { err!(UnknownGetBlockOp(t)) }
                    },
                    "setblock" => minstr! {
                        floor(3) => take![num!(), num!(), str!()]
                        ore(3) => take![num!(), num!(), str!()]
                        block(3) => take![num!(), num!(), str!(), str!(), num!()]
                        => |t, _| { err!(UnknownSetBlockOp(t)) }
                    },
                    "spawn" => {
                        instr! { (6) => take![str!(), num!(), num!(), num!(), str!(), var!()] }
                    }
                    "status" => minstr! {
                        true(3) => take![ident!(), var!(), num!()]
                        false(2) => take![ident!(), var!()]
                        => |_, t| { err!(ExpectedBool(t)) }
                    },
                    "spawnwave" => instr! { (3) => take![num!(), num!(), bool!()] },
                    "setrule" => {
                        #[rustfmt::skip]
                        macro_rules! rule { () => { take![num!(), str!()] }}
                        minstr! {
                            currentWaveTime(1) => all!(num!())
                            waveTimer(1) => all!(num!())
                            waves(1) => all!(num!())
                            wave(1) => all!(num!())
                            waveSpacing(1) => all!(num!())
                            waveSending(1) => all!(num!())
                            attackMode(1) => all!(num!())
                            enemyCoreBuildRadius(1) => all!(num!())
                            dropZoneRadius(1) => all!(num!())
                            unitCap(1) => all!(num!())
                            wave(1) => all!(num!())
                            mapArea(4) => |b| {
                                tok!()?;
                                for var in &mut *b {
                                    *var = num!();
                                }
                                Ok(())
                            }
                            lighting(1) => all!(num!())
                            ambientLight(1) => all!(num!())
                            solarMultiplier(1) => all!(num!())
                            buildSpeed(2) => rule!{}
                            unitHealth(2) => rule!{}
                            unitBuildSpeed(2) => rule!{}
                            unitCost(2) => rule!{}
                            unitDamage(2) => rule!{}
                            blockHealth(2) => rule!{}
                            blockDamage(2) => rule!{}
                            rtsMinWeight(2) => rule!{}
                            rtsMinSquad(2) => rule!{}
                            => |t, _| { err!(UnknownRule(t)) }
                        }
                    }
                    "cutscene" => minstr! {
                        pan(3) => all!(num!())
                        zoom(1) => all!(num!())
                        stop(0) => |_| Ok(())
                        => |t, _| { err!(UnknownCutscene(t)) }
                    },
                    "explosion" => {
                        instr! { (8) => take![str!(), num!(), num!(), num!(), bool!(), bool!(), bool!()] }
                    }
                    "setrate" => instr! { (1) => all!(num!()) },
                    "fetch" => minstr! {
                        buildCount(4) => take![var!(), str!(), num!(), str!()] // useless 0
                        unitCount(4)  => take![var!(), str!()]
                        playerCount(4)  => take![var!(), str!()]
                        coreCount(4)  => take![var!(), str!()]
                        unit(3) => take![var!(), str!(), num!()]
                        player(3) => take![var!(), str!(), num!()]
                        core(3) => take![var!(), str!(), num!()]
                        build(3) => take![var!(), str!(), num!()]
                        => |t, _| { err!(UnknownFetchOp(t)) }
                    },
                    "getflag" => instr! { (2) => take![bool!(), str!()] },
                    "setflag" => instr! { (2) => take![str!(), bool!()] },
                    "setprop" => instr! { (3) => take![str!(), var!(), tok!()?] },
                    "effect" => {
                        let mut v = Vec::with_capacity(6);
                        v.push(Token::Ident("effect"));
                        while let Some(tok) = tokens.next()
                            && tok != Token::Newline
                        {
                            v.push(tok);
                        }
                        executor.code(v.into_boxed_slice());
                        // we take the newline here
                        continue;
                    }
                    t => yeet!(ExpectedInstr(Token::Ident(t))),
                }
            }
            t => yeet!(ExpectedInstr(t)),
        }
        nextline!();
    }

    for (j, (label, s), i) in unfinished_jumps {
        let to = executor
            .debug_info
            .labels
            .iter()
            .find(|(v, _)| v == &label)
            .ok_or_else(|| Error::LabelNotFound(label, s))?
            .1;
        executor.program[i.get()] = UPInstr::Instr(match j {
            UJump::Always => Instr::from(AlwaysJump { to }),
            UJump::Sometimes { a, b, op } => Instr::from(Jump::new(op, to, a, b)),
        });
    }

    // check jump validity
    for i in &executor.program {
        if let UPInstr::Instr(
            Instr::Jump(Jump { to, .. }) | Instr::AlwaysJump(AlwaysJump { to, .. }),
        ) = i
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

    executor.debug_info.variables = dbg_info.into();

    Ok(())
}
