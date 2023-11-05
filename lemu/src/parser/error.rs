use super::tokstr;
use crate::executor::Instruction;
use crate::lexer::Token;
use logos::Span;

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
    /// Occurs from eg `control shootp building 4`
    #[error("expected string, got {0:?}")]
    ExpectedString(Token<'s>, Span),
    /// Occurs from `status not_a_bool`
    #[error("expected bool, got {0:?}")]
    ExpectedBool(Token<'s>, Span),
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
    #[error("unknown display type {0}, expected 'display'")]
    InvalidDisplayType(&'s str, Span),
    /// Occurs from `draw house` (or `draw image`, a valid but unsupported instruction here)
    #[error("unknown image operation {0}")]
    UnsupportedImageOp(&'s str, Span),
    /// Occurs from `control what`
    #[error("unknown control operation {0}")]
    UnknownControlOp(&'s str, Span),
    /// Occurs from `ucontrol kill`
    #[error("unknown ucontrol operation {0}")]
    UnknownUnitControlOp(&'s str, Span),
    /// Occurs from `ulocate five`
    #[error("unknown ulocate operation {0}")]
    UnknownUnitLocateOp(&'s str, Span),
    /// Occurs from `getblock core`
    #[error("unknown getblock operation {0}")]
    UnknownGetBlockOp(&'s str, Span),
    /// Occurs from `setblock unit`
    #[error("unknown setblock operation {0}")]
    UnknownSetBlockOp(&'s str, Span),
    /// Occurs from `setrule hello`
    #[error("unknown rule {0}")]
    UnknownRule(&'s str, Span),
    /// Occurs from `cutscene begin`
    #[error("unknown cutscene {0}")]
    UnknownCutscene(&'s str, Span),
    /// Occurs from `fetch hostages`
    #[error("unknown fetch operation {0}")]
    UnknownFetchOp(&'s str, Span),
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
    /// We have a limit of [`u8::MAX`] variables.
    #[error("too many variables")]
    TooManyVariables(Span),
}

impl Error<'_> {
    /// Produces a [`Error`](lerr::Error) from this error.
    #[cfg(feature = "diagnose")]
    pub fn diagnose<'s>(&self, source: &'s str) -> lerr::Error<'s> {
        use comat::{cformat as cmt, cformat_args};
        use lerr::Error;

        let error = cformat_args!("{bold_red}error{reset}");
        let note = cformat_args!("{bold_blue}note{reset}");
        let help = cformat_args!("{bold_green}help{reset}");
        let mut e = Error::new(source);
        macro_rules! msg {
            ($ms:literal $(, $args:expr)* $(,)?) => {
                e.message(cmt!($ms $(, $args)*))
            };
        }
        macro_rules! op {
            ($op:ident, $ops:expr) => {{
                let mut out = String::from("{");
                let mut ops = $ops.iter();
                use std::fmt::Write;
                write!(out, "{}", ops.next().unwrap()).unwrap();
                for op in ops {
                    write!(out, ", {}", op).unwrap();
                }
                out.write_char('}').unwrap();
                if let Some((mat, score)) =
                    rust_fuzzy_search::fuzzy_search_best_n($op, $ops, 1).first()
                    && *score > 0.5
                {
                    e.note(cmt!("{help}: you may have meant {bold_green}{mat}{reset}"));
                }
                out
            }};
        }
        match self {
            Self::UnexpectedEof => {
                msg!("{error}: wasnt able to finish read, got newline").label((
                    source.len()..source.len(),
                    cmt!("there was supposed to be another token here"),
                ));
            }
            Self::ExpectedVar(t, s) => {
                msg!("{error}: expected variable, got {:?}", t)
                    .label((s, cmt!("this was supposed to be a {blue}variable{reset} ({magenta}identifier{reset}, {magenta}number{reset}, or {magenta}string{reset})")));
            }
            Self::ExpectedIdent(_, s) => {
                msg!("{error}: expected identifier").label((
                    s,
                    cmt!("this was supposed to be a {bold_blue}identifier{reset} (eg. {magenta}name{reset})"),
                ));
            }
            Self::ExpectedJump(t, s) => {
                msg!("{error}: expected jump target")
                    .label((s, cmt!("this was supposed to be a jump target")))
                    .note(
                        cmt!("{note}: a jump target is a {bold_blue}label{reset} ({magenta}ident{reset}, or {magenta}integer{reset})"),
                    );
                if let Token::Num(n) = t {
                    e.note(cmt!(
                        "{help}: remove the fractional part: {bold_green}{n:.0}{reset}"
                    ));
                }
            }
            Self::ExpectedNum(_, s) => {
                msg!("{error}: expected number")
                    .label((s, cmt!("this was supposed to be a {bold_blue}number{reset} (eg. {magenta}3.14159{reset})")));
            }
            Self::ExpectedString(_, s) => {
                msg!("{error}: expected string").label((s, cmt!(r#"this was supposed to be a {bold_blue}string{reset} (eg. {magenta}"a cool string"{reset})"#)));
            }
            Self::ExpectedBool(_, s) => {
                msg!("{error}: expected bool").label((s, cmt!("this was supposed to be a {bold_blue}boolean{reset} (eg. {magenta}true{reset})")));
            }
            Self::ExpectedOp(t, s) => {
                msg!("{error}: expected operator")
                    .label((s, cmt!("this was supposed to be a {bold_blue}operator{reset} (eg. {magenta}equal{reset})")));
                if let Some(i) = tokstr!(*t)
                    && let Some((mat, score)) =
                        rust_fuzzy_search::fuzzy_search_best_n(i, crate::instructions::OPS, 1)
                            .first()
                    && *score > 0.5
                {
                    e.note(cmt!("{help}: maybe you meant {bold_green}{mat}{reset}"));
                }
            }
            Self::ExpectedInt(t, s) => {
                msg!("{error}: expected integer")
                    .label((s, cmt!("this was supposed to be a {bold_blue}integer{reset} (eg. {magenta}4{reset})")));
                if let Token::Num(n) = t {
                    e.note(cmt!(
                        "{help}: remove the mantissa: {bold_green}{n:.0}{reset}"
                    ));
                }
            }
            Self::ExpectedInstr(t, s) => {
                msg!("{error}: expected instruction")
                    .label((s, cmt!("this was supposed to be a {bold_blue}instruction{reset} (eg. {magenta}print{reset})")));
                if let Some(i) = tokstr!(*t)
                    && let Some((mat, score)) = rust_fuzzy_search::fuzzy_search_best_n(
                        i,
                        &[
                            "getlink",
                            "read",
                            "write",
                            "set",
                            "op",
                            "end",
                            "drawflush",
                            "draw",
                            "print",
                            "packcolor",
                            "jump",
                            "stop",
                            "printflush",
                            "control",
                            "radar",
                            "sensor",
                            "wait",
                            "lookup",
                            "packcolor",
                            "ubind",
                            "ucontrol",
                            "uradar",
                            "ulocate",
                            "getblock",
                            "setblock",
                            "spawn",
                            "status",
                            "spawnwave",
                            "setrule",
                            "cutscene",
                            "explosion",
                            "setrate",
                            "fetch",
                            "getflag",
                            "setflag",
                            "setprop",
                            "effect",
                        ],
                        1,
                    )
                    .first()
                    && *score > 0.5
                {
                    e.note(cmt!("{help}: maybe you meant {mat}"));
                }
            }
            Self::LabelNotFound(_, s) => {
                msg!("{error}: label not found").label((
                    s,
                    cmt!("this was supposed to be a (existing) {bold_blue}label{reset}"),
                )).note(cmt!("{help}: define a label with {yellow}`label_name:`{reset}, then you can {yellow}`jump label_name`{reset}."));
            }
            Self::InvalidJump(target, s) => {
                msg!("{error}: invalid jump")
                    .label((
                        s,
                        cmt!(
                            "line#{bold_red}{}{reset} is not in the program",
                            target.get()
                        ),
                    ))
                    .note(cmt!(
                        "{help}: there are {bold_blue}{}{reset} available lines",
                        source.lines().count()
                    ));
            }
            Self::MemoryTooFar(b, s) => {
                msg!("{error}: invalid memory cell/bank")
                    .label((s, cmt!("cant get cell/bank#{bold_red}{b}{reset}")))
                    .note(cmt!(
                        "{note}: only {blue}126{reset} cells/banks are allowed"
                    ));
            }
            Self::InvalidMemoryType(t, s) => {
                msg!("{error}: invalid memory type {bold_red}{}{reset}", t)
                    .label((s, "here"))
                    .note(cmt!("{note}: only banks/cells are allowed"));
            }
            Self::InvalidDisplayType(disp, s) => {
                msg!("{error}: invalid display type {bold_red}{}{reset}", disp)
                    .label((s, "here"))
                    .note(cmt!("{help}: change this to {bold_green}'display'{reset}"));
            }
            Self::UnknownControlOp(op, s) => {
                let available = op!(op, &["enabled", "shoot", "shootp", "config", "color"]);
                msg!("{error}: invalid control op {}", op)
                    .label((s, cmt!("must be one of {available}",)));
            }
            Self::UnknownUnitControlOp(op, s) => {
                let available = op!(
                    op,
                    &[
                        "idle", "stop", "move", "approach", "boost", "pathfind", "target",
                        "targetp", "itemDrop", "itemTake", "payDrop", "payEnter", "mine", "flag",
                        "build", "getBlock", "within"
                    ]
                );
                msg!("{error}: unknown unit control op {}", op)
                    .label((s, cmt!("must be one of {available}",)));
            }
            Self::UnknownSetBlockOp(op, s) => {
                let available = op!(op, &["floor", "ore", "block"]);
                msg!("{error}: unknown set block op {}", op)
                    .label((s, cmt!("must be one of {available}")));
            }
            Self::UnknownUnitLocateOp(op, s) => {
                let a = op!(op, &["ore", "spawn", "damaged", "building"]);
                msg!("{error}: unkown unit locate op {}", op)
                    .label((s, cmt!("must be one of {a}",)));
            }
            Self::UnknownGetBlockOp(op, s) => {
                let a = op!(op, &["floor", "ore", "block", "building"]);
                msg!("{error}: unknown getblock op {}", op).label((s, cmt!("must be one of {a}",)));
            }
            Self::UnsupportedImageOp(op, s) => {
                let a = op!(op, crate::instructions::draw::INSTRS);
                msg!("{error}: invalid image op {}", op).label((s, cmt!("must be one of {a}")));
            }
            Self::UnknownRule(rule, s) => {
                let a = op!(
                    rule,
                    &[
                        "currentWaveTime",
                        "waveTimer",
                        "waves",
                        "wave",
                        "waveSpacing",
                        "waveSending",
                        "attackMode",
                        "enemyCoreBuildRadius",
                        "dropZoneRadius",
                        "unitCap",
                        "wave",
                        "lighting",
                        "ambientLight",
                        "solarMultiplier",
                        "mapArea",
                        "buildSpeed",
                        "unitHealth",
                        "unitBuildSpeed",
                        "unitCost",
                        "unitDamage",
                        "blockHealth",
                        "blockDamage",
                        "rtsMinWeight",
                        "rtsMinSquad",
                    ]
                );
                msg!("{error}: invalid rule {}", rule).label((s, cmt!("must be one of {a}")));
            }
            Self::UnknownCutscene(op, s) => {
                let a = op!(op, &["pan", "zoom", "stop"]);
                msg!("{error}: invalid cutscene type {}", op)
                    .label((s, cmt!("must be one of {a}")));
            }
            Self::UnknownFetchOp(op, s) => {
                let a = op!(
                    op,
                    &[
                        "buildCount",
                        "coreCount",
                        "playerCount",
                        "unitCount",
                        "build",
                        "core",
                        "player",
                        "unit"
                    ]
                );
                msg!("{error}: invalid op {}", op).label((s, cmt!("must be one of {a}")));
            }
            Self::NoDisplay(disp, s) => {
                msg!("{error}: no display allocated")
                    .label((s, cmt!("display#{bold_red}{disp}{reset} has not been created")))
                    .note(cmt!("{note}: it is impossible for me to dynamically allocate displays, as {blue}'display1'{reset} could be large or small"));
            }
            Self::IndexOutOfBounds(index, size, s) => {
                msg!("{error}: {bold_red}index{reset} {} out of bounds", index)
                    .label((s, cmt!("memory has only {magenta}{size}{reset} elements")));
            }
            Self::TooManyVariables(s) => {
                msg!("{error}: {bold_red}too many variables{reset}. ")
                    .label((s, cmt!("we only have 255 variable slots")))
                    .note(cmt!("consider not using variables"));
            }
        };
        e
    }
}
