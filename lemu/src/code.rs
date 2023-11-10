use crate::debug::{info::DebugInfo, printable::Printable};

use super::{
    instructions::{DrawInstr, Instr},
    lexer::Token,
};

#[derive(Debug)]
pub enum PInstr<'s> {
    Instr(Instr),
    Draw(DrawInstr),
    Code(Box<[Token<'s>]>),
    Comment(&'s str),
}

impl Printable for PInstr<'_> {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Self::Instr(i) => i.print(info, f),
            Self::Draw(i) => i.print(info, f),
            Self::Code(c) => {
                let mut toks = c.iter();
                if let Some(t) = toks.next() {
                    write!(f, "{t}")?;
                }
                for token in toks {
                    write!(f, " {token}")?;
                }
                Ok(())
            }
            Self::Comment(c) => write!(f, "{c}"),
        }
    }
}

impl Printable for Code<'_> {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        for instr in &*self.0 {
            instr.print(info, f)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

#[repr(transparent)]
#[derive(Debug)]
pub struct Code<'s>(Box<[PInstr<'s>]>);

// Pin requires
impl<'s> std::ops::Deref for Code<'s> {
    type Target = [PInstr<'s>];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl<'s> Code<'s> {
    pub(crate) fn new(code: Box<[PInstr<'s>]>) -> Self {
        Self(code)
    }
}
