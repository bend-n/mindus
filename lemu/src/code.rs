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

impl std::fmt::Display for PInstr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Instr(i) => write!(f, "{i}"),
            Self::Draw(i) => write!(f, "{i}"),
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

impl std::fmt::Display for Code<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for instr in &*self.0 {
            writeln!(f, "{instr}")?;
        }
        Ok(())
    }
}

#[repr(transparent)]
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
