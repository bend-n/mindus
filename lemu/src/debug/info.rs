use std::ops::Range;

use crate::{
    executor::Instruction,
    memory::{LAddress, LVar},
};

pub struct DebugInfo<'s> {
    variables: Box<[Option<VarInfo<'s>>; 65536]>,
    /// maps "start" to 0
    pub labels: Vec<(&'s str, Instruction)>,
}

impl<'s> Default for DebugInfo<'s> {
    fn default() -> Self {
        Self {
            variables: vec![None; 65536].try_into().unwrap(),
            labels: vec![],
        }
    }
}

impl<'s> DebugInfo<'s> {
    pub fn label(&self, of: Instruction) -> Option<&'s str> {
        self.labels.iter().find(|(_, i)| *i == of).map(|&(x, _)| x)
    }
}

impl<'s> std::ops::Index<LAddress> for DebugInfo<'s> {
    type Output = VarData<'s>;

    fn index(&self, index: LAddress) -> &Self::Output {
        &self.variables[index.address as usize]
            .as_ref()
            .unwrap()
            .data
    }
}

impl<'s> DebugInfo<'s> {
    pub fn set_var(&mut self, at: u16, name: &'s str, span: Range<usize>) {
        self.variables[at as usize] = Some(VarInfo {
            data: VarData::Variable(name),
            span,
        });
    }

    pub fn set_const(&mut self, at: u16, var: impl Into<LVar<'s>>, span: Range<usize>) {
        self.variables[at as usize] = Some(VarInfo {
            data: VarData::Constant(var.into()),
            span,
        });
    }
}

#[derive(Clone, Debug)]
struct VarInfo<'s> {
    pub data: VarData<'s>,
    #[allow(dead_code)]
    pub span: Range<usize>,
}

#[derive(Clone, Debug)]
pub enum VarData<'s> {
    Variable(&'s str),
    // not necessary, but convenient.
    Constant(LVar<'s>),
}

impl std::fmt::Display for VarData<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VarData::Variable(name) => f.write_str(name),
            VarData::Constant(c) => write!(f, "{c}"),
        }
    }
}
