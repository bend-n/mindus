use super::{get_num, Flow, LInstruction};
use crate::{
    executor::{ExecutorContext, Memory},
    memory::{LAddress, LVar},
};
use std::{
    fmt::{self, Display, Formatter},
    io::Write as Wr,
};

#[derive(Debug, Copy, Clone)]

pub struct Read {
    pub(crate) index: LAddress,
    pub(crate) output: LAddress,
    pub(crate) container: Memory,
}

impl LInstruction for Read {
    fn run<'v, W: Wr>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        let i = get_num!(exec.get(self.index)).round() as usize;
        if let Some(&n) = exec.mem(self.container).get(i) {
            *exec.get_mut(self.output) = LVar::from(n);
        };
        Flow::Continue
    }
}

impl Display for Read {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "read {} {} {}", self.output, self.container, self.index)
    }
}

#[derive(Debug, Copy, Clone)]

pub struct Write {
    pub(crate) index: LAddress,
    pub(crate) set: LAddress,
    pub(crate) container: Memory,
}

impl LInstruction for Write {
    fn run<'v, W: Wr>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        let i = get_num!(exec.get(self.index)).round() as usize;
        if let &LVar::Num(b) = exec.get(self.set) {
            if let Some(a) = exec.mem(self.container).get_mut(i) {
                *a = b;
            }
        }
        Flow::Continue
    }
}

impl Display for Write {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "write {} {} {}", self.set, self.container, self.index)
    }
}

#[derive(Debug, Copy, Clone)]

pub struct Print {
    pub(crate) val: LAddress,
}
impl LInstruction for Print {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        let v = exec.get(self.val).clone();
        if let Some(o) = &mut exec.output {
            match v {
                LVar::Num(n) => write!(o, "{n}"),
                LVar::String(s) => write!(o, r#"{s}"#),
            }
            .unwrap();
        }
        Flow::Continue
    }
}
impl Display for Print {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "print {}", self.val)
    }
}
