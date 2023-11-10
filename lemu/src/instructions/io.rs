use super::{get_num, Flow, LInstruction};
use crate::{
    debug::{info::DebugInfo, printable::Printable},
    executor::{ExecutorContext, Memory},
    memory::{LAddress, LVar},
};
use std::{fmt, io::Write as Wr};

#[derive(Debug, Copy, Clone)]

pub struct Read {
    pub(crate) index: LAddress,
    pub(crate) output: LAddress,
    pub(crate) container: Memory,
}

impl LInstruction for Read {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        let i = get_num!(exec.get(self.index)).round() as usize;
        if let Some(&n) = exec.mem(self.container).get(i) {
            *exec.get_mut(self.output) = LVar::from(n);
        };
        Flow::Continue
    }
}

impl Printable for Read {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(
            f,
            "read {} {} {}",
            info[self.output], self.container, info[self.index]
        )
    }
}

#[derive(Debug, Copy, Clone)]

pub struct Write {
    pub(crate) index: LAddress,
    pub(crate) set: LAddress,
    pub(crate) container: Memory,
}

impl LInstruction for Write {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        let i = get_num!(exec.get(self.index)).round() as usize;
        if let &LVar::Num(b) = exec.get(self.set) {
            if let Some(a) = exec.mem(self.container).get_mut(i) {
                *a = b;
            }
        }
        Flow::Continue
    }
}

impl Printable for Write {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(
            f,
            "write {} {} {}",
            info[self.set], self.container, info[self.index]
        )
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
// haha
impl Printable for Print {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl fmt::Write) -> fmt::Result {
        write!(f, "print {}", info[self.val])
    }
}
