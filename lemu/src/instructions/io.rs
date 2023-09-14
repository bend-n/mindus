use super::{get_num, Flow, LInstruction};
use crate::{
    executor::{ExecutorContext, Memory},
    memory::{LAddress, LVar},
};
use std::io::Write as Wr;

#[derive(Debug)]
pub struct Read<'v> {
    pub(crate) index: LAddress<'v>,
    pub(crate) output: LAddress<'v>,
    pub(crate) container: Memory,
}

impl<'v> LInstruction<'v> for Read<'v> {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        let i = get_num!(exec.get(&self.index)).round() as usize;
        if let Some(&n) = exec.mem(self.container).get(i) {
            if let Some(out) = exec.get_mut(&self.output) {
                *out = LVar::from(n);
            }
        };
        Flow::Continue
    }
}

#[derive(Debug)]
pub struct Write<'v> {
    pub(crate) index: LAddress<'v>,
    pub(crate) set: LAddress<'v>,
    pub(crate) container: Memory,
}

impl<'v> LInstruction<'v> for Write<'v> {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        let i = get_num!(exec.get(&self.index)).round() as usize;
        if let &LVar::Num(b) = exec.get(&self.set) {
            if let Some(a) = exec.mem(self.container).get_mut(i) {
                *a = b;
            }
        }
        Flow::Continue
    }
}

#[derive(Debug)]
pub struct Print<'v> {
    pub(crate) val: LAddress<'v>,
}
impl LInstruction<'_> for Print<'_> {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        let v = exec.get(&self.val).clone();
        if let Some(o) = &mut exec.output {
            write!(o, "{v}").unwrap();
        }
        Flow::Continue
    }
}
