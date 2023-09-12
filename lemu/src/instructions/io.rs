use super::{Flow, LInstruction};
use crate::{
    executor::{ExecutorContext, Memory},
    memory::{LAddress, LVar},
};
use std::io::Write as Wr;

#[derive(Debug)]
pub struct Read<'v> {
    // index guranteed to never be out of bounds
    pub(crate) index: usize,
    pub(crate) output: LAddress<'v>,
    pub(crate) container: Memory,
}

impl<'v> LInstruction<'v> for Read<'v> {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        let to = exec.mem(self.container)[self.index];
        let Some(out) = exec.get_mut(self.output) else {
            return Flow::Continue;
        };
        *out = LVar::from(to);
        Flow::Continue
    }
}

#[derive(Debug)]
pub struct Write<'v> {
    // index guranteed to never be out of bounds
    pub(crate) index: usize,
    pub(crate) set: LAddress<'v>,
    pub(crate) container: Memory,
}

impl<'v> LInstruction<'v> for Write<'v> {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'v, W>) -> Flow {
        let LVar::Num(n) = exec.get(self.set) else {
            return Flow::Continue;
        };
        exec.mem(self.container)[self.index] = n;
        Flow::Continue
    }
}

#[derive(Debug)]
pub struct Print<'v> {
    pub(crate) val: LAddress<'v>,
}
impl LInstruction<'_> for Print<'_> {
    fn run<W: Wr>(&self, exec: &mut ExecutorContext<'_, W>) -> Flow {
        let v = exec.get(self.val);
        if let Some(o) = &mut exec.output {
            write!(o, "{v}").unwrap();
        }
        Flow::Continue
    }
}
