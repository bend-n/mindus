use std::fmt::{Debug, Result, Write};

use super::info::DebugInfo;

pub trait Printable: Debug {
    fn print(&self, info: &DebugInfo<'_>, f: &mut impl Write) -> Result;
}
