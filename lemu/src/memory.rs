use beef::lean::Cow;

use crate::debug::{info::VarData, printable::Printable};
#[derive(Clone, Debug)]
pub enum LVar<'string> {
    Num(f64),
    String(Cow<'string, str>),
}

impl PartialEq for LVar<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Num(a), Self::Num(b)) => (a - b).abs() < 0.000_001,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            _ => false,
        }
    }
}

impl Default for LVar<'static> {
    fn default() -> Self {
        Self::Num(0.0)
    }
}

impl LVar<'_> {
    // get null
    pub const fn null() -> LVar<'static> {
        LVar::Num(0.0)
    }

    pub const fn num(&self) -> Option<f64> {
        match *self {
            Self::Num(n) => Some(n),
            Self::String(_) => None,
        }
    }
}

#[derive(Clone, Copy)]
pub struct LAddress {
    pub address: u32,
}

impl LAddress {
    /// # Safety
    ///
    /// ensure that address is valid
    pub(crate) const unsafe fn addr(address: u32) -> Self {
        LAddress { address }
    }
}

#[derive(Copy, Clone)]
pub struct Priv {
    _priv: (),
}

impl std::fmt::Debug for LAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:x}", self.address)
    }
}

impl std::fmt::Display for LVar<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(n) => write!(f, "{}", crate::debug::ff(*n)), // yeeeeahhhh
            Self::String(s) => write!(f, r#""{s}""#),
        }
    }
}

impl From<f64> for LVar<'_> {
    fn from(value: f64) -> Self {
        Self::Num(value)
    }
}

impl From<bool> for LVar<'_> {
    fn from(value: bool) -> Self {
        Self::Num(value.into())
    }
}

impl From<usize> for LVar<'_> {
    fn from(value: usize) -> Self {
        Self::Num(value as f64)
    }
}

impl<'s> From<&'s str> for LVar<'s> {
    fn from(value: &'s str) -> Self {
        Self::String(value.into())
    }
}

impl<'s> From<Cow<'s, str>> for LVar<'s> {
    fn from(value: Cow<'s, str>) -> Self {
        Self::String(value)
    }
}

#[derive(Debug)]
pub struct LRegistry<'str>(pub Box<[LVar<'str>]>);

impl<'s> std::ops::Index<LAddress> for LRegistry<'s> {
    type Output = LVar<'s>;

    fn index(&self, index: LAddress) -> &Self::Output {
        debug_assert!((index.address as usize) < self.0.len());
        // SAFETY: LAddress promises to be in our bounds.
        // lreg has no constructors, so unless you keep one around, grab a laddr, and use it on the old lreg,
        // this is safeish.
        // lreg is private to the outside world, so its ok
        unsafe { self.0.get_unchecked(index.address as usize) }
    }
}

impl<'s> std::ops::IndexMut<LAddress> for LRegistry<'s> {
    fn index_mut(&mut self, index: LAddress) -> &mut Self::Output {
        debug_assert!((index.address as usize) < self.0.len());
        // SAFETY: see above
        unsafe { self.0.get_unchecked_mut(index.address as usize) }
    }
}

impl std::fmt::Display for LRegistry<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "R[")?;
        let mut iter = self
            .0
            .iter()
            .enumerate()
            .filter(|&(_, v)| v != &LVar::null());
        if let Some((i, v)) = iter.next() {
            write!(f, "{i}: {v}")?;
        }
        for (i, v) in iter {
            write!(f, ", {i}: {v}")?;
        }
        write!(f, "]")
    }
}

impl LRegistry<'_> {
    pub fn get(&self, a: LAddress) -> &LVar {
        &self[a]
    }
}

impl Printable for LRegistry<'_> {
    fn print(
        &self,
        info: &crate::debug::info::DebugInfo<'_>,
        f: &mut impl std::fmt::Write,
    ) -> std::fmt::Result {
        write!(f, "R[")?;
        let mut iter = self
            .0
            .iter()
            .zip(0..self.0.len() as u32)
            .filter(|&(v, _)| v != &LVar::null())
            // SAFETY: the address comes from me
            .map(|(v, i)| (&info[unsafe { LAddress::addr(i) }], v))
            .filter_map(|(d, v)| match d {
                VarData::Variable(d) => Some((*d, v)),
                VarData::Constant(_) => None,
            });
        if let Some((i, v)) = iter.next() {
            write!(f, "{i}: {v}")?;
        }
        for (i, v) in iter {
            write!(f, ", {i}: {v}")?;
        }
        write!(f, "]")
    }
}
