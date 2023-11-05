use beef::lean::Cow;
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
}

#[derive(Clone, Copy)]
pub struct LAddress {
    pub address: u8,
}

impl LAddress {
    pub(crate) const fn addr(address: u8) -> Self {
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

impl std::fmt::Display for LAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:x}", self.address)
    }
}

impl std::fmt::Display for LVar<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Num(n) => write!(f, "{n}"),
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

/// cleared every loop
#[derive(Debug)]
pub struct LRegistry<'str>(pub [LVar<'str>; 255]);

impl<'s> std::ops::Index<LAddress> for LRegistry<'s> {
    type Output = LVar<'s>;

    fn index(&self, index: LAddress) -> &Self::Output {
        &self.0[index.address as usize]
    }
}

impl<'s> std::ops::IndexMut<LAddress> for LRegistry<'s> {
    fn index_mut(&mut self, index: LAddress) -> &mut Self::Output {
        &mut self.0[index.address as usize]
    }
}

impl<'s> Default for LRegistry<'s> {
    fn default() -> Self {
        Self([const { LVar::null() }; 255])
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
        while let Some((i, v)) = iter.next() {
            write!(f, ", {i}: {v}")?;
        }
        write!(f, "]")
    }
}

impl<'s> LRegistry<'s> {
    pub fn get<'a>(&'a self, a: LAddress) -> &LVar {
        &self[a]
    }
}
