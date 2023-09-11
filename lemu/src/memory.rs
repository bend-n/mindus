#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LVar<'string> {
    Null,
    Num(f64),
    String(&'string str),
}

#[derive(Copy, Clone)]
pub enum LAddress<'str> {
    Const(LVar<'str>),
    Address(usize),
}

impl std::fmt::Debug for LAddress<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Const(c) => write!(f, "LAddress {c}"),
            Self::Address(n) => write!(f, "LAddress {n:x}"),
        }
    }
}

impl std::fmt::Display for LVar<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "null"),
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

impl<'s> From<&'s str> for LVar<'s> {
    fn from(value: &'s str) -> Self {
        Self::String(value)
    }
}

/// cleared every loop
#[derive(Default, Debug)]
pub struct LRegistry<'str>(Box<[LVar<'str>]>);

impl<'s> LRegistry<'s> {
    pub fn new(size: usize) -> Self {
        Self(vec![LVar::Null; size].into_boxed_slice())
    }

    pub fn clear(&mut self) {
        for var in self.0.iter_mut() {
            *var = LVar::Null
        }
    }

    pub fn get(&self, a: LAddress<'s>) -> LVar<'s> {
        match a {
            LAddress::Address(n) => self.get_by_index(n),
            LAddress::Const(n) => n,
        }
    }

    pub fn get_by_index(&self, n: usize) -> LVar<'s> {
        self.0[n]
    }

    pub fn get_mut(&mut self, a: LAddress<'s>) -> Option<&mut LVar<'s>> {
        match a {
            LAddress::Const(_) => None,
            LAddress::Address(n) => Some(self.get_mut_by_index(n)),
        }
    }

    pub fn get_mut_by_index(&mut self, index: usize) -> &mut LVar<'s> {
        &mut self.0[index]
    }
}
