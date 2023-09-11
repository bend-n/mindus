#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LVar<'string> {
    Null,
    Num(f64),
    String(&'string str),
}

#[derive(Copy, Clone)]
pub enum LAddress<'str> {
    Const(LVar<'str>),
    Address(usize, Priv),
}

impl LAddress<'_> {
    /// # Safety
    ///
    /// you must make sure that addr is in bounds of the memory.
    pub(crate) const unsafe fn addr(addr: usize) -> Self {
        LAddress::Address(addr, Priv { _priv: () })
    }
}

#[derive(Copy, Clone)]
pub struct Priv {
    _priv: (),
}

impl std::fmt::Debug for LAddress<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Const(c) => write!(f, "LAddress {c}"),
            Self::Address(n, ..) => write!(f, "LAddress {n:x}"),
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
            // SAFETY: addr constructor requires bounds
            LAddress::Address(n, ..) => unsafe { *self.0.get_unchecked(n) },
            LAddress::Const(n) => n,
        }
    }

    pub fn set(&mut self, a: LAddress<'s>, b: LAddress<'s>) -> bool {
        match a {
            LAddress::Const(_) => false,
            LAddress::Address(v, ..) => {
                match b {
                    LAddress::Const(n) => {
                        // SAFETY: v comes from Address, therefore safe
                        *unsafe { self.0.get_unchecked_mut(v) } = n;
                    }
                    LAddress::Address(n, ..) => {
                        // SAFETY: n comes from Address, therefore safe
                        let b = *unsafe { self.0.get_unchecked(n) };
                        // SAFETY: v comes from Addr, therefore safe
                        *unsafe { self.0.get_unchecked_mut(v) } = b;
                    }
                };
                true
            }
        }
    }

    pub fn get_mut(&mut self, a: LAddress<'s>) -> Option<&mut LVar<'s>> {
        match a {
            LAddress::Const(_) => None,
            // SAFETY: addr constructor requires bounds
            LAddress::Address(n, ..) => Some(unsafe { self.0.get_unchecked_mut(n) }),
        }
    }
}
