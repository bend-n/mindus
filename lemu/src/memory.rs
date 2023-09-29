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

impl LVar<'_> {
    // get null
    pub const fn null() -> LVar<'static> {
        LVar::Num(0.0)
    }
}

#[derive(Clone)]
pub enum LAddress<'str> {
    Const(LVar<'str>),
    Address(usize, &'str str, Priv),
}

impl<'v> LAddress<'v> {
    /// # Safety
    ///
    /// you must make sure that addr is in bounds of the memory.
    pub(crate) const unsafe fn addr(addr: usize, name: &'v str) -> Self {
        LAddress::Address(addr, name, Priv { _priv: () })
    }

    pub(crate) fn cnst(c: impl Into<LVar<'v>>) -> Self {
        Self::Const(c.into())
    }
}

#[derive(Copy, Clone)]
pub struct Priv {
    _priv: (),
}

impl std::fmt::Debug for LAddress<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Const(c) => write!(f, "{c}"),
            Self::Address(n, name, ..) => write!(f, "{name}@0x{n:x}"),
        }
    }
}

impl std::fmt::Display for LAddress<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Const(c) => write!(f, "{c}"),
            Self::Address(_, n, ..) => write!(f, "{n}"),
        }
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
#[derive(Default, Debug)]
pub struct LRegistry<'str>(Box<[LVar<'str>]>);

impl<'s> LRegistry<'s> {
    pub fn new(size: usize) -> Self {
        Self(vec![LVar::null(); size].into_boxed_slice())
    }

    pub fn clear(&mut self) {
        for var in &mut *self.0 {
            *var = LVar::null();
        }
    }

    pub fn get<'a>(&'a self, a: &'a LAddress<'s>) -> &LVar<'s> {
        match a {
            // SAFETY: addr constructor requires bounds
            LAddress::Address(n, ..) => unsafe { self.0.get_unchecked(*n) },
            LAddress::Const(n) => n,
        }
    }

    pub fn set(&mut self, a: &LAddress<'s>, b: LAddress<'s>) -> bool {
        match a {
            LAddress::Const(_) => false,
            LAddress::Address(v, ..) => {
                match b {
                    LAddress::Const(n) => {
                        // SAFETY: v comes from Address, therefore safe
                        *unsafe { self.0.get_unchecked_mut(*v) } = n;
                    }
                    LAddress::Address(n, ..) => {
                        // SAFETY: n comes from Address, therefore safe
                        let b = unsafe { self.0.get_unchecked(n).clone() };
                        // SAFETY: v comes from Addr, therefore safe
                        *unsafe { self.0.get_unchecked_mut(*v) } = b;
                    }
                };
                true
            }
        }
    }

    pub fn get_mut(&mut self, a: &LAddress<'s>) -> Option<&mut LVar<'s>> {
        match a {
            LAddress::Const(_) => None,
            // SAFETY: addr constructor requires bounds
            LAddress::Address(n, ..) => Some(unsafe { self.0.get_unchecked_mut(*n) }),
        }
    }
}
