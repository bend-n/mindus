#[derive(Copy, Clone, Debug, PartialEq)]
pub enum LVar<'string> {
    Null,
    Num(f64),
    String(&'string str),
}

#[derive(Copy, Clone)]
pub enum LAddress<'varname> {
    Const(LVar<'varname>),
    Name(&'varname str),
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

struct LVarWrap<'varname> {
    name: &'varname str,
    inner: LVar<'varname>,
}

/// cleared every loop
#[derive(Default)]
pub struct LRegistry<'varnames>(Vec<LVarWrap<'varnames>>);

impl std::fmt::Debug for LRegistry<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_map()
            .entries(self.0.iter().map(|v| (v.name, v.inner)))
            .finish()
    }
}

impl<'s> LRegistry<'s> {
    pub fn clear(&mut self) {
        self.0.clear();
    }

    pub fn get(&self, a: LAddress<'s>) -> LVar<'s> {
        match a {
            LAddress::Name(n) => self.get_by_name(n),
            LAddress::Const(n) => n,
        }
    }

    pub fn get_by_name(&self, adr: &'s str) -> LVar<'s> {
        self.0
            .iter()
            .find(|v| v.name == adr)
            .map(|v| &v.inner)
            .copied()
            .unwrap_or(LVar::Null)
    }

    pub fn get_mut(&mut self, a: LAddress<'s>) -> Option<&mut LVar<'s>> {
        match a {
            LAddress::Const(_) => None,
            LAddress::Name(n) => Some(self.get_mut_by_name(n)),
        }
    }

    pub fn get_mut_by_name(&mut self, adr: &'s str) -> &mut LVar<'s> {
        // SAFETY: give new lifetime
        for x in unsafe { &mut *(&mut self.0 as *mut Vec<LVarWrap>) }.iter_mut() {
            if x.name == adr {
                return &mut x.inner;
            }
        }

        self.0.push(LVarWrap {
            name: adr,
            inner: LVar::Null,
        });
        &mut self.0.last_mut().unwrap().inner
    }
}
