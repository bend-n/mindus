use super::Image;

#[derive(Clone, Debug)]
pub enum Cow {
    Ref(&'static [u8]),
    Own(Box<[u8]>),
}
impl From<&'static [u8]> for Cow {
    fn from(value: &'static [u8]) -> Self {
        Self::Ref(value)
    }
}
impl From<Box<[u8]>> for Cow {
    fn from(value: Box<[u8]>) -> Self {
        Self::Own(value)
    }
}

impl Cow {
    pub fn own(self) -> Box<[u8]> {
        match self {
            Self::Own(x) => x,
            Self::Ref(x) => x.into(),
        }
    }
}
impl AsRef<[u8]> for Cow {
    fn as_ref(&self) -> &[u8] {
        match self {
            Cow::Ref(x) => x,
            Cow::Own(x) => &x,
        }
    }
}

impl AsMut<[u8]> for Cow {
    fn as_mut(&mut self) -> &mut [u8] {
        match self {
            Cow::Ref(x) => {
                *self = Self::Own((*x).into());
                self.as_mut()
            }
            Cow::Own(x) => x,
        }
    }
}

pub type ImageHolder<const CHANNELS: usize> = Image<Cow, CHANNELS>;
