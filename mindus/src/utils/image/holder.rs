use super::{Image, ImageUtils, Overlay, OverlayAt};
#[derive(Clone, Debug)]
pub enum ImageHolder<const CHANNELS: usize> {
    Borrow(Image<&'static [u8], CHANNELS>),
    Own(Image<Vec<u8>, CHANNELS>),
}

impl<const CHANNELS: usize> ImageHolder<CHANNELS> {
    #[must_use]
    pub fn own(self) -> Image<Vec<u8>, CHANNELS> {
        match self {
            Self::Own(x) => x,
            Self::Borrow(x) => x.to_owned(),
        }
    }
}

impl<const CHANNELS: usize> ImageHolder<CHANNELS> {
    #[must_use]
    #[inline]
    pub fn borrow(&self) -> Image<&[u8], CHANNELS> {
        match self {
            Self::Own(x) => x.as_ref(),
            Self::Borrow(x) => x.clone(),
        }
    }

    #[must_use]
    #[inline]
    pub fn borrow_mut(&mut self) -> Image<&mut [u8], CHANNELS> {
        match self {
            Self::Own(x) => x.as_mut(),
            Self::Borrow(x) => {
                *self = Self::from(x.to_owned());
                self.borrow_mut()
            }
        }
    }
}

impl OverlayAt<ImageHolder<4>> for ImageHolder<4> {
    unsafe fn overlay_at(&mut self, with: &ImageHolder<4>, x: u32, y: u32) -> &mut Self {
        // SAFETY: this is basically a deref impl, the caller upholds the safety invariants
        unsafe { self.borrow_mut().overlay_at(&with.borrow(), x, y) };
        self
    }
}

impl Overlay<ImageHolder<4>> for ImageHolder<4> {
    unsafe fn overlay(&mut self, with: &Self) -> &mut Self {
        unsafe { self.borrow_mut().overlay(&with.borrow()) };
        self
    }
}

impl ImageUtils for ImageHolder<4> {
    fn tint(&mut self, color: (u8, u8, u8)) -> &mut Self {
        self.borrow_mut().tint(color);
        self
    }

    unsafe fn rotate(&mut self, times: u8) -> &mut Self {
        if times == 0 {
            return self;
        }
        // borrow mut may clone, so try to avoid
        unsafe { self.borrow_mut().rotate(times) };
        self
    }

    fn shadow(&mut self) -> &mut Self {
        self.borrow_mut().shadow();
        self
    }
}

impl<const CHANNELS: usize> From<Image<&'static [u8], CHANNELS>> for ImageHolder<CHANNELS> {
    fn from(value: Image<&'static [u8], CHANNELS>) -> Self {
        Self::Borrow(value)
    }
}

impl<const CHANNELS: usize> From<Image<Vec<u8>, CHANNELS>> for ImageHolder<CHANNELS> {
    fn from(value: Image<Vec<u8>, CHANNELS>) -> Self {
        Self::Own(value)
    }
}
