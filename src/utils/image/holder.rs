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
            Self::Borrow(x) => Image::new(x.width, x.height, x.buffer.to_vec()),
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
            Self::Own(x) => Image::new(x.width, x.height, &mut x.buffer),
            Self::Borrow(_) => {
                *self = Self::from(std::mem::replace(self, Self::from(Image::default())).own());
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

    fn flip_h(&mut self) -> &mut Self {
        self.borrow_mut().flip_h();
        self
    }

    fn flip_v(&mut self) -> &mut Self {
        self.borrow_mut().flip_v();
        self
    }

    fn shadow(&mut self) -> &mut Self {
        self.borrow_mut().shadow();
        self
    }

    fn scale(mut self, to: u32) -> Image<Vec<u8>, 4> {
        self.borrow_mut().scale(to)
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
