use fast_image_resize as fr;
use std::{num::NonZeroU32, slice::SliceIndex};

mod affine;
use affine::*;
mod holder;
mod overlay;
pub use holder::*;
pub use overlay::*;

pub trait RepeatNew {
    type Output;
    /// Repeat self till it fills a new image of size x, y
    /// # Safety
    ///
    /// UB if self's width is not a multiple of x, or self's height is not a multiple of y
    unsafe fn repeated(&self, x: u32, y: u32) -> Self::Output;
}

pub trait ImageUtils {
    /// Tint this image with the color
    fn tint(&mut self, color: (u8, u8, u8)) -> &mut Self;
    /// rotate (squares only)
    /// # Safety
    ///
    /// UB if image is not square
    unsafe fn rotate(&mut self, times: u8) -> &mut Self;
    /// flip along the horizontal axis
    fn flip_h(&mut self) -> &mut Self;
    /// flip along the vertical axis
    fn flip_v(&mut self) -> &mut Self;
    /// shadow
    fn shadow(&mut self) -> &mut Self;
    /// scale a image
    fn scale(self, to: u32) -> Image<Vec<u8>, 4>;
}

macro_rules! assert_unchecked {
    ($cond:expr) => {{
        if !$cond {
            #[cfg(debug_assertions)]
            let _ = ::core::ptr::NonNull::<()>::dangling().as_ref(); // force unsafe wrapping block
            #[cfg(debug_assertions)]
            panic!("assertion failed: {} returned false", stringify!($cond));
            #[cfg(not(debug_assertions))]
            std::hint::unreachable_unchecked()
        }
    }};
}
use assert_unchecked;

impl RepeatNew for Image<&[u8], 3> {
    type Output = Image<Vec<u8>, 3>;
    unsafe fn repeated(&self, x: u32, y: u32) -> Self::Output {
        let mut img = Image::alloc(x, y); // could probably optimize this a ton but eh
        for x in 0..(x / self.width()) {
            for y in 0..(y / self.height()) {
                let a: &mut Image<&mut [u8], 3> = &mut img.as_mut();
                // SAFETY: caller upholds
                unsafe { a.overlay_at(self, x * self.width(), y * self.height()) };
            }
        }
        img
    }
}

impl ImageUtils for Image<&mut [u8], 4> {
    unsafe fn rotate(&mut self, times: u8) -> &mut Self {
        match times {
            2 => rot_180(self),
            1 => unsafe { rot_90(self) },
            3 => unsafe { rot_270(self) },
            _ => {}
        }
        self
    }

    fn tint(&mut self, (r, g, b): (u8, u8, u8)) -> &mut Self {
        let [tr, tg, tb] = [r as f32 / 255.0, g as f32 / 255.0, b as f32 / 255.0];
        for [r, g, b, _] in self.buffer.array_chunks_mut::<4>() {
            *r = (*r as f32 * tr) as u8;
            *g = (*g as f32 * tg) as u8;
            *b = (*b as f32 * tb) as u8;
        }
        self
    }

    // this function is very cold but im removing image so might as well use fir
    fn scale(self, to: u32) -> Image<Vec<u8>, 4> {
        let from =
            fr::Image::from_slice_u8(self.width, self.height, self.buffer, fr::PixelType::U8x4)
                .unwrap();
        let to = to.try_into().unwrap();
        let mut dst = fr::Image::new(to, to, fr::PixelType::U8x4);
        fr::Resizer::new(fr::ResizeAlg::Nearest)
            .resize(&from.view(), &mut dst.view_mut())
            .unwrap();
        Image::new(to, to, dst.into_vec())
    }

    fn shadow(&mut self) -> &mut Self {
        let mut shadow: Image<Vec<u8>, 4> =
            Image::new(self.width, self.height, self.buffer.to_vec());
        for [r, g, b, a] in shadow.buffer.array_chunks_mut() {
            if *a < 128 {
                *r /= 10;
                *g /= 10;
                *b /= 10;
            }
        }
        blurslice::gaussian_blur_bytes::<4>(
            &mut shadow.buffer,
            self.width() as usize,
            self.height() as usize,
            9.0,
        )
        .unwrap();
        for ([r, g, b, a], &[from_r, from_g, from_b, from_a]) in self
            .buffer
            .array_chunks_mut()
            .zip(shadow.buffer.array_chunks())
        {
            if *a == 0 {
                (*r, *g, *b, *a) = (from_r, from_g, from_b, from_a);
            }
        }
        self
    }

    #[inline]
    fn flip_h(&mut self) -> &mut Self {
        flip_h(self);
        self
    }

    #[inline(always)]
    fn flip_v(&mut self) -> &mut Self {
        flip_v(self);
        self
    }
}

#[inline]
unsafe fn really_unsafe_index(x: u32, y: u32, w: u32) -> usize {
    // y * w + x
    let tmp = unsafe { (y as usize).unchecked_mul(w as usize) };
    unsafe { tmp.unchecked_add(x as usize) }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Image<T, const CHANNELS: usize> {
    pub buffer: T,
    pub width: NonZeroU32,
    pub height: NonZeroU32,
}

impl<const CHANNELS: usize> Default for Image<&'static [u8], CHANNELS> {
    fn default() -> Self {
        Self {
            buffer: &[0; CHANNELS],
            width: NonZeroU32::new(1).unwrap(),
            height: NonZeroU32::new(1).unwrap(),
        }
    }
}

impl<T, const CHANNELS: usize> Image<T, CHANNELS> {
    #[inline]
    pub fn height(&self) -> u32 {
        self.height.into()
    }

    #[inline]
    pub fn width(&self) -> u32 {
        self.width.into()
    }

    #[inline]
    pub const fn new(width: NonZeroU32, height: NonZeroU32, buffer: T) -> Self {
        Image {
            width,
            height,
            buffer,
        }
    }
}

impl<const CHANNELS: usize> Image<&[u8], CHANNELS> {
    #[inline]
    pub const fn copy(&self) -> Self {
        Self {
            width: self.width,
            height: self.height,
            buffer: self.buffer,
        }
    }
}

impl<T: std::ops::Deref<Target = [u8]>, const CHANNELS: usize> Image<T, CHANNELS> {
    /// # Safety
    ///
    /// - UB if x, y is out of bounds
    /// - UB if buffer is too small
    #[inline]
    pub unsafe fn slice(&self, x: u32, y: u32) -> impl SliceIndex<[u8], Output = [u8]> {
        debug_assert!(x < self.width(), "x out of bounds");
        debug_assert!(y < self.height(), "y out of bounds");
        let index = unsafe { really_unsafe_index(x, y, self.width()) };
        let index = unsafe { index.unchecked_mul(CHANNELS) };
        debug_assert!(self.buffer.len() > index);
        index..unsafe { index.unchecked_add(CHANNELS) }
    }

    #[inline]
    pub fn chunked(&self) -> impl Iterator<Item = &[u8; CHANNELS]> {
        // SAFETY: 0 sized images illegal
        unsafe { assert_unchecked!(self.buffer.len() > CHANNELS) };
        // SAFETY: no half pixels!
        unsafe { assert_unchecked!(self.buffer.len() % CHANNELS == 0) };
        self.buffer.array_chunks::<CHANNELS>()
    }

    /// Return a pixel at (x, y).
    /// # Safety
    ///
    /// Refer to [`slice`]
    #[inline]
    pub unsafe fn pixel(&self, x: u32, y: u32) -> [u8; CHANNELS] {
        let idx = unsafe { self.slice(x, y) };
        let ptr = unsafe { self.buffer.get_unchecked(idx).as_ptr().cast() };
        unsafe { *ptr }
    }
}

impl<T: std::ops::DerefMut<Target = [u8]>, const CHANNELS: usize> Image<T, CHANNELS> {
    /// Return a mutable reference to a pixel at (x, y).
    /// # Safety
    ///
    /// Refer to [`slice`]
    #[inline]
    pub unsafe fn pixel_mut(&mut self, x: u32, y: u32) -> &mut [u8] {
        let idx = unsafe { self.slice(x, y) };
        unsafe { self.buffer.get_unchecked_mut(idx) }
    }

    #[inline]
    pub fn chunked_mut(&mut self) -> impl Iterator<Item = &mut [u8; CHANNELS]> {
        // SAFETY: 0 sized images are not allowed
        unsafe { assert_unchecked!(self.buffer.len() > CHANNELS) };
        // SAFETY: buffer cannot have half pixels
        unsafe { assert_unchecked!(self.buffer.len() % CHANNELS == 0) };
        self.buffer.array_chunks_mut::<CHANNELS>()
    }

    /// Set the pixel at x, y
    ///
    /// # Safety
    ///
    /// UB if x, y is out of bounds.
    #[inline]
    pub unsafe fn set_pixel(&mut self, x: u32, y: u32, px: [u8; CHANNELS]) {
        // SAFETY: Caller says that x, y is in bounds
        let out = unsafe { self.pixel_mut(x, y) };
        // SAFETY: px must be CHANNELS long
        unsafe { std::ptr::copy_nonoverlapping(px.as_ptr(), out.as_mut_ptr(), CHANNELS) };
    }
}

impl<const CHANNELS: usize> Image<Vec<u8>, CHANNELS> {
    pub fn alloc(width: u32, height: u32) -> Self {
        Image {
            width: width.try_into().unwrap(),
            height: height.try_into().unwrap(),
            buffer: vec![0; CHANNELS * width as usize * height as usize],
        }
    }

    pub fn as_ref(&self) -> Image<&[u8], CHANNELS> {
        Image::new(self.width, self.height, &self.buffer)
    }

    pub fn as_mut(&mut self) -> Image<&mut [u8], CHANNELS> {
        Image::new(self.width, self.height, &mut self.buffer)
    }
}

impl Image<Vec<u8>, 3> {
    #[cfg(feature = "bin")]
    pub fn save(&self, f: impl AsRef<std::path::Path>) {
        let p = std::fs::File::create(f).unwrap();
        let w = &mut std::io::BufWriter::new(p);
        let mut enc = png::Encoder::new(w, self.width(), self.height());
        enc.set_color(png::ColorType::Rgb);
        enc.set_depth(png::BitDepth::Eight);
        enc.set_source_gamma(png::ScaledFloat::new(1.0 / 2.2));
        enc.set_source_chromaticities(png::SourceChromaticities::new(
            (0.31270, 0.32900),
            (0.64000, 0.33000),
            (0.30000, 0.60000),
            (0.15000, 0.06000),
        ));
        let mut writer = enc.write_header().unwrap();
        writer.write_image_data(&self.buffer).unwrap();
    }
}

#[cfg(test)]
macro_rules! img {
    [[$($v:literal),+] [$($v2:literal),+]] => {{
        let from: Image<Vec<u8>, 1> = Image::new(
            2.try_into().unwrap(),
            2.try_into().unwrap(),
            vec![$($v,)+ $($v2,)+]
        );
        from
    }}
}
#[cfg(test)]
pub(self) use img;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn scale() {
        let mut from = Image::alloc(6, 6);
        unsafe { from.set_pixel(3, 3, [255, 255, 255, 255]) };
        let from = from.as_mut().scale(12);
        assert_eq!(unsafe { from.pixel(6, 6) }, [255, 255, 255, 255]);
    }
}

pub fn blend(bg: &mut [u8; 4], fg: [u8; 4]) {
    if fg[3] == 0 {
        return;
    }
    if fg[3] == 255 {
        *bg = fg;
        return;
    }
    let bg_a = bg[3] as f32 / 255.0;
    let fg_a = fg[3] as f32 / 255.0;
    let a = bg_a + fg_a - bg_a * fg_a;
    if a == 0.0 {
        return;
    };
    *bg = [
        (255.0
            * ((((fg[0] as f32 / 255.0) * fg_a) + ((bg[0] as f32 / 255.0) * bg_a) * (1.0 - fg_a))
                / a)) as u8,
        (255.0
            * ((((fg[1] as f32 / 255.0) * fg_a) + ((bg[1] as f32 / 255.0) * bg_a) * (1.0 - fg_a))
                / a)) as u8,
        (255.0
            * ((((fg[2] as f32 / 255.0) * fg_a) + ((bg[2] as f32 / 255.0) * bg_a) * (1.0 - fg_a))
                / a)) as u8,
        (255.0 * a) as u8,
    ]
}
