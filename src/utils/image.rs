use fast_image_resize as fr;
use image::{GenericImageView, Rgb, Rgba, RgbaImage};
use std::num::NonZeroU32;
use blurslice::gaussian_blur_bytes;

pub trait ImageUtils {
    /// Tint this image with the color
    fn tint(&mut self, color: Rgb<u8>) -> &mut Self;
    /// Repeat with over self
    fn repeat(&mut self, with: &Self) -> &mut Self;
    /// Overlay with onto self (does not blend)
    fn overlay(&mut self, with: &Self, x: u32, y: u32) -> &mut Self;
    /// rotate
    fn rotate(&mut self, times: u8) -> &mut Self;
    /// shadow
    fn shadow(&mut self) -> &mut Self;
    /// silhouette
    fn silhouette(&mut self) -> &mut Self;
    /// scale a image
    ///
    /// SAFETY: to and width and height cannot be 0.
    unsafe fn scale(self, to: u32) -> Self;
}

impl ImageUtils for RgbaImage {
    fn rotate(&mut self, times: u8) -> &mut Self {
        use image::imageops::{rotate180, rotate270, rotate90};
        match times {
            2 => *self = rotate180(self),
            1 => *self = rotate90(self),
            3 => *self = rotate270(self),
            _ => {}
        }
        self
    }

    fn tint(&mut self, color: Rgb<u8>) -> &mut Self {
        let [tr, tg, tb] = [
            color[0] as f32 / 255.0,
            color[1] as f32 / 255.0,
            color[2] as f32 / 255.0,
        ];
        for Rgba([r, g, b, _]) in self.pixels_mut() {
            *r = (*r as f32 * tr) as u8;
            *g = (*g as f32 * tg) as u8;
            *b = (*b as f32 * tb) as u8;
        }
        self
    }

    fn repeat(&mut self, with: &RgbaImage) -> &mut Self {
        for x in 0..(self.width() / with.width()) {
            for y in 0..(self.height() / with.height()) {
                self.overlay(with, x * with.width(), y * with.height());
            }
        }
        self
    }

    fn overlay(&mut self, with: &RgbaImage, x: u32, y: u32) -> &mut Self {
        for j in 0..with.height() {
            for i in 0..with.width() {
                let get = with.get_pixel(i, j);
                if get[3] > 128 {
                    self.put_pixel(i + x, j + y, *get);
                }
            }
        }
        self
    }

    unsafe fn scale(self, to: u32) -> Self {
        debug_assert_ne!(to, 0);
        debug_assert_ne!(self.width(), 0);
        debug_assert_ne!(self.height(), 0);
        let to = NonZeroU32::new_unchecked(to);
        let src = fr::Image::from_vec_u8(
            NonZeroU32::new_unchecked(self.width()),
            NonZeroU32::new_unchecked(self.height()),
            self.into_vec(),
            fr::PixelType::U8x4,
        )
        .unwrap();
        let mut dst = fr::Image::new(to, to, fr::PixelType::U8x4);
        fr::Resizer::new(fr::ResizeAlg::Nearest)
            .resize(&src.view(), &mut dst.view_mut())
            .unwrap();
        RgbaImage::from_raw(to.get(), to.get(), dst.into_vec()).unwrap()
    }

    fn silhouette(&mut self) -> &mut Self {
        for pixel in self.pixels_mut() {
            if pixel[3] < 128 {
                pixel[2] /= 10;
                pixel[1] /= 10;
                pixel[0] /= 10;
            }
        }
        self
    }

    fn shadow(&mut self) -> &mut Self {
        let mut shadow = self.clone();
        shadow.silhouette();
        let samples = shadow.as_flat_samples_mut();
        gaussian_blur_bytes::<4>(samples.samples, self.width() as usize, self.height() as usize, 9.0).unwrap();
        for x in 0..shadow.width() {
            for y in 0..shadow.height() {
                let Rgba([r, g, b, a]) = self.get_pixel_mut(x, y);
                if *a == 0 {
                    // SAFETY: yes
                    let p = unsafe { shadow.unsafe_get_pixel(x, y) };
                    *r = p[0];
                    *g = p[0];
                    *b = p[0];
                    *a = p[1];
                }
            }
        }
        self
    }
}
