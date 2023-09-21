pub use fimg::*;

mod holder;
pub use holder::ImageHolder;

pub trait ImageUtils {
    /// Tint this image with the color
    fn tint(&mut self, color: (u8, u8, u8)) -> &mut Self;
    /// rotate (squares only)
    /// # Safety
    ///
    /// UB if image is not square
    unsafe fn rotate(&mut self, times: u8) -> &mut Self;
    /// shadow
    fn shadow(&mut self) -> &mut Self;
}

impl ImageUtils for Image<&mut [u8], 4> {
    unsafe fn rotate(&mut self, times: u8) -> &mut Self {
        match times {
            2 => self.rot_180(),
            1 => unsafe { self.rot_90() },
            3 => unsafe { self.rot_270() },
            _ => {}
        }
        self
    }

    fn tint(&mut self, (r, g, b): (u8, u8, u8)) -> &mut Self {
        let [tr, tg, tb] = [r as f32 / 255.0, g as f32 / 255.0, b as f32 / 255.0];
        for [r, g, b, _] in self.chunked_mut() {
            *r = (*r as f32 * tr) as u8;
            *g = (*g as f32 * tg) as u8;
            *b = (*b as f32 * tb) as u8;
        }
        self
    }

    fn shadow(&mut self) -> &mut Self {
        let mut shadow: Image<Vec<u8>, 4> = self.to_owned();
        for [r, g, b, a] in shadow.chunked_mut() {
            if *a < 128 {
                *r /= 10;
                *g /= 10;
                *b /= 10;
            }
        }
        blurslice::gaussian_blur_bytes::<4>(
            unsafe { shadow.buffer_mut() },
            self.width() as usize,
            self.height() as usize,
            9.0,
        )
        .unwrap();
        for ([r, g, b, a], &[from_r, from_g, from_b, from_a]) in
            self.chunked_mut().zip(shadow.chunked())
        {
            if *a == 0 {
                (*r, *g, *b, *a) = (from_r, from_g, from_b, from_a);
            }
        }
        self
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
