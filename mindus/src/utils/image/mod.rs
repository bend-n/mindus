pub use fimg::*;

mod holder;
pub use holder::*;

pub trait ImageUtils {
    /// Tint this image with the color
    fn tint(&mut self, color: (u8, u8, u8)) -> &mut Self;
    /// rotate (squares only)
    /// # Safety
    ///
    /// UB if image is not square
    unsafe fn rotate(&mut self, times: u8) -> &mut Self;
    unsafe fn rotated(mut self, times: u8) -> Self
    where
        Self: Sized,
    {
        unsafe { self.rotate(times) };
        self
    }
    /// shadow
    fn shadow(&mut self) -> &mut Self;
}

impl<T: AsMut<[u8]> + AsRef<[u8]>> ImageUtils for Image<T, 4> {
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
        let mut shadow = self.as_ref().boxed();
        for [r, g, b, a] in shadow.chunked_mut() {
            if *a < 128 {
                *r /= 10;
                *g /= 10;
                *b /= 10;
            }
        }
        shadow.blur(22);
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

#[test]
fn x() {
    let mut x = Image::<_, 4>::open("/home/os/pod.png");
    x.as_mut().shadow();
    x.save("/home/os/shadowpod.png");
}
