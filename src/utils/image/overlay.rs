use super::{really_unsafe_index, unsafe_assert, Image};
use std::simd::SimdInt;
use std::simd::SimdPartialOrd;
use std::simd::{simd_swizzle, Simd};

pub trait OverlayAt<W> {
    /// Overlay with => self at coordinates x, y, without blending
    /// # Safety
    ///
    /// UB if x, y is out of bounds
    unsafe fn overlay_at(&mut self, with: &W, x: u32, y: u32) -> &mut Self;
}

pub trait Overlay<W> {
    /// Overlay with => self (does not blend)
    /// # Safety
    ///
    /// UB if a.width != b.width || a.height != b.height
    unsafe fn overlay(&mut self, with: &W) -> &mut Self;
}

#[inline]
pub unsafe fn blit(rgb: &mut [u8], rgba: &[u8]) {
    const LAST4: Simd<u8, 16> = Simd::from_array([
        255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 0, 0, 0,
    ]);

    let mut srci = 0;
    let mut dsti = 0;
    while dsti + 16 <= rgb.len() {
        let old: Simd<u8, 16> = Simd::from_slice(rgb.get_unchecked(dsti..dsti + 16));
        let new: Simd<u8, 16> = Simd::from_slice(rgba.get_unchecked(srci..srci + 16));

        let threshold = new.simd_ge(Simd::splat(128)).to_int().cast::<u8>();
        let mut mask = simd_swizzle!(
            threshold,
            [3, 3, 3, 7, 7, 7, 11, 11, 11, 15, 15, 15, 0, 0, 0, 0]
        );
        mask &= LAST4;

        let new_rgb = simd_swizzle!(new, [0, 1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 14, 0, 0, 0, 0]);
        let blended = (new_rgb & mask) | (old & !mask);
        blended.copy_to_slice(rgb.get_unchecked_mut(dsti..dsti + 16));

        srci += 16;
        dsti += 12;
    }

    while dsti + 3 <= rgb.len() {
        if *rgba.get_unchecked(srci + 3) >= 128 {
            std::ptr::copy_nonoverlapping(
                rgba.get_unchecked(srci..srci + 3).as_ptr(),
                rgb.get_unchecked_mut(dsti..dsti + 3).as_mut_ptr(),
                3,
            );
        }

        srci += 4;
        dsti += 3;
    }
}

impl OverlayAt<Image<&[u8], 4>> for Image<&mut [u8], 3> {
    unsafe fn overlay_at(&mut self, with: &Image<&[u8], 4>, x: u32, y: u32) -> &mut Self {
        unsafe_assert!(x + with.width() <= self.width());
        unsafe_assert!(y + with.height() <= self.height());
        for j in 0..with.height() {
            let i_x = j as usize * with.width() as usize * 4
                ..(j as usize + 1) * with.width() as usize * 4;
            let o_x = ((j as usize + y as usize) * self.width() as usize + x as usize) * 3
                ..((j as usize + y as usize) * self.width() as usize
                    + x as usize
                    + with.width() as usize)
                    * 3;
            blit(
                self.buffer.get_unchecked_mut(o_x),
                with.buffer.get_unchecked(i_x),
            )
        }
        self
    }
}

impl Overlay<Image<&[u8], 4>> for Image<&mut [u8], 3> {
    unsafe fn overlay(&mut self, with: &Image<&[u8], 4>) -> &mut Self {
        unsafe_assert!(self.width() == with.width());
        unsafe_assert!(self.height() == with.height());
        for (i, chunk) in with
            .buffer
            .chunks_exact(with.width() as usize * 4)
            .enumerate()
        {
            blit(
                self.buffer.get_unchecked_mut(
                    i * with.width() as usize * 3..(i + 1) * with.width() as usize * 3,
                ),
                chunk,
            );
        }
        self
    }
}

impl OverlayAt<Image<&[u8], 4>> for Image<&mut [u8], 4> {
    unsafe fn overlay_at(&mut self, with: &Image<&[u8], 4>, x: u32, y: u32) -> &mut Self {
        for j in 0..with.height() {
            for i in 0..with.width() {
                let with_index = really_unsafe_index(i, j, with.width()).unchecked_mul(4);
                let their_px = with
                    .buffer
                    .get_unchecked(with_index..with_index.unchecked_add(4));
                if *their_px.get_unchecked(3) >= 128 {
                    let our_index =
                        really_unsafe_index(i.unchecked_add(x), j.unchecked_add(y), self.width())
                            .unchecked_mul(4);
                    let our_px = self
                        .buffer
                        .get_unchecked_mut(our_index..our_index.unchecked_add(4));
                    std::ptr::copy_nonoverlapping(their_px.as_ptr(), our_px.as_mut_ptr(), 4);
                }
            }
        }

        self
    }
}

#[cfg(test)]
mod bench {
    extern crate test;
    use test::{black_box, Bencher};

    use super::*;
    use crate::{data::renderer::Scale, load};

    #[bench]
    fn overlay_4on3at(bench: &mut Bencher) {
        let mut v = vec![0u8; 3 * 56 * 56];
        let mut a: Image<_, 3> = Image::new(
            56.try_into().unwrap(),
            56.try_into().unwrap(),
            v.as_mut_slice(),
        );
        let b = load!("interplanetary-accelerator", Scale::Eigth);
        bench.iter(|| unsafe {
            black_box(a.overlay_at(&b.borrow(), 0, 0));
            black_box(a.overlay_at(&b.borrow(), 28, 0));
            black_box(a.overlay_at(&b.borrow(), 28, 28));
            black_box(a.overlay_at(&b.borrow(), 0, 28));
        });
    }

    #[bench]
    fn overlay_4on4at(bench: &mut Bencher) {
        let mut v = vec![0u8; 4 * 56 * 56];
        let mut a: Image<_, 4> = Image::new(
            56.try_into().unwrap(),
            56.try_into().unwrap(),
            v.as_mut_slice(),
        );
        let b = load!("interplanetary-accelerator", Scale::Eigth);
        bench.iter(|| unsafe {
            black_box(a.overlay_at(&b.borrow(), 0, 0));
            black_box(a.overlay_at(&b.borrow(), 28, 0));
            black_box(a.overlay_at(&b.borrow(), 28, 28));
            black_box(a.overlay_at(&b.borrow(), 0, 28));
        });
    }
}
