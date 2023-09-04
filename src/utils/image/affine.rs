use super::Image;

/// Rotate a image 180 degrees clockwise.
pub fn rot_180<const CHANNELS: usize>(img: &mut Image<&mut [u8], CHANNELS>) {
    for y in 0..img.height() / 2 {
        for x in 0..img.width() {
            let p = unsafe { img.pixel(x, y) };
            let x2 = img.width() - x - 1;
            let y2 = img.height() - y - 1;
            let p2 = unsafe { img.pixel(x2, y2) };
            unsafe { img.set_pixel(x, y, p2) };
            unsafe { img.set_pixel(x2, y2, p) };
        }
    }

    if img.height() % 2 != 0 {
        let middle = img.height() / 2;

        for x in 0..img.width() / 2 {
            let p = unsafe { img.pixel(x, middle) };
            let x2 = img.width() - x - 1;
            let p2 = unsafe { img.pixel(x2, middle) };
            unsafe { img.set_pixel(x, middle, p2) };
            unsafe { img.set_pixel(x2, middle, p) };
        }
    }
}

/// # Safety
///
/// UB if supplied image rectangular
unsafe fn transpose<const CHANNELS: usize>(img: &mut Image<&mut [u8], CHANNELS>) {
    debug_assert_eq!(img.width(), img.height());
    let size = img.width();
    for i in 0..size {
        for j in i..size {
            for c in 0..CHANNELS {
                // SAFETY: caller gurantees rectangularity
                unsafe {
                    img.buffer.swap_unchecked(
                        (i * size + j) as usize * CHANNELS + c,
                        (j * size + i) as usize * CHANNELS + c,
                    )
                };
            }
        }
    }
}

/// Rotate a image 90 degrees clockwise.
/// This is done by first [flipping vertically](flip_v), then [transposing](transpose) the image, to save allocations.
///
/// # Safety
///
/// UB if the image is not square
#[inline]
pub unsafe fn rot_90<const CHANNELS: usize>(img: &mut Image<&mut [u8], CHANNELS>) {
    flip_v(img);
    // SAFETY: caller ensures rectangularity
    unsafe { transpose(img) };
}

/// Rotate a image 270 degrees clockwise, or 90 degrees anti clockwise.
/// [horizontal flip](flip_h), then [transpose].
///
/// # Safety
///
/// UB if the image is not square
#[inline]
pub unsafe fn rot_270<const CHANNELS: usize>(img: &mut Image<&mut [u8], CHANNELS>) {
    flip_h(img);
    // SAFETY: caller ensures rectangularity
    unsafe { transpose(img) };
}

/// Flip a image vertically.
pub fn flip_v<const CHANNELS: usize>(img: &mut Image<&mut [u8], CHANNELS>) {
    for y in 0..img.height() / 2 {
        for x in 0..img.width() {
            let y2 = img.height() - y - 1;
            // SAFETY: within bounds
            let p2 = unsafe { img.pixel(x, y2) };
            let p = unsafe { img.pixel(x, y) };
            unsafe { img.set_pixel(x, y2, p) };
            unsafe { img.set_pixel(x, y, p2) };
        }
    }
}

/// Flip a image horizontally.
pub fn flip_h<const CHANNELS: usize>(img: &mut Image<&mut [u8], CHANNELS>) {
    for y in 0..img.height() {
        for x in 0..img.width() / 2 {
            let x2 = img.width() - x - 1;
            let p2 = unsafe { img.pixel(x2, y) };
            let p = unsafe { img.pixel(x, y) };
            unsafe { img.set_pixel(x2, y, p) };
            unsafe { img.set_pixel(x, y, p2) };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils::image::img;

    #[test]
    fn rotate_90() {
        let mut from = img![
            [00, 01]
            [02, 10]
        ];
        unsafe { rot_90(&mut from.as_mut()) };
        assert_eq!(
            from,
            img![
                [02, 00]
                [10, 01]
            ]
        );
    }

    #[test]
    fn rotate_180() {
        let mut from = img![
            [00, 01]
            [02, 10]
        ];
        rot_180(&mut from.as_mut());
        assert_eq!(
            from,
            img![
                [10, 02]
                [01, 00]
            ]
        );
    }

    #[test]
    fn rotate_270() {
        let mut from = img![
            [00, 01]
            [20, 10]
        ];
        unsafe { rot_270(&mut from.as_mut()) };
        assert_eq!(
            from,
            img![
                [01, 10]
                [00, 20]
            ]
        );
    }

    #[test]
    fn flip_vertical() {
        let mut from = img![
            [90, 01]
            [21, 42]
        ];
        flip_v(&mut from.as_mut());
        assert_eq!(
            from,
            img![
                [21, 42]
                [90, 01]
            ]
        )
    }
    #[test]
    fn flip_horizontal() {
        let mut from = img![
            [90, 01]
            [21, 42]
        ];
        flip_h(&mut from.as_mut());
        assert_eq!(
            from,
            img![
                [01, 90]
                [42, 21]
            ]
        )
    }
}
