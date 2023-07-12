use std::{
    fmt::{Debug, Write},
    ops::Deref,
};
#[derive(Clone, PartialEq, Eq)]
pub struct Array2D<T: Clone> {
    width: usize,
    height: usize,
    /// column
    data: Box<[T]>,
}

impl<T: Debug + Clone> Debug for Array2D<Option<T>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Arr[\n")?;
        for y in (0..self.height).rev() {
            for x in 0..self.width {
                let t = &self[x][y];
                if let Some(t) = t {
                    t.fmt(f)?;
                } else {
                    f.write_char('_')?;
                }
                f.write_str(", ")?;
            }
            f.write_char('\n')?;
        }
        f.write_char(']')?;
        Ok(())
    }
}

impl<T: Clone> Array2D<T> {
    pub fn new(fill: T, width: usize, height: usize) -> Array2D<T> {
        Array2D {
            width,
            height,
            data: vec![fill; width * height].into_boxed_slice(),
        }
    }
}

impl<T: Clone> Deref for Array2D<T> {
    type Target = Box<[T]>;
    /// a sin it is
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T: Clone> std::ops::Index<usize> for Array2D<T> {
    type Output = [T];

    fn index(&self, x: usize) -> &Self::Output {
        &self.data[self.height * x..self.height * (x + 1)]
    }
}

impl<T: Clone> std::ops::IndexMut<usize> for Array2D<T> {
    fn index_mut(&mut self, x: usize) -> &mut Self::Output {
        &mut self.data[self.height * x..self.height * (x + 1)]
    }
}
