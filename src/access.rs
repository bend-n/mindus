//! Similar to Cow but doesn't require ToOwned
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};

use std::ops::Deref;

pub type BoxAccess<'a, D> = Access<'a, Box<D>, D>;

#[derive(Clone, Debug)]
/// Similar to Cow but doesn't require ToOwned
pub enum Access<'a, T: AsRef<B>, B: ?Sized> {
    Borrowed(&'a B),
    Owned(T),
}

impl<'a, T: AsRef<B>, B> Access<'a, T, B> {
    pub const fn is_borrowed(&self) -> bool {
        matches!(self, Self::Borrowed(..))
    }

    pub const fn is_owned(&self) -> bool {
        matches!(self, Self::Owned(..))
    }
}

impl<'a, T: AsRef<B>, B: ?Sized> From<T> for Access<'a, T, B> {
    fn from(value: T) -> Self {
        Self::Owned(value)
    }
}

impl<'a, T: AsRef<B>, B: ?Sized> AsRef<B> for Access<'a, T, B> {
    fn as_ref(&self) -> &B {
        self
    }
}

impl<'a, T: AsRef<B>, B: ?Sized> Borrow<B> for Access<'a, T, B> {
    fn borrow(&self) -> &B {
        match self {
            Self::Borrowed(r) => r,
            Self::Owned(v) => v.as_ref(),
        }
    }
}

impl<'a, T: AsRef<B> + Default, B: ?Sized> Default for Access<'a, T, B> {
    fn default() -> Self {
        Self::Owned(T::default())
    }
}

impl<'a, T: AsRef<B>, B: ?Sized> Deref for Access<'a, T, B> {
    type Target = B;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Borrowed(r) => r,
            Self::Owned(v) => v.as_ref(),
        }
    }
}

impl<'a, T: AsRef<B>, B: ?Sized + fmt::Display> fmt::Display for Access<'a, T, B> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        B::fmt(self, f)
    }
}

impl<'a, T: AsRef<B>, B: ?Sized + Eq> Eq for Access<'a, T, B> {}

impl<'a, T: AsRef<B>, B: ?Sized + Hash> Hash for Access<'a, T, B> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        B::hash(self, state);
    }
}

impl<'a, T: AsRef<B>, B: ?Sized + Ord> Ord for Access<'a, T, B> {
    fn cmp(&self, other: &Self) -> Ordering {
        B::cmp(self, other)
    }
}

impl<'a, 'b, T: AsRef<B>, B: ?Sized + PartialEq<C>, U: AsRef<C>, C: ?Sized>
    PartialEq<Access<'b, U, C>> for Access<'a, T, B>
{
    fn eq(&self, other: &Access<'b, U, C>) -> bool {
        B::eq(self, other)
    }
}

impl<'a, 'b, T: AsRef<B>, B: ?Sized + PartialOrd<C>, U: AsRef<C>, C: ?Sized>
    PartialOrd<Access<'b, U, C>> for Access<'a, T, B>
{
    fn partial_cmp(&self, other: &Access<'b, U, C>) -> Option<Ordering> {
        B::partial_cmp(self, other)
    }
}
