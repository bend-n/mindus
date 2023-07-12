use std::fmt;
use std::iter::{Enumerate, FusedIterator};
use std::marker::PhantomData;
use std::slice;

use crate::item;

#[derive(Clone, Debug, Eq)]
/// stores data
pub struct Storage<T> {
    base: Vec<u32>,
    total: u64,
    holds: PhantomData<T>,
}

pub type ItemStorage = Storage<item::Type>;

impl<T> Default for Storage<T> {
    fn default() -> Self {
        Self {
            base: Vec::default(),
            total: 0,
            holds: Default::default(),
        }
    }
}

impl<T> Storage<T>
where
    u16: From<T>,
{
    #[must_use]
    /// create a new storage
    ///
    /// ```
    /// # use mindus::item::storage::ItemStorage;
    /// // ItemStorage is a alias to Storage<Item>
    /// let s = ItemStorage::new();
    /// ```
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    /// total items
    pub fn get_total(&self) -> u64 {
        self.total
    }

    #[must_use]
    /// check if its empty
    ///
    /// ```
    /// # use mindus::item::storage::ItemStorage;
    /// # use mindus::item;
    ///
    /// let mut s = ItemStorage::new();
    /// assert!(s.is_empty());
    /// s.set(item::Type::Copper, 500);
    /// assert!(!s.is_empty());
    /// s.sub(item::Type::Copper, 500, 0);
    /// assert!(s.is_empty());
    /// ```
    pub fn is_empty(&self) -> bool {
        self.total == 0
    }

    /// get item count of certain element
    ///
    /// ```
    /// # use mindus::item::storage::ItemStorage;
    /// # use mindus::item;
    ///
    /// let mut s = ItemStorage::new();
    /// assert!(s.get(item::Type::Coal) == 0);
    /// s.set(item::Type::Coal, 500);
    /// assert!(s.get(item::Type::Titanium) == 0);
    /// assert!(s.get(item::Type::Coal) == 500);
    /// s.sub(item::Type::Coal, 500, 0);
    /// assert!(s.get(item::Type::Coal) == 0);
    /// ```
    #[must_use]
    pub fn get(&self, ty: T) -> u32 {
        match self.base.get(u16::from(ty) as usize) {
            None => 0,
            Some(cnt) => *cnt,
        }
    }
    /// set item count of certain element
    ///
    /// ```
    /// # use mindus::item::storage::ItemStorage;
    /// # use mindus::item;
    ///
    /// let mut s = ItemStorage::new();
    /// s.set(item::Type::Coal, 500);
    /// s.set(item::Type::Copper, 500);
    /// assert!(s.get(item::Type::Copper) == 500);
    /// ```
    pub fn set(&mut self, ty: T, count: u32) -> u32 {
        let idx = u16::from(ty) as usize;
        match self.base.get_mut(idx) {
            None => {
                self.base.resize(idx + 1, 0);
                self.base[idx] = count;
                self.total += u64::from(count);
                0
            }
            Some(curr) => {
                let prev = *curr;
                self.total = self.total - u64::from(prev) + u64::from(count);
                *curr = count;
                prev
            }
        }
    }

    /// add to a certain elements item count, capping.
    ///
    /// ```
    /// # use mindus::item::storage::ItemStorage;
    /// # use mindus::item;
    ///
    /// let mut s = ItemStorage::new();
    /// s.add(item::Type::Coal, 500, 500);
    /// assert!(s.get(item::Type::Coal) == 500);
    /// s.add(item::Type::Coal, 500, 10000);
    /// assert!(s.get(item::Type::Coal) == 1000);
    /// s.add(item::Type::Coal, 500, 1250);
    /// assert!(s.get(item::Type::Coal) == 1250);
    /// ```
    pub fn add(&mut self, ty: T, add: u32, max: u32) -> (u32, u32) {
        let idx = u16::from(ty) as usize;
        match self.base.get_mut(idx) {
            None => {
                let actual = add.min(max);
                self.base.resize(idx + 1, 0);
                self.base[idx] = actual;
                self.total += u64::from(add);
                (actual, actual)
            }
            Some(curr) => {
                if *curr < max {
                    let actual = add.min(max - *curr);
                    *curr += actual;
                    self.total += u64::from(actual);
                    (actual, *curr)
                } else {
                    (0, *curr)
                }
            }
        }
    }

    /// like [`Storage::add`] but fails
    pub fn try_add(&mut self, ty: T, add: u32, max: u32) -> Result<(u32, u32), TryAddError> {
        let idx = u16::from(ty) as usize;
        match self.base.get_mut(idx) {
            None => {
                if add <= max {
                    self.base.resize(idx + 1, 0);
                    self.base[idx] = add;
                    self.total += u64::from(add);
                    Ok((add, add))
                } else {
                    Err(TryAddError { have: 0, add, max })
                }
            }
            Some(curr) => {
                if *curr <= max && max - *curr <= add {
                    *curr += add;
                    self.total += u64::from(add);
                    Ok((add, *curr))
                } else {
                    Err(TryAddError {
                        have: *curr,
                        add,
                        max,
                    })
                }
            }
        }
    }

    pub fn sub(&mut self, ty: T, sub: u32, min: u32) -> (u32, u32) {
        match self.base.get_mut(u16::from(ty) as usize) {
            None => (0, 0),
            Some(curr) => {
                if *curr > min {
                    let actual = sub.min(*curr - min);
                    *curr -= actual;
                    self.total -= u64::from(actual);
                    (actual, *curr)
                } else {
                    (0, *curr)
                }
            }
        }
    }

    pub fn try_sub(&mut self, ty: T, sub: u32, min: u32) -> Result<(u32, u32), TrySubError> {
        let idx = u16::from(ty) as usize;
        match self.base.get_mut(idx) {
            None => Err(TrySubError { have: 0, sub, min }),
            Some(curr) => {
                if *curr >= min && *curr - min >= sub {
                    *curr -= sub;
                    self.total -= u64::from(sub);
                    Ok((sub, *curr))
                } else {
                    Err(TrySubError {
                        have: *curr,
                        sub,
                        min,
                    })
                }
            }
        }
    }

    pub fn add_all(&mut self, other: &Storage<T>, max_each: u32) -> (u64, u64) {
        let mut added = 0u64;
        if max_each > 0 && other.total > 0 {
            let mut iter = other.base.iter().enumerate();
            // resize our vector only once and if necessary
            let (last, add_last) = iter.rfind(|(_, n)| **n != 0).unwrap();
            if self.base.len() <= last {
                self.base.resize(last + 1, 0);
            }
            // process items by increasing ID
            for (idx, add) in iter {
                let curr = self.base[idx];
                if curr < max_each {
                    let actual = (*add).min(max_each - curr);
                    self.base[idx] += actual;
                    added += u64::from(actual);
                }
            }
            // process the final element (which we've retrieved first)
            let curr = self.base[last];
            if curr < max_each {
                let actual = (*add_last).min(max_each - curr);
                self.base[last] += actual;
                added += u64::from(actual);
            }
            // update total
            self.total += added;
        }
        (added, self.total)
    }

    pub fn pull_all(&mut self, other: &mut Storage<T>, max_each: u32) -> (u64, u64, u64) {
        let mut added = 0u64;
        if max_each > 0 && other.total > 0 {
            let mut iter = other.base.iter_mut().enumerate();
            // resize our vector only once and if necessary
            let (last, add_last) = iter.rfind(|(_, n)| **n != 0).unwrap();
            if self.base.len() <= last {
                self.base.resize(last + 1, 0);
            }
            // process items by increasing ID
            for (idx, add) in iter {
                let curr = self.base[idx];
                if curr < max_each {
                    let actual = (*add).min(max_each - curr);
                    self.base[idx] += actual;
                    *add -= actual;
                    added += u64::from(actual);
                }
            }
            // process the final element (which we've retrieved first)
            let curr = self.base[last];
            if curr < max_each {
                let actual = (*add_last).min(max_each - curr);
                self.base[last] += actual;
                *add_last -= actual;
                added += u64::from(actual);
            }
            // update totals
            self.total += added;
            other.total -= added;
        }
        (added, self.total, other.total)
    }

    pub fn sub_all(&mut self, other: &Storage<T>, min_each: u32) -> (u64, u64) {
        let mut subbed = 0u64;
        if self.total > 0 && other.total > 0 {
            // no need for resizing, we only remove
            // process items by increasing ID
            for (idx, sub) in other.base.iter().enumerate() {
                if let Some(curr) = self.base.get(idx) {
                    if *curr > min_each {
                        let actual = (*sub).min(*curr - min_each);
                        self.base[idx] -= actual;
                        subbed += u64::from(actual);
                    }
                } else {
                    break;
                }
            }
            // update total
            self.total -= subbed;
        }
        (subbed, self.total)
    }

    pub fn diff_all(&mut self, other: &mut Storage<T>, min_each: u32) -> (u64, u64, u64) {
        let mut subbed = 0u64;
        if self.total > 0 && other.total > 0 {
            // no need for resizing, we only remove
            // consider only indexes present in both
            let end = self.base.len().min(other.base.len());
            let lhs = &mut self.base[..end];
            let rhs = &mut other.base[..end];
            // process items by increasing ID
            for (l, r) in lhs.iter_mut().zip(rhs) {
                if *l > min_each && *r > min_each {
                    let actual = (*l - min_each).min(*r - min_each);
                    *l -= actual;
                    *r -= actual;
                    subbed -= u64::from(actual);
                }
            }
            // update totals
            self.total -= subbed;
            other.total -= subbed;
        }
        (subbed, self.total, other.total)
    }

    #[must_use]
    pub fn iter(&self) -> Iter<'_> {
        Iter {
            base: self.base.iter().enumerate(),
            all: true,
        }
    }

    #[must_use]
    pub fn iter_nonzero(&self) -> Iter<'_> {
        Iter {
            base: self.base.iter().enumerate(),
            all: false,
        }
    }

    pub fn clear(&mut self) {
        self.base.clear();
    }
}

// manual because padding with zeros doesn't affect equality
impl<T> PartialEq for Storage<T> {
    fn eq(&self, other: &Self) -> bool {
        let mut li = self.base.iter().fuse();
        let mut ri = other.base.iter().fuse();
        loop {
            match (li.next(), ri.next()) {
                (None, None) => return true,
                (l, r) => {
                    if l.unwrap_or(&0) != r.unwrap_or(&0) {
                        return false;
                    }
                }
            }
        }
    }
}

impl<T> fmt::Display for Storage<T>
where
    u16: From<T>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut iter = self.iter_nonzero();
        if let Some((ty, cnt)) = iter.next() {
            write!(f, "{cnt} {ty}")?;
            for (ty, cnt) in iter {
                write!(f, ", {cnt} {ty}")?;
            }
        }
        Ok(())
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
#[error("adding {add} to current {have} would exceed {max}")]
pub struct TryAddError {
    pub have: u32,
    pub add: u32,
    pub max: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, thiserror::Error)]
#[error("removing {sub} from current {have} would drop below {min}")]
pub struct TrySubError {
    pub have: u32,
    pub sub: u32,
    pub min: u32,
}

#[derive(Clone, Debug)]
pub struct Iter<'l> {
    base: Enumerate<slice::Iter<'l, u32>>,
    all: bool,
}

impl<'l> Iterator for Iter<'l> {
    type Item = (item::Type, u32);

    fn next(&mut self) -> Option<Self::Item> {
        for (idx, cnt) in self.base.by_ref() {
            if *cnt > 0 || self.all {
                if let Ok(ty) = item::Type::try_from(idx as u16) {
                    return Some((ty, *cnt));
                }
            }
        }
        None
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (0, self.base.size_hint().1)
    }
}

impl<'l> FusedIterator for Iter<'l> where Enumerate<slice::Iter<'l, u32>>: FusedIterator {}
