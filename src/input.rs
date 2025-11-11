use crate::{ErrorKind, ParseError};

pub trait Input: Sized {
    type Item: Copy + IntoChar;

    fn len(&self) -> usize;

    fn separate_at(&self, i: usize) -> (Self, Self);

    /// Flipped separate at.
    #[inline(always)]
    fn fseparate_at(&self, i: usize) -> (Self, Self) {
        let res = self.separate_at(i);
        (res.1, res.0)
    }

    fn items(&self) -> impl Iterator<Item = Self::Item>;

    fn items_indices(&self) -> impl Iterator<Item = (usize, Self::Item)>;

    fn to_error(&self, kind: ErrorKind) -> ParseError<Self>;
}

pub trait IntoChar {
    fn into_char(self) -> char;
}

pub trait Compare<Other> {
    fn compare(&self, other: &Other) -> bool;
}

impl Input for &str {
    type Item = char;

    fn len(&self) -> usize {
        (*self).len()
    }

    fn separate_at(&self, i: usize) -> (Self, Self) {
        self.split_at(i)
    }

    fn items(&self) -> impl Iterator<Item = Self::Item> {
        self.chars()
    }

    fn items_indices(&self) -> impl Iterator<Item = (usize, Self::Item)> {
        self.char_indices()
    }

    fn to_error(&self, kind: ErrorKind) -> ParseError<Self> {
        ParseError::from_kind(self, kind)
    }
}

impl Input for char {
    type Item = char;

    fn len(&self) -> usize {
        1
    }

    fn separate_at(&self, i: usize) -> (Self, Self) {
        panic!("cannot separate char")
    }

    fn items(&self) -> impl Iterator<Item = Self::Item> {
        (0..1).map(|_| *self)
    }

    fn items_indices(&self) -> impl Iterator<Item = (usize, Self::Item)> {
        (0..1).map(|i| (i, *self))
    }

    fn to_error(&self, kind: ErrorKind) -> ParseError<Self> {
        ParseError::from_kind(*self, kind)
    }
}

impl Compare<&str> for &str {
    fn compare(&self, other: &&str) -> bool {
        if self.len() > other.len() {
            return false;
        }
        *self == &other[0..self.len()]
    }
}

impl Compare<&str> for char {
    fn compare(&self, other: &&str) -> bool {
        other.chars().nth(0) == Some(*self)
    }
}

impl IntoChar for char {
    fn into_char(self) -> char {
        self
    }
}
