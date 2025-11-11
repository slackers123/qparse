use std::{
    error::Error,
    fmt::{Debug, Display},
};

pub mod input;
pub mod list;
pub mod map;
pub mod multi;
pub mod parser;
pub mod sequence;
pub mod tag;
pub mod take_while;
pub mod whitespace;

use crate::{
    input::{Compare, Input},
    list::List,
    parser::Parser,
};

pub type SimpleParser<I, T> = fn(I) -> ParseRes<I, T>;

#[derive(Debug)]
pub struct ParseError<I> {
    pub input: I,
    kind: ErrorKind,
}

impl<I> ParseError<I> {
    pub fn from_kind(input: I, kind: ErrorKind) -> Self {
        Self { input, kind }
    }

    pub fn from_external(input: I, e: impl Error + 'static) -> Self {
        Self {
            input,
            kind: ErrorKind::Inner(Box::new(e)),
        }
    }
}

impl<I: Display> Display for ParseError<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "error: {:?}", self.kind)
    }
}

impl<I: Debug + Display> Error for ParseError<I> {}

#[derive(Debug)]
pub enum ErrorKind {
    MoreNeeded,
    Unexpected(char),
    NotMatching,
    NoMatches,
    EOFReached,
    Inner(Box<dyn Error>),
}

pub type ParseRes<I, T> = Result<(I, T), ParseError<I>>;

pub fn to_optional<I, T>(
    parser: impl Parser<I, Output = T>,
) -> impl Fn(I) -> ParseRes<I, Option<T>> {
    move |input| match parser.parse(input) {
        Ok((input, val)) => Ok((input, Some(val))),
        Err(e) => Ok((e.input, None)),
    }
}

pub fn use_first<I, T, L: List<I, T>>(list: L) -> impl Fn(I) -> ParseRes<I, L::Output> {
    list.use_first()
}

pub fn map_err<I, T>(res: Result<T, impl Error + 'static>) -> impl FnOnce(I) -> ParseRes<I, T> {
    move |input| match res {
        Ok(v) => Ok((input, v)),
        Err(e) => Err(ParseError::from_external(input, e)),
    }
}

pub fn char<I, C: Clone>(c: C) -> impl Fn(I) -> ParseRes<I, C>
where
    I: Input,
    C: Input + Compare<I>,
{
    move |input| {
        if c.compare(&input) {
            Ok((input.separate_at(c.len()).1, c.clone()))
        } else {
            Err(ParseError::from_kind(input, ErrorKind::NotMatching))
        }
    }
}
