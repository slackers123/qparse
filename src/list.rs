use crate::{ErrorKind, ParseError, ParseRes, parser::Parser};

pub trait List<I, T>: Sized {
    type Output;
    fn use_first(self) -> impl Fn(I) -> ParseRes<I, Self::Output>;
}

impl<const N: usize, I, T, P: Parser<I, Output = T>> List<I, T> for [P; N] {
    type Output = T;
    fn use_first(self) -> impl Fn(I) -> ParseRes<I, T> {
        move |mut input| {
            for p in &self {
                match p.parse(input) {
                    Ok(v) => return Ok(v),
                    Err(e) => {
                        input = e.input;
                    }
                }
            }

            return Err(ParseError::from_kind(input, ErrorKind::NoMatches));
        }
    }
}

impl<I, T, P0, P1> List<I, T> for (P0, P1)
where
    P0: Parser<I, Output = T>,
    P1: Parser<I, Output = T>,
{
    type Output = T;
    fn use_first(self) -> impl Fn(I) -> ParseRes<I, T> {
        move |mut input| {
            match self.0.parse(input) {
                Ok(v) => return Ok(v),
                Err(e) => {
                    input = e.input;
                }
            }

            match self.1.parse(input) {
                Ok(v) => return Ok(v),
                Err(e) => {
                    input = e.input;
                }
            }

            return Err(ParseError::from_kind(input, ErrorKind::NoMatches));
        }
    }
}

impl<I, T, P0, P1, P2> List<I, T> for (P0, P1, P2)
where
    P0: Parser<I, Output = T>,
    P1: Parser<I, Output = T>,
    P2: Parser<I, Output = T>,
{
    type Output = T;
    fn use_first(self) -> impl Fn(I) -> ParseRes<I, T> {
        move |mut input| {
            match self.0.parse(input) {
                Ok(v) => return Ok(v),
                Err(e) => {
                    input = e.input;
                }
            }

            match self.1.parse(input) {
                Ok(v) => return Ok(v),
                Err(e) => {
                    input = e.input;
                }
            }

            match self.2.parse(input) {
                Ok(v) => return Ok(v),
                Err(e) => {
                    input = e.input;
                }
            }

            return Err(ParseError::from_kind(input, ErrorKind::NoMatches));
        }
    }
}
