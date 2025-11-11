use std::{fmt::Display, ops::Range};

use crate::{ErrorKind, ParseError, ParseRes, parser::Parser, sequence::trailed};

#[derive(Debug)]
pub struct ManyParser<P> {
    range: Range<usize>,
    f: P,
}

impl<I: Clone, P: Parser<I>> Parser<I> for ManyParser<P> {
    type Output = Vec<P::Output>;

    fn parse(&self, mut input: I) -> ParseRes<I, Self::Output> {
        let original = input.clone();
        let mut i = 0;
        let mut res = Vec::new();
        while i < self.range.end {
            match self.f.parse(input) {
                Ok((inp, val)) => {
                    res.push(val);
                    input = inp;
                    i += 1;
                }
                Err(e) => {
                    input = e.input;
                    break;
                }
            }
        }

        if i >= self.range.start {
            return Ok((input, res));
        } else {
            Err(ParseError::from_kind(original, ErrorKind::MoreNeeded))
        }
    }
}

pub fn many<I: Clone, P: Parser<I>>(
    f: P,
    range: Range<usize>,
) -> impl Parser<I, Output = Vec<P::Output>> {
    ManyParser { range, f }
}

pub fn many0<I: Clone, P: Parser<I>>(f: P) -> impl Parser<I, Output = Vec<P::Output>> {
    many(f, 0..usize::MAX)
}

pub fn many1<I: Clone, P: Parser<I>>(f: P) -> impl Parser<I, Output = Vec<P::Output>> {
    many(f, 1..usize::MAX)
}

pub fn many_with_separator<I: Clone + Display, P: Parser<I>, S: Parser<I>>(
    parser: P,
    separator: S,
) -> impl Parser<I, Output = Vec<P::Output>> {
    many0(trailed(parser, separator))
}

pub struct ManyLaxSeparator<P, S> {
    p: P,
    s: S,
}

impl<I: Clone, P, S> Parser<I> for ManyLaxSeparator<P, S>
where
    P: Parser<I>,
    S: Parser<I>,
{
    type Output = Vec<P::Output>;
    fn parse(&self, mut input: I) -> ParseRes<I, Self::Output> {
        let mut res = Vec::new();
        loop {
            match self.p.parse(input) {
                Ok((inp, val)) => {
                    res.push(val);
                    input = inp;
                }
                Err(e) => {
                    input = e.input;
                    break;
                }
            }

            match self.s.parse(input) {
                Ok((inp, _)) => {
                    input = inp;
                }
                Err(e) => {
                    input = e.input;
                    break;
                }
            }
        }
        Ok((input, res))
    }
}

pub fn many_with_separator_lax<I: Clone, P: Parser<I>, S: Parser<I>>(
    p: P,
    s: S,
) -> impl Parser<I, Output = Vec<P::Output>> {
    ManyLaxSeparator { p, s }
}
