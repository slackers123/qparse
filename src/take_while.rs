use std::ops::Range;

use crate::{
    ErrorKind, ParseError, ParseRes,
    input::{Input, IntoChar},
    parser::Parser,
};

pub struct TakeWhile<I: Input> {
    range: Range<usize>,
    predicate: fn(I::Item) -> bool,
}

impl<I: Input + Clone> Parser<I> for TakeWhile<I> {
    type Output = I;
    fn parse(&self, input: I) -> ParseRes<I, Self::Output> {
        let original = input.clone();
        let mut i = 0;
        for item in input.items() {
            if !(self.predicate)(item) {
                if i < self.range.start {
                    return Err(original.to_error(ErrorKind::Unexpected(item.into_char())));
                } else {
                    return Ok(input.fseparate_at(i));
                }
            }

            if i == self.range.end {
                return Ok(input.fseparate_at(i));
            }

            i += 1;
        }

        if i >= self.range.start {
            return Ok(input.fseparate_at(i));
        } else {
            Err(ParseError::from_kind(original, ErrorKind::MoreNeeded))
        }
    }
}

pub fn take_while_range<I: Input + Clone>(
    predicate: fn(I::Item) -> bool,
    range: Range<usize>,
) -> impl Parser<I, Output = I> {
    TakeWhile { predicate, range }
}

pub fn take_while<I: Input + Clone>(predicate: fn(I::Item) -> bool) -> impl Parser<I, Output = I> {
    take_while_range(predicate, 0..usize::MAX)
}

pub fn take_while_min<I: Input + Clone>(
    min: usize,
    predicate: fn(I::Item) -> bool,
) -> impl Parser<I, Output = I> {
    take_while_range(predicate, min..usize::MAX)
}

pub fn take_while_max<I: Input + Clone>(
    max: usize,
    predicate: fn(I::Item) -> bool,
) -> impl Parser<I, Output = I> {
    take_while_range(predicate, 0..max)
}

pub fn alpha0<I: Clone>(input: I) -> ParseRes<I, I>
where
    I: Input<Item = char>,
{
    take_while(|c: char| c.is_alphabetic()).parse(input)
}

pub fn alpha1<I: Clone>(input: I) -> ParseRes<I, I>
where
    I: Input<Item = char>,
{
    take_while_min(1, |c: char| c.is_alphabetic()).parse(input)
}

pub fn digit0<I: Clone>(input: I) -> ParseRes<I, I>
where
    I: Input<Item = char>,
{
    take_while(|c: char| c.is_numeric()).parse(input)
}

pub fn digit1<I: Clone>(input: I) -> ParseRes<I, I>
where
    I: Input<Item = char>,
{
    take_while_min(1, |c: char| c.is_numeric()).parse(input)
}

pub fn alpha_num0<I: Clone>(input: I) -> ParseRes<I, I>
where
    I: Input<Item = char>,
{
    take_while(|c: char| c.is_alphanumeric()).parse(input)
}

pub fn alpha_num1<I: Clone>(input: I) -> ParseRes<I, I>
where
    I: Input<Item = char>,
{
    take_while_min(1, |c: char| c.is_alphanumeric()).parse(input)
}
