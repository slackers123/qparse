use crate::{
    ErrorKind, ParseError, ParseRes,
    input::{Compare, Input},
};

pub fn tag<I, P>(pat: P) -> impl Fn(I) -> ParseRes<I, ()>
where
    I: Input,
    P: Input + Compare<I>,
{
    move |input| {
        if pat.compare(&input) {
            return Ok((input.separate_at(pat.len()).1, ()));
        } else {
            return Err(ParseError::from_kind(input, ErrorKind::NotMatching));
        }
    }
}

pub fn tag_map<I, P, T>(pat: P, v: T) -> impl Fn(I) -> ParseRes<I, T>
where
    I: Input,
    P: Input + Compare<I>,
    T: 'static + Send + Clone,
{
    move |input| {
        if pat.compare(&input) {
            return Ok((input.separate_at(pat.len()).1, v.clone()));
        } else {
            return Err(ParseError::from_kind(input, ErrorKind::NotMatching));
        }
    }
}
