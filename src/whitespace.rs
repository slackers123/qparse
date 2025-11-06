use crate::{ParseRes, input::Input, parser::Parser, sequence::delimited};

pub fn whitespace<I: Input>(input: I) -> ParseRes<I, ()>
where
    I: Input<Item = char>,
{
    for (i, c) in input.items_indices() {
        if !c.is_whitespace() {
            return Ok((input.separate_at(i).1, ()));
        }
    }
    return Ok((input.separate_at(input.len()).1, ()));
}

pub fn whitespace_wrapped<I: Input<Item = char>, P: Parser<I>>(
    parser: P,
) -> impl Parser<I, Output = P::Output> {
    delimited(whitespace, parser, whitespace)
}
