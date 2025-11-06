use crate::parser::Parser;

pub struct MapRes<P, M> {
    p: P,
    m: M,
}

impl<I, P, M, O> Parser<I> for MapRes<P, M>
where
    P: Parser<I>,
    M: Fn(P::Output) -> O,
{
    type Output = O;

    fn parse(&self, input: I) -> crate::ParseRes<I, Self::Output> {
        let res = self.p.parse(input)?;

        Ok((res.0, (self.m)(res.1)))
    }
}

#[inline(always)]
pub fn map_res<I, P: Parser<I>, M, O>(p: P, m: M) -> impl Parser<I, Output = O>
where
    M: Fn(P::Output) -> O,
{
    MapRes { p, m }
}
