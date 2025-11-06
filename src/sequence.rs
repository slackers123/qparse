use crate::parser::Parser;

pub struct Preceded<L, V> {
    l: L,
    v: V,
}

impl<I, L: Parser<I>, V: Parser<I>> Parser<I> for Preceded<L, V> {
    type Output = V::Output;
    fn parse(&self, input: I) -> crate::ParseRes<I, Self::Output> {
        let (input, _) = self.l.parse(input)?;
        self.v.parse(input)
    }
}

pub fn preceded<I, L: Parser<I>, V: Parser<I>>(l: L, v: V) -> impl Parser<I, Output = V::Output> {
    Preceded { l, v }
}

pub struct Trailed<V, R> {
    v: V,
    r: R,
}

impl<I, V: Parser<I>, R: Parser<I>> Parser<I> for Trailed<V, R> {
    type Output = V::Output;
    fn parse(&self, input: I) -> crate::ParseRes<I, Self::Output> {
        let (input, val) = self.v.parse(input)?;
        let (input, _) = self.r.parse(input)?;
        Ok((input, val))
    }
}

pub fn trailed<I, V: Parser<I>, R: Parser<I>>(v: V, r: R) -> impl Parser<I, Output = V::Output> {
    Trailed { v, r }
}

pub fn delimited<I, L: Parser<I>, V: Parser<I>, R: Parser<I>>(
    l: L,
    v: V,
    r: R,
) -> impl Parser<I, Output = V::Output> {
    preceded(l, trailed(v, r))
}
