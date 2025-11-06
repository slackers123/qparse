use crate::{ParseRes, input::Input};

pub trait Parser<I> {
    type Output;
    fn parse(&self, input: I) -> ParseRes<I, Self::Output>;
}

impl<I, T, O> Parser<I> for T
where
    T: Fn(I) -> ParseRes<I, O>,
{
    type Output = O;

    fn parse(&self, input: I) -> ParseRes<I, Self::Output> {
        self(input)
    }
}

impl<I, A0, A1> Parser<I> for (A0, A1)
where
    I: Input,
    A0: Parser<I>,
    A1: Parser<I>,
{
    type Output = (A0::Output, A1::Output);
    fn parse(&self, input: I) -> ParseRes<I, Self::Output> {
        let (input, res0) = self.0.parse(input)?;
        let (input, res1) = self.1.parse(input)?;

        Ok((input, (res0, res1)))
    }
}

impl<I, A0, A1, A2> Parser<I> for (A0, A1, A2)
where
    I: Input,
    A0: Parser<I>,
    A1: Parser<I>,
    A2: Parser<I>,
{
    type Output = (A0::Output, A1::Output, A2::Output);
    fn parse(&self, input: I) -> ParseRes<I, Self::Output> {
        let (input, res0) = self.0.parse(input)?;
        let (input, res1) = self.1.parse(input)?;
        let (input, res2) = self.2.parse(input)?;

        Ok((input, (res0, res1, res2)))
    }
}

impl<I, A0, A1, A2, A3, A4> Parser<I> for (A0, A1, A2, A3, A4)
where
    I: Input,
    A0: Parser<I>,
    A1: Parser<I>,
    A2: Parser<I>,
    A3: Parser<I>,
    A4: Parser<I>,
{
    type Output = (A0::Output, A1::Output, A2::Output, A3::Output, A4::Output);
    fn parse(&self, input: I) -> ParseRes<I, Self::Output> {
        let (input, res0) = self.0.parse(input)?;
        let (input, res1) = self.1.parse(input)?;
        let (input, res2) = self.2.parse(input)?;
        let (input, res3) = self.3.parse(input)?;
        let (input, res4) = self.4.parse(input)?;

        Ok((input, (res0, res1, res2, res3, res4)))
    }
}
