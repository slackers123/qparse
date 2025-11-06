use qparse::{
    ParseRes, char, map::map_res, multi::many0, parser::Parser, sequence::delimited,
    take_while::digit1, use_first, whitespace::whitespace,
};
use std::str::FromStr;

#[derive(Debug, PartialEq)]
pub enum Expr {
    ENum(f32),
    EAdd(Box<Expr>, Box<Expr>),
    ESub(Box<Expr>, Box<Expr>),
    EMul(Box<Expr>, Box<Expr>),
    EDiv(Box<Expr>, Box<Expr>),
    EExp(Box<Expr>, Box<Expr>),
}

impl Expr {
    pub fn eval(self) -> f32 {
        match self {
            Self::ENum(v) => v,
            Self::EAdd(lhs, rhs) => lhs.eval() + rhs.eval(),
            Self::ESub(lhs, rhs) => lhs.eval() - rhs.eval(),
            Self::EMul(lhs, rhs) => lhs.eval() * rhs.eval(),
            Self::EDiv(lhs, rhs) => lhs.eval() / rhs.eval(),
            Self::EExp(lhs, rhs) => lhs.eval().powf(rhs.eval()),
        }
    }
}

fn parse(input: &str) -> ParseRes<&str, Expr> {
    parse_basic_expr(input)
}

fn parse_basic_expr(input: &str) -> ParseRes<&str, Expr> {
    parse_math_expr(input)
}

fn parse_parens(input: &str) -> ParseRes<&str, Expr> {
    delimited(
        whitespace,
        delimited(char('('), parse_math_expr, char(')')),
        whitespace,
    )
    .parse(input)
}

fn parse_operation(input: &str) -> ParseRes<&str, Expr> {
    use_first([parse_parens, parse_number]).parse(input)
}

fn parse_factor(input: &str) -> ParseRes<&str, Expr> {
    let (input, num1) = parse_operation(input)?;
    let (input, exprs) = many0((char('^'), parse_factor)).parse(input)?;
    Ok((input, parse_expr(num1, exprs)))
}

fn parse_term(input: &str) -> ParseRes<&str, Expr> {
    let (input, num1) = parse_factor(input)?;
    let (input, exprs) = many0((use_first((char('/'), char('*'))), parse_factor)).parse(input)?;
    Ok((input, parse_expr(num1, exprs)))
}

fn parse_math_expr(input: &str) -> ParseRes<&str, Expr> {
    let (input, num1) = parse_term(input)?;
    let (input, exprs) = many0((use_first((char('+'), char('-'))), parse_term)).parse(input)?;
    Ok((input, parse_expr(num1, exprs)))
}

fn parse_expr(expr: Expr, rem: Vec<(char, Expr)>) -> Expr {
    rem.into_iter().fold(expr, |acc, val| parse_op(val, acc))
}

fn parse_op(tup: (char, Expr), expr1: Expr) -> Expr {
    let (op, expr2) = tup;
    match op {
        '+' => Expr::EAdd(Box::new(expr1), Box::new(expr2)),
        '-' => Expr::ESub(Box::new(expr1), Box::new(expr2)),
        '*' => Expr::EMul(Box::new(expr1), Box::new(expr2)),
        '/' => Expr::EDiv(Box::new(expr1), Box::new(expr2)),
        '^' => Expr::EExp(Box::new(expr1), Box::new(expr2)),
        _ => panic!("Unknown Operation"),
    }
}

fn parse_enum(parsed_num: &str) -> Expr {
    let num = f32::from_str(parsed_num).unwrap();

    Expr::ENum(num)
}

fn parse_number(input: &str) -> ParseRes<&str, Expr> {
    let p = delimited(whitespace, digit1, whitespace);
    map_res(p, parse_enum).parse(input)
}

fn main() {
    let res = parse("(10 + 10) * 3 ^ 5").unwrap().1;
    println!("{res:?}");

    assert_eq!(res.eval(), 4_860.0);
}
