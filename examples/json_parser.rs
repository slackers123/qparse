use std::collections::HashMap;

use qparse::{
    ParseRes, map_err,
    multi::many_with_separator_lax,
    parser::Parser,
    sequence::delimited,
    tag::{tag, tag_map},
    take_while::take_while,
    use_first,
    whitespace::whitespace_wrapped,
};

#[derive(Debug, PartialEq, Clone)]
enum JSONValue {
    String(String),
    Number(f64),
    Object(HashMap<String, JSONValue>),
    Array(Vec<JSONValue>),
    Boolean(bool),
    Null,
}

fn generic_string(input: &str) -> ParseRes<&str, String> {
    let (input, (_, res, _)) = (tag("\""), take_while(|v| v != '"'), tag("\"")).parse(input)?;

    return Ok((input, String::from(res)));
}

fn json_string(input: &str) -> ParseRes<&str, JSONValue> {
    let (input, val) = generic_string(input)?;
    return Ok((input, JSONValue::String(val)));
}

fn json_number(input: &str) -> ParseRes<&str, JSONValue> {
    let (input, num) = take_while(|v: char| v.is_numeric() || v == '.').parse(input)?;

    return Ok((input, JSONValue::Number(map_err(num.parse())(input)?.1)));
}

fn json_key_value(input: &str) -> ParseRes<&str, (String, JSONValue)> {
    let (input, (key, _, value)) =
        (generic_string, whitespace_wrapped(tag(":")), json_value).parse(input)?;

    Ok((input, (key, value)))
}

fn json_object(input: &str) -> ParseRes<&str, JSONValue> {
    let (input, values) = delimited(
        tag("{"),
        whitespace_wrapped(many_with_separator_lax(
            whitespace_wrapped(json_key_value),
            tag(","),
        )),
        tag("}"),
    )
    .parse(input)?;

    Ok((input, JSONValue::Object(values.into_iter().collect())))
}

fn json_array(input: &str) -> ParseRes<&str, JSONValue> {
    let (input, values) = delimited(
        tag("["),
        whitespace_wrapped(many_with_separator_lax(
            whitespace_wrapped(json_value),
            tag(","),
        )),
        tag("]"),
    )
    .parse(input)?;

    Ok((input, JSONValue::Array(values)))
}

fn json_true(input: &str) -> ParseRes<&str, JSONValue> {
    tag_map("true", JSONValue::Boolean(true))(input)
}

fn json_false(input: &str) -> ParseRes<&str, JSONValue> {
    tag_map("false", JSONValue::Boolean(false))(input)
}

fn json_bool(input: &str) -> ParseRes<&str, JSONValue> {
    use_first([json_true, json_false]).parse(input)
}

fn json_null(input: &str) -> ParseRes<&str, JSONValue> {
    tag_map("null", JSONValue::Null)(input)
}

fn json_value(input: &str) -> ParseRes<&str, JSONValue> {
    use_first([
        json_string,
        json_number,
        json_object,
        json_array,
        json_bool,
        json_null,
    ])
    .parse(input)
}

fn main() {
    let res = json_value("{\"a\": 10, \"b\": [\"hallo\"]}");
    println!("{res:?}");

    assert_eq!(
        res.unwrap().1,
        JSONValue::Object(HashMap::from([
            ("a".into(), JSONValue::Number(10.0)),
            (
                "b".into(),
                JSONValue::Array(vec![JSONValue::String("hallo".into())])
            )
        ]))
    );
}
