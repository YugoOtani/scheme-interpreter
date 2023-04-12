use crate::token::*;
use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::error::ParseError;
use nom::multi::*;
use nom::sequence::*;
use nom::*;
pub fn parse(s: &str) -> IResult<&str, Toplevel> {
    parse_top(s)
}
fn fail<T>(s: &str) -> IResult<&str, T> {
    Err(Err::Error(error::Error::new(s, error::ErrorKind::Fail)))
}
// compile error if &str to generic and add constraint -> why?
fn in_bracket<'a, O, E: ParseError<&'a str>, F>(
    inside: F,
) -> impl FnMut(&'a str) -> Result<(&'a str, O), nom::Err<E>>
where
    F: FnMut(&'a str) -> Result<(&'a str, O), nom::Err<E>>,
{
    map(
        permutation((
            multispace0,
            char('('),
            multispace0,
            inside,
            multispace0,
            char(')'),
        )),
        |(_, _, _, tkn, _, _)| tkn,
    )
}
fn parse_top(s: &str) -> IResult<&str, Toplevel> {
    if let Ok((s, tkn)) = parse_exp(s) {
        return Ok((s, Toplevel::Exp(tkn)));
    }
    if let Ok((s, tkn)) = parse_def(s) {
        return Ok((s, Toplevel::Define(tkn)));
    }
    // parse (load String)
    in_bracket(permutation((tag("load"), multispace1, parse_string)))(s)
        .map(|(s2, (_, _, s))| (s2, Toplevel::Load(s)))
}
fn parse_string(s: &str) -> IResult<&str, String> {
    let s = del_space(s);
    let (s, res) = delimited(char('"'), is_not("\n\""), char('"'))(s)?;
    Ok((s, res.to_string()))
}
fn parse_def(s: &str) -> IResult<&str, Define> {
    if let Ok((s, res)) = parse_def_var(s) {
        Ok((s, res))
    } else {
        parse_def_fn(s)
    }
}
fn parse_def_var(s: &str) -> IResult<&str, Define> {
    map(
        in_bracket(permutation((
            tag("define"),
            multispace1,
            parse_id,
            multispace1,
            parse_exp,
        ))),
        |(_, _, id, _, exp)| Define::Var(id, exp),
    )(s)
}
fn parse_def_fn(s: &str) -> IResult<&str, Define> {
    map(in_bracket(permutation((
        tag("define"),
        parse_id,
        multispace1,
        parse_params,
        parse_body,
    )))
    .map(|(_, param, body)| Define::Func()))(s)
}

fn parse_params(s: &str) -> IResult<&str, Params> {
    todo!()
}
fn parse_branch(s: &str) -> IResult<&str, Branch> {
    todo!()
}
fn parse_bind(s: &str) -> IResult<&str, Bind> {
    todo!()
}
fn parse_body(s: &str) -> IResult<&str, Body> {
    todo!()
}
fn parse_arg(s: &str) -> IResult<&str, Arg> {
    todo!()
}
fn parse_exp(s: &str) -> IResult<&str, Exp> {
    fail(s)
}

fn parse_id(s: &str) -> IResult<&str, Id> {
    fail(s)
    //alphanumeric1(s).map(|(s2, res)| (s2, Id(res.to_string())))
}
fn parse_sexp(s: &str) -> IResult<&str, SExp> {
    todo!()
}
fn parse_const(s: &str) -> IResult<&str, Const> {
    todo!()
}
fn del_space(s: &str) -> &str {
    match s.find(|c: char| !c.is_whitespace()) {
        Some(0) => s,
        Some(i) => &s[i..],
        None => "",
    }
}
fn next(s: &str) -> Option<char> {
    s.chars().nth(0)
}
fn consume_str<'a>(s: &'a str, str_to_consume: &'a str) -> Result<&'a str, String> {
    if s.starts_with(str_to_consume) {
        Ok(&s[str_to_consume.len()..])
    } else {
        Err(format!("cannot take '{str_to_consume}' from '{s}'"))
    }
}
