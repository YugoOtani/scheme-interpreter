use crate::token::*;
use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::error::ParseError;
//use nom::multi::*;
use nom::sequence::*;
use nom::*;
#[test]
fn exp_test() {
    println!("{:?}", test_parser("(+ 1 2)"));
}
fn test_parser(s: &str) -> IResult<&str, Body> {
    parse_body(s)
}
pub fn parse_token(s: &str) -> IResult<&str, Toplevel> {
    parse_top(s)
}
fn fail<T>(s: &str) -> IResult<&str, T> {
    Err(Err::Error(error::Error::new(s, error::ErrorKind::Fail)))
}
fn in_bracket<'a, O, E: ParseError<&'a str>, F>(
    inside: F,
) -> impl FnMut(&'a str) -> Result<(&'a str, O), nom::Err<E>>
where
    F: FnMut(&'a str) -> Result<(&'a str, O), nom::Err<E>>,
{
    map(
        permutation((char('('), multispace0, inside, multispace0, char(')'))),
        |(_, _, tkn, _, _)| tkn,
    )
}
pub fn separated_list0<I, O, O2, E, F, G>(
    mut sep: G,
    mut f: F,
) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
    I: Clone + InputLength,
    F: Parser<I, O, E>,
    G: Parser<I, O2, E>,
    E: ParseError<I>,
{
    move |mut i: I| {
        let mut res = Vec::new();

        match f.parse(i.clone()) {
            Err(Err::Error(_)) => return Ok((i, res)),
            Err(e) => return Err(e),
            Ok((i1, o)) => {
                res.push(o);
                i = i1;
            }
        }

        loop {
            let len = i.input_len();
            match sep.parse(i.clone()) {
                Err(Err::Error(_)) => return Ok((i, res)),
                Err(e) => return Err(e),
                Ok((i1, _)) => match f.parse(i1.clone()) {
                    Err(Err::Error(_)) => return Ok((i, res)),
                    Err(e) => return Err(e),
                    Ok((i2, o)) => {
                        res.push(o);
                        i = i2;
                    }
                },
            }
        }
    }
}
fn separated_list1<I, O, O2, E, F, G>(
    mut sep: G,
    mut f: F,
) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
    I: Clone + InputLength,
    F: Parser<I, O, E>,
    G: Parser<I, O2, E>,
    E: ParseError<I>,
{
    move |mut i: I| {
        let mut res = Vec::new();

        // Parse the first element
        match f.parse(i.clone()) {
            Err(e) => return Err(e),
            Ok((i1, o)) => {
                res.push(o);
                i = i1;
            }
        }

        loop {
            let len = i.input_len();
            match sep.parse(i.clone()) {
                Err(Err::Error(_)) => return Ok((i, res)),
                Err(e) => return Err(e),
                Ok((i1, _)) => match f.parse(i1.clone()) {
                    Err(Err::Error(_)) => return Ok((i, res)),
                    Err(e) => return Err(e),
                    Ok((i2, o)) => {
                        res.push(o);
                        i = i2;
                    }
                },
            }
        }
    }
}
fn end_token<'a>(s: &'a str) -> IResult<&'a str, ()> {
    match next(s) {
        None => Ok((s, ())),
        Some('(') => Ok((s, ())),
        Some(')') => Ok((s, ())),
        Some(c) => {
            if c.is_whitespace() {
                let s = del_space(s);
                Ok((s, ()))
            } else {
                fail("end of token expected")
            }
        }
    }
}
fn parse_top(s: &str) -> IResult<&str, Toplevel> {
    alt((
        map(parse_def, Toplevel::Define),
        map(
            in_bracket(permutation((tag("load"), multispace1, parse_string))),
            |(_, _, fname)| Toplevel::Load(fname),
        ),
        map(parse_exp, Toplevel::Exp),
    ))(s)
}
fn parse_string(s: &str) -> IResult<&str, String> {
    let (s, res) = delimited(char('"'), is_not("\""), char('"'))(s)?;
    Ok((s, res.to_string()))
}
fn parse_def(s: &str) -> IResult<&str, Define> {
    alt((parse_def_var, parse_def_fn))(s)
}
fn parse_def_var(s: &str) -> IResult<&str, Define> {
    map(
        in_bracket(permutation((
            tag("define"),
            multispace1,
            parse_id,
            end_token,
            parse_exp,
        ))),
        |(_, _, id, _, exp)| Define::Var(id, exp),
    )(s)
}
fn parse_def_fn(s: &str) -> IResult<&str, Define> {
    let (s, res) = in_bracket(permutation((
        tag("define"),
        end_token,
        parse_id,
        multispace0,
        parse_argfn,
        multispace0,
        char(')'),
        multispace0,
        parse_body,
    )))(s)?;
    let (_, _, id, _, param, _, _, _, body) = res;
    Ok((s, Define::Func(id, param, body)))
}
fn parse_argfn(s: &str) -> IResult<&str, Params> {
    let (s, prms) = separated_list0(multispace1, parse_id)(s)?;
    let tmp = map(
        permutation((space1, char(')'), space1, parse_id)),
        |(_, _, _, id)| id,
    );
    let (s, other) = opt(tmp)(s)?;
    if prms.is_empty() && other.is_some() {
        fail("illeagal use of '.'")
    } else {
        Ok((s, Params { prms, other }))
    }
}
fn parse_params(s: &str) -> IResult<&str, Params> {
    alt((
        map(parse_id, |id| Params {
            prms: vec![id],
            other: None,
        }),
        in_bracket(parse_multiparams),
    ))(s)
}
fn parse_multiparams(s: &str) -> IResult<&str, Params> {
    let (s, ids) = separated_list0(multispace1, parse_id)(s)?;
    let (s, id) = opt(map(
        permutation((multispace1, char('.'), multispace1, parse_id)),
        |(_, _, _, id)| id,
    ))(s)?;
    if ids.is_empty() && id.is_some() {
        fail("illeagal use of '.'")
    } else {
        Ok((
            s,
            Params {
                prms: ids,
                other: id,
            },
        ))
    }
}
fn parse_branch(s: &str) -> IResult<&str, Branch> {
    in_bracket(map(
        permutation((parse_exp, end_token, separated_list1(end_token, parse_exp))),
        |(exp, _, exps)| Branch {
            cond: exp,
            then: exps,
        },
    ))(s)
}
fn parse_bind(s: &str) -> IResult<&str, Bind> {
    let (s2, res) = in_bracket(permutation((parse_id, end_token, parse_exp)))(s)?;
    let (id, _, exp) = res;
    Ok((s2, Bind { name: id, val: exp }))
}
fn parse_body(s: &str) -> IResult<&str, Body> {
    let (s, defs) = separated_list0(end_token, parse_def)(s)?;
    let (s, _) = multispace0(s)?;
    let (s, exps) = separated_list1(end_token, parse_exp)(s)?;
    Ok((s, Body { defs, exps }))
}

fn parse_exp(s: &str) -> IResult<&str, Exp> {
    alt((
        map(parse_const, Exp::Const),
        map(parse_id, Exp::Id),
        parse_quote,
        parse_set,
        parse_ifexp,
        parse_cond,
        parse_and,
        parse_or,
        parse_let,
        parse_lambda,
        parse_let2,
        parse_letrec,
        parse_begin,
        in_bracket(parse_fncall),
    ))(s)
}
fn parse_lambda(s: &str) -> IResult<&str, Exp> {
    let (s, _) = tag("lambda")(s)?;
    let (s, _) = end_token(s)?;
    let (s, prms) = parse_params(s)?;
    let (s, _) = end_token(s)?;
    let (s, body) = parse_body(s)?;

    Ok((s, Exp::Lambda(prms, body)))
}
fn parse_quote(s: &str) -> IResult<&str, Exp> {
    alt((parse_quote1, parse_quote2))(s)
}
fn parse_quote1(s: &str) -> IResult<&str, Exp> {
    let (s, (_, _, sexp)) = in_bracket(permutation((tag("quote"), end_token, parse_sexp)))(s)?;
    Ok((s, Exp::Quote(sexp)))
}
fn parse_quote2(s: &str) -> IResult<&str, Exp> {
    let (s, _) = char('\'')(s)?;
    let (s, sexp) = parse_sexp(s)?;
    Ok((s, Exp::Quote(sexp)))
}
fn parse_set(s: &str) -> IResult<&str, Exp> {
    let (s2, res) = in_bracket(permutation((
        tag("set!"),
        multispace1,
        parse_id,
        end_token,
        parse_exp,
    )))(s)?;
    let (_, _, id, _, exp) = res;
    Ok((s2, Exp::Set(id, Box::new(exp))))
}

fn parse_let(s: &str) -> IResult<&str, Exp> {
    let (s, res) = in_bracket(permutation((
        tag("let"),
        end_token,
        opt(map(permutation((parse_id, end_token)), |(id, _)| id)),
        in_bracket(separated_list0(end_token, parse_bind)),
        end_token,
        parse_body,
    )))(s)?;
    let (_, _, id, bind, _, body) = res;
    Ok((
        s,
        Exp::Let {
            name: id,
            bind,
            body,
        },
    ))
}
fn parse_let2(s: &str) -> IResult<&str, Exp> {
    let (s, res) = in_bracket(permutation((
        tag("let*"),
        end_token,
        in_bracket(separated_list0(end_token, parse_bind)),
        end_token,
        parse_body,
    )))(s)?;
    let (_, _, bind, _, body) = res;
    Ok((s, Exp::Let2(bind, body)))
}
fn parse_letrec(s: &str) -> IResult<&str, Exp> {
    let (s, res) = in_bracket(permutation((
        tag("letrec"),
        end_token,
        in_bracket(separated_list0(end_token, parse_bind)),
        end_token,
        parse_body,
    )))(s)?;
    let (_, _, bind, _, body) = res;
    Ok((s, Exp::LetRec(bind, body)))
}
fn parse_ifexp(s: &str) -> IResult<&str, Exp> {
    let (s, res) = in_bracket(permutation((
        tag("if"),
        end_token,
        parse_exp,
        end_token,
        parse_exp,
        end_token,
        opt(parse_exp),
        end_token,
    )))(s)?;
    let (_, _, cond, _, then_exp, _, else_exp, _) = res;
    Ok((
        s,
        Exp::If {
            cond: Box::new(cond),
            then_exp: Box::new(then_exp),
            else_exp: Box::new(else_exp),
        },
    ))
}
fn parse_cond(s: &str) -> IResult<&str, Exp> {
    let (s, res) = in_bracket(permutation((
        tag("cond"),
        multispace0,
        separated_list0(end_token, parse_branch),
        multispace0,
        opt(map(
            permutation((
                tag("else"),
                end_token,
                separated_list1(end_token, parse_exp),
            )),
            |(_, _, exp)| exp,
        )),
    )))(s)?;
    let (_, _, branches, _, else_branch) = res;
    Ok((
        s,
        Exp::Cond {
            branches,
            else_branch,
        },
    ))
}
fn parse_and(s: &str) -> IResult<&str, Exp> {
    in_bracket(map(
        permutation((tag("and"), end_token, separated_list0(end_token, parse_exp))),
        |(_, _, exps)| Exp::And(exps),
    ))(s)
}
fn parse_or(s: &str) -> IResult<&str, Exp> {
    in_bracket(map(
        permutation((tag("or"), end_token, separated_list0(end_token, parse_exp))),
        |(_, _, exps)| Exp::And(exps),
    ))(s)
}
fn parse_begin(s: &str) -> IResult<&str, Exp> {
    in_bracket(map(
        permutation((
            tag("begin"),
            end_token,
            separated_list0(end_token, parse_exp),
        )),
        |(_, _, exps)| Exp::And(exps),
    ))(s)
}
fn parse_fncall(s: &str) -> IResult<&str, Exp> {
    let (s, fname) = parse_exp(s)?;
    let (s, _) = end_token(s)?;
    let (s, args) = separated_list0(end_token, parse_exp)(s)?;
    Ok((
        s,
        Exp::FunCall {
            fname: Box::new(fname),
            args,
        },
    ))
}
fn parse_id(s: &str) -> IResult<&str, Id> {
    let (s, id) = take_while1(|c: char| Id::is_valid(c))(s)?;
    match Id::new(id) {
        None => fail("invalid id"),
        Some(id) => Ok((s, id)),
    }
}
fn parse_sexp(s: &str) -> IResult<&str, SExp> {
    alt((
        map(parse_const, SExp::Const),
        map(parse_id, SExp::Id),
        in_bracket(parse_sexplist),
    ))(s)
}
fn parse_sexplist(s: &str) -> IResult<&str, SExp> {
    let (s, exps) = separated_list0(end_token, parse_sexp)(s)?;
    let (s, rest) = opt(map(
        permutation((multispace1, char('.'), end_token, parse_sexp)),
        |(_, _, _, e)| e,
    ))(s)?;
    if exps.is_empty() && rest.is_some() {
        fail("")
    } else {
        Ok((
            s,
            SExp::List {
                elems: exps,
                tail: Box::new(rest),
            },
        ))
    }
}
fn parse_const(s: &str) -> IResult<&str, Const> {
    alt((
        parse_num,
        parse_bool,
        map(parse_string, Const::String),
        parse_nil,
    ))(s)
}
fn parse_num(s: &str) -> IResult<&str, Const> {
    map(i64, Const::Num)(s)
}
fn parse_bool(s: &str) -> IResult<&str, Const> {
    alt((
        map(tag("#t"), |_| Const::Bool(true)),
        map(tag("#f"), |_| Const::Bool(false)),
    ))(s)
}
fn parse_nil(s: &str) -> IResult<&str, Const> {
    map(permutation((char('('), multispace0, char(')'))), |_| {
        Const::Nil
    })(s)
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
