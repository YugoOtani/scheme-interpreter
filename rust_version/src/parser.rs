use crate::token::*;
use anyhow::{bail, Result};
use nom::branch::*;
use nom::bytes::complete::*;
use nom::character::complete::*;
use nom::combinator::*;
use nom::error::ParseError;
use nom::sequence::*;
use nom::*;
#[test]
fn exp_test() {
    println!(
        "{:?}",
        in_bracket(parse_defmacro)("(macro (lets vars . body) (let*-expander vars body))")
    );
}
pub fn parse_tkns(s: &str) -> Result<Vec<Toplevel>> {
    fn helper(s: &str, mut v: Vec<Toplevel>) -> Result<Vec<Toplevel>> {
        match s.trim_start() {
            "" => Ok(v),
            s2 => match parse_top(s2) {
                Err(err) => bail!("could not parse \"{s2}\" : [{err}]"),
                Ok((s2, res)) => {
                    v.push(res);
                    helper(s2, v)
                }
            },
        }
    }
    helper(s, vec![])
}
pub fn parse_token(s: &str) -> Result<Toplevel> {
    fn helper(s: &str) -> IResult<&str, Toplevel> {
        let (s, _) = multispace0(s)?;
        let (s, res) = parse_top(s)?;
        let (s, _) = multispace0(s)?;
        Ok((s, res))
    }
    match helper(s) {
        Err(e) => bail!("{:?}", e),
        Ok(("", res)) => Ok(res),
        Ok((s, _)) => bail!("{s} remains untaken"),
    }
}
fn fail<T>(s: &str) -> IResult<&str, T> {
    Err(Err::Error(error::Error::new(s, error::ErrorKind::Fail)))
}
fn in_bracket<'a, O, E: ParseError<&'a str>, F>(
    mut inside: F,
) -> impl FnMut(&'a str) -> Result<(&'a str, O), nom::Err<E>>
where
    F: FnMut(&'a str) -> Result<(&'a str, O), nom::Err<E>>,
{
    move |s| {
        let (s, _) = char('(')(s)?;
        let (s, _) = multispace0(s)?;
        let (s, res) = inside(s)?;
        let (s, _) = multispace0(s)?;
        let (s, _) = char(')')(s)?;
        Ok((s, res))
    }
}
#[test]
fn p_let() {
    let s = "(let ((x 'inside) (y x)) y)";
    println!("{:?}", in_bracket(parse_let)(s));
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
            if len == 0 {
                break Ok((i, res));
            }
            match sep.parse(i.clone()) {
                Err(Err::Error(_)) => return Ok((i, res)),
                Err(e) => return Err(e),
                Ok((i1, _)) => match f.parse(i1.clone()) {
                    Err(Err::Error(_)) => return Ok((i, res)),
                    Err(e) => return Err(e),
                    Ok((i2, o)) => {
                        res.push(o);
                        if i2.input_len() == len {
                            return Ok((i, res));
                        }
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
            if len == 0 {
                break Ok((i, res));
            }
            match sep.parse(i.clone()) {
                Err(Err::Error(_)) => return Ok((i, res)),
                Err(e) => return Err(e),
                Ok((i1, _)) => match f.parse(i1.clone()) {
                    Err(Err::Error(_)) => return Ok((i, res)),
                    Err(e) => return Err(e),
                    Ok((i2, o)) => {
                        res.push(o);
                        if i2.input_len() == len {
                            return Ok((i, res));
                        }
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
                let (s, _) = multispace0(s)?;
                Ok((s, ()))
            } else {
                fail("end of token expected")
            }
        }
    }
}

fn parse_top(s: &str) -> IResult<&str, Toplevel> {
    if let Ok((s, res)) = map(parse_def, Toplevel::Define)(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = map(in_bracket(parse_load), |fname| Toplevel::Load(fname))(s) {
        return Ok((s, res));
    }
    map(parse_exp, Toplevel::Exp)(s)
}
fn parse_defmacro(s: &str) -> IResult<&str, Exp> {
    let (s, _) = tag("macro")(s)?;
    let (s, _) = end_token(s)?;
    let (s, _) = char('(')(s)?;
    let (s, _) = multispace0(s)?;
    let (s, id) = parse_id(s)?;
    let (s, _) = multispace0(s)?;
    let (s, param) = parse_argfn(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = char(')')(s)?;
    let (s, _) = multispace0(s)?;
    let (s, sexp) = parse_exp(s)?;
    Ok((s, Exp::DefMacro(id, param, Box::new(sexp))))
}

fn parse_load(s: &str) -> IResult<&str, String> {
    let (s, _) = tag("load")(s)?;
    let (s, _) = multispace1(s)?;
    let (s, str) = parse_string(s)?;
    Ok((s, str))
}
fn parse_string(s: &str) -> IResult<&str, String> {
    let (s, res) = delimited(char('"'), is_not("\""), char('"'))(s)?;
    Ok((s, res.to_string()))
}
fn parse_def(s: &str) -> IResult<&str, Define> {
    if let Ok((s, res)) = in_bracket(parse_def_var)(s) {
        return Ok((s, res));
    }
    in_bracket(parse_def_fn)(s)
}
fn parse_def_var(s: &str) -> IResult<&str, Define> {
    let (s, _) = tag("define")(s)?;
    let (s, _) = multispace1(s)?;
    let (s, id) = parse_id(s)?;
    let (s, _) = end_token(s)?;
    let (s, exp) = parse_exp(s)?;
    Ok((s, Define::Var(id, exp)))
}
fn parse_def_fn(s: &str) -> IResult<&str, Define> {
    let (s, _) = tag("define")(s)?;
    let (s, _) = end_token(s)?;
    let (s, _) = char('(')(s)?;
    let (s, _) = multispace0(s)?;
    let (s, id) = parse_id(s)?;
    let (s, _) = multispace0(s)?;
    let (s, param) = parse_argfn(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = char(')')(s)?;
    let (s, _) = multispace0(s)?;
    let (s, body) = parse_body(s)?;
    Ok((s, Define::Func(id, param, body)))
}
fn parse_argfn(s: &str) -> IResult<&str, Params> {
    let (s, prms) = separated_list0(multispace1, parse_id)(s)?;
    let (s, other) = opt(parse_prm_rest)(s)?;
    if prms.is_empty() && other.is_some() {
        fail("illeagal use of '.'")
    } else {
        Ok((s, Params { prms, other }))
    }
}
fn parse_params(s: &str) -> IResult<&str, Params> {
    if let Ok((s, res)) = map(parse_id, |id| Params {
        prms: vec![id],
        other: None,
    })(s)
    {
        return Ok((s, res));
    }
    in_bracket(parse_multiparams)(s)
}
#[test]
fn do_test() {
    let s = "(do (
                    (i 0 (+ i 1))
                    (j 0 (+ i j))
                 )
   ((= i 10) j)
 (print j))";

    println!("{:?}", in_bracket(parse_do)(s))
}
fn parse_do(s: &str) -> IResult<&str, Exp> {
    let (s, _) = tag("do")(s)?;
    let (s, _) = end_token(s)?;
    let (s, binds) = in_bracket(separated_list0(end_token, in_bracket(parse_dobind)))(s)?;
    let (s, _) = end_token(s)?;
    let (s, _) = char('(')(s)?;
    let (s, _) = end_token(s)?;
    let (s, pred) = parse_exp(s)?;

    let (s, _) = end_token(s)?;
    let (s, ret) = separated_list0(end_token, parse_exp)(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = char(')')(s)?;
    let (s, _) = end_token(s)?;
    let (s, body) = parse_body(s)?;
    Ok((
        s,
        Exp::Do {
            binds,
            pred: Box::new(pred),
            ret,
            body,
        },
    ))
}
fn parse_dobind(s: &str) -> IResult<&str, DoBind> {
    let (s, id) = parse_id(s)?;
    let (s, _) = end_token(s)?;
    let (s, init) = parse_exp(s)?;
    let (s, _) = end_token(s)?;
    let (s, update) = parse_exp(s)?;
    Ok((
        s,
        DoBind {
            name: id,
            init,
            update,
        },
    ))
}
fn parse_multiparams(s: &str) -> IResult<&str, Params> {
    let (s, ids) = separated_list0(multispace1, parse_id)(s)?;
    let (s, id) = opt(parse_prm_rest)(s)?;
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
fn parse_prm_rest(s: &str) -> IResult<&str, Id> {
    let (s, _) = multispace1(s)?;
    let (s, _) = char('.')(s)?;
    let (s, _) = multispace1(s)?;
    let (s, id) = parse_id(s)?;
    Ok((s, id))
}
fn parse_branch(s: &str) -> IResult<&str, Branch> {
    let (s, cond) = parse_exp(s)?;
    match cond {
        Exp::Id(ref id) => {
            if id.get() == "else" {
                return fail("else branch");
            }
        }
        _ => (),
    };
    let (s, _) = end_token(s)?;
    let (s, mut then) = separated_list1(end_token, parse_exp)(s)?;
    let ret = then.pop().unwrap();
    Ok((s, Branch { cond, then, ret }))
}
fn parse_bind(s: &str) -> IResult<&str, Bind> {
    let (s2, (name, val)) = in_bracket(one_bind)(s)?;
    Ok((s2, Bind { name, val }))
}
fn one_bind(s: &str) -> IResult<&str, (Id, Exp)> {
    let (s, id) = parse_id(s)?;
    let (s, _) = end_token(s)?;
    let (s, exp) = parse_exp(s)?;
    Ok((s, (id, exp)))
}
#[test]
fn plambda() {
    let s = "(lambda () (print 'a)(+ 1 2))";
    let b = "(print 'a)(+ 1 2)";

    println!("{:?}", separated_list1(end_token, parse_exp)(b));
    println!("{:?}", parse_body(b));
    println!("{:?}", in_bracket(parse_lambda)(s));
}
fn parse_body(s: &str) -> IResult<&str, Body> {
    let (s, defs) = separated_list0(end_token, parse_def)(s)?;
    let (s, _) = multispace0(s)?;
    let (s, mut exps) = separated_list1(end_token, parse_exp)(s)?;
    let ret = Box::new(exps.pop().unwrap());
    Ok((s, Body { defs, exps, ret }))
}
fn parse_expand_macro(s: &str) -> IResult<&str, Exp> {
    let (s, id) = parse_id(s)?;
    let (s, _) = end_token(s)?;
    let (s, args) = separated_list0(end_token, parse_sexp)(s)?;
    Ok((s, Exp::ExpandMacro(id, args)))
}
fn parse_exp(s: &str) -> IResult<&str, Exp> {
    if let Ok((s, res)) = map(parse_const, Exp::Const)(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = map(parse_id, Exp::Id)(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = in_bracket(parse_defmacro)(s) {
        return Ok((s, res));
    }

    if let Ok((s, res)) = parse_quote(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = in_bracket(parse_set)(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = in_bracket(parse_cond)(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = in_bracket(parse_and)(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = in_bracket(parse_or)(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = in_bracket(parse_ifexp)(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = in_bracket(parse_lambda)(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = in_bracket(parse_let)(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = in_bracket(parse_let2)(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = in_bracket(parse_letrec)(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = in_bracket(parse_begin)(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = in_bracket(parse_do)(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = in_bracket(parse_fncall)(s) {
        return Ok((s, res));
    }
    in_bracket(parse_expand_macro)(s)
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
    if let Ok((s, res)) = in_bracket(parse_quote1)(s) {
        return Ok((s, res));
    }
    parse_quote2(s)
}
fn parse_quote1(s: &str) -> IResult<&str, Exp> {
    let (s, _) = tag("quote")(s)?;
    let (s, _) = end_token(s)?;
    let (s, es) = parse_sexp(s)?;
    Ok((s, Exp::Quote(es)))
}
fn parse_quote2(s: &str) -> IResult<&str, Exp> {
    let (s, _) = char('\'')(s)?;
    let (s, sexp) = parse_sexp(s)?;
    Ok((s, Exp::Quote(sexp)))
}
fn parse_set(s: &str) -> IResult<&str, Exp> {
    let (s, _) = tag("set!")(s)?;
    let (s, _) = multispace1(s)?;
    let (s, id) = parse_id(s)?;
    let (s, _) = end_token(s)?;
    let (s, exp) = parse_exp(s)?;
    Ok((s, Exp::Set(id, Box::new(exp))))
}

fn parse_let(s: &str) -> IResult<&str, Exp> {
    let (s, _) = tag("let")(s)?;
    let (s, _) = end_token(s)?;
    let (s, id) = opt(map(permutation((parse_id, end_token)), |(id, _)| id))(s)?;
    let (s, bind) = in_bracket(separated_list0(end_token, parse_bind))(s)?;

    let (s, _) = end_token(s)?;

    let (s, body) = parse_body(s)?;

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
    let (s, _) = tag("let*")(s)?;
    let (s, _) = end_token(s)?;
    let (s, bind) = in_bracket(separated_list0(end_token, parse_bind))(s)?;
    let (s, _) = end_token(s)?;
    let (s, body) = parse_body(s)?;
    Ok((s, Exp::Let2(bind, body)))
}
fn parse_letrec(s: &str) -> IResult<&str, Exp> {
    let (s, _) = tag("letrec")(s)?;
    let (s, _) = end_token(s)?;
    let (s, bind) = in_bracket(separated_list0(end_token, parse_bind))(s)?;
    let (s, _) = end_token(s)?;
    let (s, body) = parse_body(s)?;
    Ok((s, Exp::LetRec(bind, body)))
}
fn parse_ifexp(s: &str) -> IResult<&str, Exp> {
    let (s, _) = tag("if")(s)?;
    let (s, _) = end_token(s)?;
    let (s, c) = parse_exp(s)?;
    let (s, _) = end_token(s)?;
    let (s, e1) = parse_exp(s)?;
    let (s, _) = end_token(s)?;
    let (s, e2) = opt(parse_exp)(s)?;
    let (s, _) = end_token(s)?;
    Ok((
        s,
        Exp::If {
            cond: Box::new(c),
            then_exp: Box::new(e1),
            else_exp: e2.map(Box::new),
        },
    ))
}
fn parse_cond(s: &str) -> IResult<&str, Exp> {
    let (s, _) = tag("cond")(s)?;
    let (s, _) = multispace0(s)?;
    let (s, branches) = separated_list0(end_token, in_bracket(parse_branch))(s)?;
    let (s, _) = multispace0(s)?;
    let (s, else_branch) = opt(in_bracket(parse_else))(s)?;
    let else_branch = else_branch.map(|mut v| {
        let ret = v.pop().unwrap();
        (v, Box::new(ret))
    });
    Ok((
        s,
        Exp::Cond {
            branches,
            else_branch,
        },
    ))
}
fn parse_else(s: &str) -> IResult<&str, Vec<Exp>> {
    let (s, _) = tag("else")(s)?;
    let (s, _) = end_token(s)?;
    let (s, es) = separated_list1(end_token, parse_exp)(s)?;
    Ok((s, es))
}
fn parse_and(s: &str) -> IResult<&str, Exp> {
    let (s, _) = tag("and")(s)?;
    let (s, _) = end_token(s)?;
    let (s, es) = separated_list0(end_token, parse_exp)(s)?;
    Ok((s, Exp::And(es)))
}
fn parse_or(s: &str) -> IResult<&str, Exp> {
    let (s, _) = tag("or")(s)?;
    let (s, _) = end_token(s)?;
    let (s, es) = separated_list0(end_token, parse_exp)(s)?;
    Ok((s, Exp::Or(es)))
}
fn parse_begin(s: &str) -> IResult<&str, Exp> {
    let (s, _) = tag("begin")(s)?;
    let (s, _) = end_token(s)?;
    let (s, es) = separated_list0(end_token, parse_exp)(s)?;
    Ok((s, Exp::Begin(es)))
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
    if let Ok((s, res)) = map(parse_const, SExp::Const)(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = map(parse_id, SExp::Id)(s) {
        return Ok((s, res));
    }
    in_bracket(parse_sexplist)(s)
}

fn parse_sexplist(s: &str) -> IResult<&str, SExp> {
    let (s, exps) = separated_list0(end_token, parse_sexp)(s)?;
    fn helper(s: &str) -> IResult<&str, SExp> {
        let (s, _) = multispace1(s)?;
        let (s, _) = char('.')(s)?;
        let (s, _) = end_token(s)?;
        let (s, exp) = parse_sexp(s)?;
        Ok((s, exp))
    }
    let (s, rest) = opt(helper)(s)?;
    if exps.is_empty() && rest.is_some() {
        fail("")
    } else {
        Ok((
            s,
            SExp::List {
                elems: exps,
                tail: rest.map(Box::new),
            },
        ))
    }
}
fn parse_const(s: &str) -> IResult<&str, Const> {
    if let Ok((s, res)) = parse_num(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = parse_bool(s) {
        return Ok((s, res));
    }
    if let Ok((s, res)) = map(parse_string, Const::String)(s) {
        return Ok((s, res));
    }
    parse_nil(s)
}
fn parse_num(s: &str) -> IResult<&str, Const> {
    map(i64, Const::Num)(s)
}
fn parse_bool(s: &str) -> IResult<&str, Const> {
    if let Ok((s, _)) = tag::<&str, &str, ()>("#t")(s) {
        return Ok((s, Const::Bool(true)));
    }
    map(tag("#f"), |_| Const::Bool(false))(s)
}
fn parse_nil(s: &str) -> IResult<&str, Const> {
    let (s, _) = char('(')(s)?;
    let (s, _) = multispace0(s)?;
    let (s, _) = char(')')(s)?;
    Ok((s, Const::Nil))
}

fn next(s: &str) -> Option<char> {
    s.chars().nth(0)
}
