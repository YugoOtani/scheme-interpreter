use crate::ast::*;
use crate::token::*;
use anyhow::*;
#[derive(Debug, Clone)]
struct Tokens<'a> {
    tkn: Vec<Token<'a>>,
    cur: usize,
}
pub fn parse<'a>(tkn: Vec<Token<'a>>) -> anyhow::Result<SExp<'a>> {
    let mut tkn = Tokens::new(tkn);
    let ast = tkn.parse()?;
    if tkn.peek().is_some() {
        let mut s = String::new();
        for i in tkn.cur..tkn.tkn.len() {
            s.push_str(&tkn.tkn[i].to_string())
        }
        bail!("[parse error] '{}' remains untaken", s)
    } else {
        Ok(ast)
    }
}
pub fn parse_many<'a>(tkn: Vec<Token<'a>>) -> anyhow::Result<Vec<SExp<'a>>> {
    let mut tkn = Tokens::new(tkn);
    let mut ret = vec![];
    while tkn.peek().is_some() {
        ret.push(tkn.parse()?)
    }
    Ok(ret)
}

impl<'a> Tokens<'a> {
    fn new(tkn: Vec<Token<'a>>) -> Self {
        Self { tkn, cur: 0 }
    }
    fn next(&mut self) -> Option<&Token<'a>> {
        let tmp = self.cur;
        self.cur += 1;
        self.tkn.get(tmp)
    }
    fn peek(&self) -> Option<&Token<'a>> {
        self.tkn.get(self.cur)
    }
    fn parse(&mut self) -> anyhow::Result<SExp<'a>> {
        match self.next() {
            Some(Token::Close) | None => bail!("[sexp] expected ) or identifier"),
            Some(Token::Open) => {
                let mut lst = vec![];
                loop {
                    match self.peek() {
                        None => {
                            bail!("unexpected end of token")
                        }
                        Some(Token::Close) => {
                            self.next();
                            break;
                        }
                        Some(Token::Open) | Some(Token::Id(_)) => {
                            lst.push(self.parse()?);
                        }
                    }
                }
                Ok(SExp::SList(lst))
            }
            Some(Token::Id(s)) => {
                let id = Id::from_str(*s).with_context(|| format!("id {} is invalid", s))?;

                Ok(SExp::Id(id))
            }
        }
    }
}
// SEXP = ID | (SEXP-LIST)
// SEXP-LIST = None | SEXP SEXP-LIST
