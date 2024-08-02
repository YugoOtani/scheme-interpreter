use anyhow::bail;
use regex::Regex;
use std::fmt::{Debug, Display};
#[derive(Clone, PartialEq, Eq)]
pub enum SExp<'a> {
    Id(Id<'a>),
    SList(Vec<SExp<'a>>),
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Id<'a> {
    Id(&'a str),
    StrLiteral(&'a str),
    Num(i64),
    Bool(bool),
}
impl<'a> Display for Id<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Id::Id(s) => write!(f, "{s}"),
            Id::StrLiteral(s) => write!(f, "\"{s}\""),
            Id::Num(i) => write!(f, "{i}"),
            Id::Bool(b) => write!(f, "{}", if *b { "#t" } else { "#f" }),
        }
    }
}
impl<'a> Id<'a> {
    // TODO: support escape charactor
    pub fn from_str(s: &'a str) -> Option<Id<'a>> {
        let id_regex =
            Regex::new(r"^[A-Za-z!$%&*+-./<=>?@^_][0-9A-Za-z!$%&*+-./<=>?@^_]*$").unwrap();
        let quoted_str_regex = Regex::new(r#"^"[0-9A-Za-z!$%&*+-./<=>?@^_]*"$"#).unwrap();
        if s == "#t" {
            Some(Id::Bool(true))
        } else if s == "#f" {
            Some(Id::Bool(false))
        } else if let Some(i) = str::parse::<i64>(s).ok() {
            Some(Id::Num(i))
        } else if id_regex.is_match(s) {
            Some(Id::Id(s))
        } else if quoted_str_regex.is_match(s) {
            Some(Id::StrLiteral(&s[1..s.len() - 1]))
        } else {
            None
        }
    }
    pub fn expect_id(&self) -> anyhow::Result<&str> {
        match self {
            Id::Id(id) => Ok(id),
            _ => bail!("expect identifier, found {:?}", self),
        }
    }
}
#[test]
fn from_str_test() {
    assert_eq!(Id::from_str("a12"), Some(Id::Id("a12")));
    assert_eq!(Id::from_str("12x"), None);
    assert_eq!(Id::from_str("\"a12"), None);
    assert_eq!(Id::from_str("\"12x\""), Some(Id::StrLiteral("12x")));
}
impl Debug for SExp<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Id(id) => write!(f, "{}", id),
            Self::SList(slist) => {
                write!(f, "(")?;
                let n = slist.len();
                for i in 0..n {
                    write!(f, "{:?}", slist[i])?;
                    if i == n - 1 {
                        write!(f, ")")?;
                    } else {
                        write!(f, " ")?;
                    }
                }
                Ok(())
            }
        }
    }
}
impl<'a> SExp<'a> {
    pub fn expect_id(&self) -> anyhow::Result<&str> {
        match self {
            SExp::Id(Id::Id(s)) => Ok(s),
            _ => bail!("expect identifier, found {:?}", self),
        }
    }
}
