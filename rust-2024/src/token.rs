use anyhow::*;
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token<'a> {
    Open,
    Close,
    Id(&'a str),
}

impl<'a> Token<'a> {
    pub fn from_str(s: &str) -> anyhow::Result<Vec<Token>> {
        let mut ret = vec![];
        let mut str_begin = None;
        for (i, c) in s.chars().enumerate() {
            if !Self::is_separator(c) && !Self::is_in_id(c) {
                bail!("invalid charactor '{}'", c)
            }
            if Self::is_separator(c) {
                if let Some(bgn) = str_begin {
                    ret.push(Token::Id(&s[bgn..i]));
                    str_begin = None;
                }
            } else {
                if str_begin.is_none() {
                    str_begin = Some(i)
                }
            }
            match c {
                ')' => ret.push(Token::Close),
                '(' => ret.push(Token::Open),
                _ => (),
            }
        }
        Ok(ret)
    }
    fn is_in_id(c: char) -> bool {
        matches!(c,
            'a'..='z'
            | 'A'..='Z'
            | '0'..='9'
            | '!' | '$' | '%' | '&' | '*' | '+' | '-'
            | '.' | '/' | '<' | '=' | '>' | '?' | '@'
            | '^' | '_' | '#' | '"' | '\'' | '\\'
        )
    }
    fn is_separator(c: char) -> bool {
        c.is_ascii_whitespace() || c == ')' || c == '('
    }
}
impl<'a> ToString for Token<'a> {
    fn to_string(&self) -> String {
        match self {
            Token::Open => "(".to_string(),
            Token::Close => ")".to_string(),
            Token::Id(id) => id.to_string(),
        }
    }
}
