use logos::{Logos, Lexer};
use std::num::ParseIntError;


#[derive(Logos, Debug)]
pub enum Token<'source> {
    #[regex("[+-]?0b[01]+", |lex| decode_num(lex, 2, 2))]
    #[regex("[+-]?0o[0-7]+", |lex| decode_num(lex, 8, 2))]
    #[regex("[+-]?0x[0-9a-fA-F]+", |lex| decode_num(lex, 16, 2))]
    #[regex("[+-]?[0-9]+", |lex| decode_num(lex, 10, 0))]
    Integer(PreParsedInt<'source>),
    #[regex("[+-]?[0-9]+[.][0-9]+")]
    Float,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,
    #[regex("[a-zA-Z_][a-zA-Z0-9_]*", set_slice)]
    Identifier(&'source str),
    #[regex("\"(\\\\.|[^\"\\\\])*\"", decode_string)]
    String(String),
    #[token("\n")]
    NewLine,
    #[token("+")]
    Plus,
    #[token("=")]
    Equal,
    #[regex("#define ")]
    Define,  // TODO
    #[error]
    #[regex("[ \\t\\r\\f]+", logos::skip)]
    #[regex("#[^\\r\\n]*", logos::skip)]
    Error
}


#[derive(Debug)]
pub struct PreParsedInt<'source> {
    negative: bool,
    radix: u32,
    slice: &'source str
}

pub trait FromPreParsedInt: Sized {
    fn from_pre_parsed_int(pre_parsed: &PreParsedInt) -> Option<Self>;
}

macro_rules! impl_from_pre_parsed_int {
    ($($t:ty),+) => {
        $(impl FromPreParsedInt for $t {
            fn from_pre_parsed_int(pre_parsed: &PreParsedInt) -> Option<Self> {
                let value = <$t>::from_str_radix(pre_parsed.slice, pre_parsed.radix).ok()?;
                if pre_parsed.negative {
                    value.checked_neg()
                } else {
                    Some(value)
                }
            }
        })+
    };
}

impl_from_pre_parsed_int!(u8, i8, u16, i16, u32, i32);


pub type TokenLexer<'source> = Lexer<'source, Token<'source>>;


fn decode_num<'source>(lex: &mut TokenLexer<'source>, radix: u32, offset: usize) -> Option<PreParsedInt<'source>> {
    let slice: &'source str = lex.slice();
    let (start, negative) = match slice.chars().next() {
        Some('+') => (1, false),
        Some('-') => (1, true),
        _ => (0, false)
    };
    Some(PreParsedInt {
        negative,
        radix,
        slice: &slice[(offset + start)..]
    })
}


fn decode_string<'source>(lex: &mut TokenLexer<'source>) -> Option<String> {
    let slice: &'source str = lex.slice();
    let slice = &slice[1..(slice.len() - 1)];
    let mut buf = String::new();
    let mut escaped = false;
    for ch in slice.chars() {
        match ch {
            '\\' if !escaped => escaped = true,
            _ => {
                buf.push(ch);
                escaped = false;
            }
        }
    }
    Some(buf)
}


fn set_slice<'source>(lex: &mut TokenLexer<'source>) -> Option<&'source str> {
    Some(lex.slice())
}


pub fn lex(input: &str) -> TokenLexer {
    Token::lexer(input)
}
