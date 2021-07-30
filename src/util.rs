use std::num::ParseIntError;


pub trait FromStrWithRadix: Sized {
    fn from_str_with_radix(src: &str) -> Result<Self, ParseIntError>;
}

fn parse_integer_radix(src: &str) -> (&str, u32) {
    let mut chars = src.char_indices();
    if let Some((_, '0')) = chars.next() {
        match chars.next() {
            Some((idx, c)) => {
                let radix = match c {
                    'b' => 2,
                    'o' => 8,
                    'x' => 16,
                    _ => 10
                };
                (&src[(idx + 1)..], radix)
            }
            _ => (src, 10)
        }
    } else {
        (src, 10)
    }
}

macro_rules! impl_integer_from_str {
    ($($t:ty),+) => {
        $(impl FromStrWithRadix for $t {
            fn from_str_with_radix(src: &str) -> Result<Self, ParseIntError> {
                let (slice, radix) = parse_integer_radix(src);
                <$t>::from_str_radix(slice, radix)
            }
        })+
    };
}

impl_integer_from_str!(u8, i8, u16, i16, u32, i32);