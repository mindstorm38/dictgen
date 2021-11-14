use super::{Dictionary, Entry, EntryKind, EntryAccess, EntryType, IntRange, FactoryParameters, FactoryValue, EntryValueProcessor};
use super::lexer::{Token, TokenLexer, PreParsedInt, FromPreParsedInt};

use std::io::{Result as IoResult};
use std::str::FromStr;


// Specific to dictionary parsing //

pub fn parse_dictionary_from_file(path: &str) -> IoResult<Result<Dictionary, String>> {
    std::fs::read_to_string(path).map(|content| parse_dictionary(content.as_str()))
}

pub fn parse_dictionary(input: &str) -> Result<Dictionary, String> {

    let mut lexer = super::lexer::lex(input);
    let mut dict = Dictionary::new();

    loop {
        match lexer.next() {
            None => break,
            Some(Token::NewLine) => continue,
            Some(Token::Define(def)) => {
                dict.add_define(def.to_string());
                continue
            },
            Some(Token::OpenBracket) => {},
            _ => return Err(format!("Expected an open bracket '[' before entry index."))
        }
        match parse_entry(&mut lexer) {
            Ok(entry) => dict.add_entry(entry),
            Err(err) => return Err(format!("{} At '{:?}'.", err, lexer.slice()))
        }
    }

    Ok(dict)

}


fn parse_entry(lexer: &mut TokenLexer) -> Result<Entry, String> {

    let index = match lexer.next() {
        Some(Token::Integer(ref int)) => {
            u16::from_pre_parsed_int(int).ok_or_else(||
                format!("Given index is not an unsigned 16 bits integer."))?
        },
        Some(Token::Identifier("_NUL_")) => 0,
        _ => return Err(format!("Expected '_NUL_' or a valid index integer."))
    };

    if !matches!(lexer.next(), Some(Token::CloseBracket)) {
        return Err(format!("Expected a close bracket ']' after index."));
    }

    let kind = match lexer.next() {
        Some(Token::Identifier("PAR")) => EntryKind::Parameter,
        Some(Token::Identifier("VAR")) => EntryKind::Variable,
        _ => return Err(format!("Expected the entry kind, must be 'PAR' or 'VAR'."))
    };

    let typ_repr = match lexer.next() {
        Some(Token::Identifier(repr)) => repr,
        _ => return Err(format!("Expected an entry type."))
    };

    let typ = match typ_repr {
        "UINT8" => EntryType::UInt8(Default::default()),
        "INT8" => EntryType::Int8(Default::default()),
        "UINT16" => EntryType::UInt16(Default::default()),
        "INT16" => EntryType::Int16(Default::default()),
        "UINT32" => EntryType::UInt32(Default::default()),
        "INT32" => EntryType::Int32(Default::default()),
        "FLOAT" => EntryType::Float,
        "BOOL" => EntryType::Bool,
        "STRING" => EntryType::String(0),
        "REF" => EntryType::Ref,
        "REGION" => {
            if !matches!(kind, EntryKind::Variable) {
                return Err(format!("By convention, REGION should only be used for VAR."))
            }
            EntryType::Region
        },
        "STRUCT" => EntryType::Struct(String::new()),
        _ => return Err(format!("Invalid entry type '{}'.", typ_repr)),
    };

    let identifier = match lexer.next() {
        Some(Token::Identifier(id)) => id,
        _ => return Err(format!("Expected an entry identifier."))
    };

    let description = match lexer.next() {
        Some(Token::String(desc)) => desc,
        _ => return Err(format!("Expected an entry description string."))
    };

    let mut entry = Entry::new(index, identifier, kind, typ, description);

    loop {

        let param_name = match lexer.next() {
            Some(Token::Plus) => {
                match lexer.next() {
                    Some(Token::Identifier(param_name)) => param_name,
                    _ => return Err(format!("Expected parameter name after plus sign, following '+<param> <value>'."))
                }
            }
            Some(Token::Identifier(param_name)) => {
                match lexer.next() {
                    Some(Token::Equal) => param_name,
                    _ => return Err(format!("Expected an equal sign after parameter name, following '{}=<value>'.", param_name))
                }
            }
            Some(Token::NewLine) => {
                break
            }
            _ => return Err(format!("Expected either '+<param> <value>' or '<param>=<value>'."))
        };

        match param_name {
            "access" => {
                entry.access = match lexer.next() {
                    Some(Token::Identifier("R")) => EntryAccess::Read,
                    Some(Token::Identifier("W")) => EntryAccess::Write,
                    Some(Token::Identifier("RW" | "WR")) => EntryAccess::ReadWrite,
                    _ => return Err(format!("Invalid 'access' parameter, expected 'R', 'W' or 'RW'."))
                }
            }
            "length" => {
                if let EntryType::String(length) = &mut entry.typ {
                    *length = match lexer.next() {
                        Some(Token::Integer(ref int)) => {
                            u32::from_pre_parsed_int(int).ok_or_else(||
                                format!("Given STRING length must be an unsigned 32 bits integer."))?
                        },
                        _ => return Err(format!("Expected an integer for the STRING length."))
                    };
                } else {
                    return Err(format!("Invalid 'length' parameter for type {}, only usable on STRING.", typ_repr))
                }
            }
            "min" | "max" => {
                if let Some(range) = mut_entry_type_range(&mut entry.typ) {
                    if let Some(Token::Integer(ref int)) = lexer.next() {
                        if param_name == "min" {
                            range.parse_min(int)
                        } else {
                            range.parse_max(int)
                        }.ok_or_else(||
                            format!("Given value for '{}' must be a valid integer for type {}.", param_name, typ_repr))?
                    } else {
                        return Err(format!("Invalid '{}' parameter value, expected an integer.", param_name))
                    }
                } else {
                    return Err(format!("Invalid '{}' parameter for type {}, only usable on integers.", param_name, typ_repr))
                }
            }
            "value" | "forced_value" => {
                entry.value_processor = EntryValueProcessor::Constant(parse_factory_value(lexer, &entry, None)?)
            }
            "value_proc" => {
                entry.value_processor = match lexer.next() {
                    Some(Token::Identifier("entries_size")) => {
                        if let EntryType::UInt16(_) = &mut entry.typ {
                            EntryValueProcessor::Size
                        } else {
                            return Err(format!("The value processor 'entries_size' is only usable on UINT16 entries."))
                        }
                    },
                    Some(Token::Identifier(unknown_proc)) => return Err(format!("Unknown value processor '{}'.", unknown_proc)),
                    _ => return Err(format!("Expected a valid value processor."))
                };
            }
            "struct" => {
                if let EntryType::Struct(s) = &mut entry.typ {
                    match lexer.next() {
                        Some(Token::Identifier(struct_id)) => s.push_str(struct_id),
                        _ => return Err(format!("Expected a struct identifier for the STRUCT's name."))
                    }
                } else {
                    return Err(format!("Invalid 'struct' parameter for type {}, only usable on STRUCT.", typ_repr))
                }
            }
            _ => return Err(format!("Unknown entry parameter '{}'.", param_name))
        }

    }

    Ok(entry)

}


trait RangeParser {
    fn parse_min(&mut self, int: &PreParsedInt) -> Option<()>;
    fn parse_max(&mut self, int: &PreParsedInt) -> Option<()>;
}

macro_rules! impl_int_range_parsers {
    ($($t:ty),+) => {
        $(impl RangeParser for IntRange<$t> {
            fn parse_min(&mut self, int: &PreParsedInt) -> Option<()> {
                self.min = <$t>::from_pre_parsed_int(int)?;
                Some(())
            }
            fn parse_max(&mut self, int: &PreParsedInt) -> Option<()> {
                self.max = <$t>::from_pre_parsed_int(int)?;
                Some(())
            }
        })+
    };
}

impl_int_range_parsers!(u8, i8, u16, i16, u32, i32);

fn mut_entry_type_range(entry_type: &mut EntryType) -> Option<&mut dyn RangeParser> {
    use EntryType::*;
    match entry_type {
        UInt8(range) => Some(range),
        Int8(range) => Some(range),
        UInt16(range) => Some(range),
        Int16(range) => Some(range),
        UInt32(range) => Some(range),
        Int32(range) => Some(range),
        _ => None
    }
}


// Specific to factory parameters parsing //


pub fn parse_factory_parameters_from_file(path: &str, params: &mut FactoryParameters) -> IoResult<Result<(), String>> {
    std::fs::read_to_string(path).map(|content| parse_factory_parameters(content.as_str(), params))
}

pub fn parse_factory_parameters(input: &str, params: &mut FactoryParameters) -> Result<(), String> {

    let mut lexer = super::lexer::lex(input);

    loop {
        match lexer.next() {
            None => break,
            Some(Token::NewLine) => continue,
            Some(Token::Identifier(entry_id)) => {
                if let Some(entry) = params.dict.get_entry(entry_id) {
                    if let Some(Token::Equal) = lexer.next() {
                        let value = parse_factory_value(&mut lexer, &**entry, Some(params.dict))?;
                        params.add_value_unchecked(entry.identifier.clone(), value);
                    } else {
                        return Err(format!("Expected an equal sign after entry identifier and before factory value."))
                    }
                } else {
                    return Err(format!("Entry '{}' not found in the dictionary.", entry_id))
                }
            },
            _ => return Err(format!("Expected an entry identifier."))
        }
    }

    Ok(())

}


fn parse_factory_value(lexer: &mut TokenLexer, entry: &Entry, dict: Option<&Dictionary>) -> Result<FactoryValue, String> {

    macro_rules! gen_int_parse_funcs {
        ($($func:ident($enum:ident, $typ:ty, $typ_repr:literal)),+) => {
            $(
            fn $func(lexer: &mut TokenLexer, range: &IntRange<$typ>) -> Result<FactoryValue, String> {
                match lexer.next() {
                    Some(Token::Integer(ref int)) => {
                        match <$typ>::from_pre_parsed_int(int) {
                            Some(val) => {
                                if range.contains(val) {
                                    Ok(FactoryValue::$enum(val))
                                } else {
                                    Err(format!("Invalid integer, not within the entry range {}-{}.", range.min, range.max))
                                }
                            },
                            None => Err(format!("Invalid {} integer.", $typ_repr))
                        }
                    },
                    _ => Err(format!("Expected a valid {} integer.", $typ_repr))
                }
            }
            )+
        }
    }

    gen_int_parse_funcs!(
        parse_u8(UInt8, u8, "unsigned 8 bits"),
        parse_i8(Int8, i8, "signed 8 bits"),
        parse_u16(UInt16, u16, "unsigned 16 bits"),
        parse_i16(Int16, i16, "signed 16 bits"),
        parse_u32(UInt32, u32, "unsigned 32 bits"),
        parse_i32(Int32, i32, "signed 32 bits")
    );

    match entry.typ {
        EntryType::UInt8(ref range) => parse_u8(lexer, range),
        EntryType::Int8(ref range) => parse_i8(lexer, range),
        EntryType::UInt16(ref range) => parse_u16(lexer, range),
        EntryType::Int16(ref range) => parse_i16(lexer, range),
        EntryType::UInt32(ref range) => parse_u32(lexer, range),
        EntryType::Int32(ref range) => parse_i32(lexer, range),
        EntryType::Float => {
            match lexer.next() {
                Some(Token::Float) => {
                    match f32::from_str(lexer.slice()) {
                        Ok(val) => Ok(FactoryValue::Float(val)),
                        _ => Err(format!("Invalid 32 bits floating point."))
                    }
                }
                _ => Err(format!("Expected a valid decimal number 'xxx.xxx'."))
            }
        }
        EntryType::Bool => {
            match lexer.next() {
                Some(Token::Identifier("true")) => Ok(FactoryValue::Bool(true)),
                Some(Token::Identifier("false")) => Ok(FactoryValue::Bool(false)),
                _ => Err(format!("Expected a valid boolean 'true' or 'false'."))
            }
        }
        EntryType::String(limit) => {
            match lexer.next() {
                Some(Token::String(str)) => {
                    if str.len() > limit as usize {
                        Err(format!("The given string '{}' is too long for the limit of {} bytes.", str, limit))
                    } else {
                        Ok(FactoryValue::String(str))
                    }
                },
                _ => Err(format!("Expected a valid string."))
            }
        }
        EntryType::Ref => {
            if let Some(dict) = dict {
                if let Some(Token::Identifier(ref_id)) = lexer.next() {
                    if let Some(ref_entry) = dict.get_entry(ref_id) {
                        if !matches!(ref_entry.kind, EntryKind::Variable) {
                            Err(format!("Invalid referenced entry, not a variable."))
                        } else if ref_entry.index == 0 {
                            Err(format!("The referenced entry has a null index."))
                        } else {
                            Ok(FactoryValue::Ref(ref_entry.index))
                        }
                    } else {
                        Err(format!("Referenced entry '{}' not found in the dictionary.", ref_id))
                    }
                } else {
                    Err(format!("Expected a valid entry identifier to reference."))
                }
            } else {
                Err(format!("Cannot decode REF entry type when dictionary is partially loaded."))
            }
        }
        EntryType::Region => {
            Err(format!("The REGION type can't have factory value."))
        }
        EntryType::Struct(_) => {
            Err(format!("The STRUCT type can't have factory value."))
        }
    }

}
