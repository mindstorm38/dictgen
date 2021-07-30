use super::{Dictionary, Entry, EntryKind, EntryAccess, EntryType, IntRange, FactoryParameters, FactoryValue, EntryValueProcessor};
use crate::util::FromStrWithRadix;
use std::io::{Result as IoResult};
use std::collections::HashMap;
use std::str::FromStr;


// Specific to dictionary parsing //

pub fn parse_dictionary_from_file(path: &str) -> IoResult<Result<Dictionary, String>> {
    std::fs::read_to_string(path).map(|content| parse_dictionary(content.as_str()))
}

pub fn parse_dictionary(input: &str) -> Result<Dictionary, String> {
    let mut dict = Dictionary::new();
    let mut temp_params = HashMap::new();
    for (i, line) in input.lines().enumerate() {
        if let Some(line) = check_line(line) {
            match parse_entry_line(line, &mut temp_params) {
                Ok(entry) => {
                    dict.add_entry(entry);
                },
                Err(err) => {
                    return Err(format!("{}\n=> At line {}: '{}'.", err, i + 1, line));
                }
            }
        }
    }
    Ok(dict)
}


fn parse_entry_line<'a, 'b>(mut line: &'a str, temp_params: &'b mut HashMap<&'a str, &'a str>) -> Result<Entry, String> {

    let index = parse_entry_index(&mut line)?;
    parse_whitespaces(&mut line, Some("entry index"))?;

    let kind = parse_entry_kind(&mut line)?;
    parse_whitespaces(&mut line, Some("entry kind"))?;

    let typ = parse_entry_type(&mut line)?;
    parse_whitespaces(&mut line, Some("entry type"))?;

    let identifier = parse_identifier(&mut line)?;
    parse_whitespaces(&mut line, Some("entry identifier"))?;

    let description = parse_string(&mut line)?;

    let mut entry = Entry::new(index, identifier, kind, typ, description);

    temp_params.clear();
    let mut param_name = None;
    for (i, part) in line.split_whitespace().enumerate() {
        if i % 2 == 0 {
            match part.chars().next() {
                Some('+') => param_name = Some(&part[1..]),
                _ => return Err(format!("Expected a '+' before the parameter name."))
            }
        } else {
            temp_params.insert(param_name.unwrap(), part);
        }
    }

    process_entry_params(&mut entry, temp_params)?;

    Ok(entry)

}


fn parse_entry_index(slice: &mut &str) -> Result<u16, String> {

    let begin = slice.find("[").ok_or_else(|| format!("No open bracket '[' for entry index."))? + 1;
    let end = begin + slice[begin..].find("]").ok_or_else(|| format!("No close bracket ']' for entry index."))?;

    if end - begin == 0 {
        Err(format!("No space for entry index in '{}'.", &slice[(begin - 1)..=end]))
    } else {

        let raw = &slice[begin..end];

        let value = if raw == "_NUL_" {
            0
        } else {
            u16::from_str_with_radix(raw).map_err(|_|
                format!("Failed to parse the entry index in '{}'.", raw)
            )?
        };

        *slice = &slice[(end + 1)..];
        Ok(value)

    }

}


fn parse_entry_kind(slice: &mut &str) -> Result<EntryKind, String> {
    let id = parse_identifier(slice)?;
    match id {
        "PAR" => Ok(EntryKind::Parameter),
        "VAR" => Ok(EntryKind::Variable),
        _ => Err(format!("Invalid entry kind '{}', expecting 'PAR' or 'VAR'.", id))
    }
}


fn parse_entry_type(slice: &mut &str) -> Result<EntryType, String> {
    let id = parse_identifier(slice)?;
    match id {
        "UINT8" => Ok(EntryType::UInt8(Default::default())),
        "INT8" => Ok(EntryType::Int8(Default::default())),
        "UINT16" => Ok(EntryType::UInt16(Default::default())),
        "INT16" => Ok(EntryType::Int16(Default::default())),
        "UINT32" => Ok(EntryType::UInt32(Default::default())),
        "INT32" => Ok(EntryType::Int32(Default::default())),
        "FLOAT" => Ok(EntryType::Float),
        "BOOL" => Ok(EntryType::Bool),
        "STRING" => Ok(EntryType::String(0)),
        "REF" => Ok(EntryType::Ref),
        _ => Err(format!("Invalid entry type '{}'.", id))
    }
}


fn get_entry_type_repr(typ: &EntryType) -> &'static str {
    match typ {
        EntryType::UInt8(_) => "UINT8",
        EntryType::Int8(_) => "INT8",
        EntryType::UInt16(_) => "UINT16",
        EntryType::Int16(_) => "INT16",
        EntryType::UInt32(_) => "UINT32",
        EntryType::Int32(_) => "INT32",
        EntryType::Float => "FLOAT",
        EntryType::Bool => "BOOL",
        EntryType::String(_) => "STRING",
        EntryType::Ref => "REF"
    }
}


fn process_entry_params(entry: &mut Entry, params: &HashMap<&str, &str>) -> Result<(), String> {

    if let Some(&val) = params.get("access") {
        entry.access = match val {
            "R" => EntryAccess::Read,
            "W" => EntryAccess::Write,
            "RW" | "WR" => EntryAccess::ReadWrite,
            _ => return Err(format!("Invalid 'access' parameter, expected 'R', 'W' or 'RW'."))
        };
    }

    match entry.typ {
        EntryType::String(ref mut length) => {
            if let Some(&val) = params.get("length") {
                match val.parse::<u32>() {
                    Ok(val) => *length = val,
                    _ => return Err(format!("Expecting a valid positive integer for the 'length' parameter."))
                }
            } else {
                return Err(format!("Expecting a 'length' parameter because the entry type is STRING."))
            }
        },
        _ => {}
    }

    let type_repr = get_entry_type_repr(&entry.typ);
    if let Some(range) = mut_entry_type_range(&mut entry.typ) {

        if let Some(&val) = params.get("min") {
            range.parse_min(val).map_err(|_|
                format!("Expecting a valid integer for 'min' parameter in type {}.", type_repr)
            )?;
        }

        if let Some(&val) = params.get("max") {
            range.parse_max(val).map_err(|_|
                format!("Expecting a valid integer for 'max' parameter in type {}.", type_repr)
            )?;
        }

    }

    if let Some(&val) = params.get("forced_value") {
        let mut value = FactoryValue::default_for_type(&entry.typ);
        if let Err(err) = parse_factory_value(val, entry.identifier.as_str(), &mut value, None) {
            return Err(format!("Failed to parse forced value parameter. {}", err))
        }
        entry.value_processor = EntryValueProcessor::Constant(value);
    }

    if let Some(&val) = params.get("value_proc") {

        if let EntryValueProcessor::Constant(_) = entry.value_processor {
            return Err(format!("Already defined a 'forced_value', cannot combine with 'value_proc' parameter."));
        }

        entry.value_processor = match val {
            "entries_size" => {
                if let EntryType::UInt16(_) = entry.typ {
                    EntryValueProcessor::Size
                } else {
                    return Err(format!("The 'entries_size' value processor requires a UINT16 type."))
                }
            },
            "entries_checksum" => {
                if let EntryType::UInt8(_) = entry.typ {
                    EntryValueProcessor::Checksum
                } else {
                    return Err(format!("The 'entries_checksum' value processor requires a UINT8 type."))
                }
            },
            _ => return Err(format!("Unknown value processor '{}'.", val))
        };

    }

    Ok(())

}


trait RangeParser {
    fn parse_min(&mut self, txt: &str) -> Result<(), ()>;
    fn parse_max(&mut self, txt: &str) -> Result<(), ()>;
}

macro_rules! impl_int_range_parsers {
    ($($t:ty),+) => {
        $(impl RangeParser for IntRange<$t> {
            fn parse_min(&mut self, txt: &str) -> Result<(), ()> {
                self.min = <$t>::from_str_with_radix(txt).map_err(|_| ())?;
                Ok(())
            }
            fn parse_max(&mut self, txt: &str) -> Result<(), ()> {
                self.max = <$t>::from_str_with_radix(txt).map_err(|_| ())?;
                Ok(())
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


pub fn parse_factory_parameters_from_file<'a, 'b>(path: &'a str, dict: &'b Dictionary) -> IoResult<Result<FactoryParameters<'b>, String>> {
    std::fs::read_to_string(path).map(|content| parse_factory_parameters(content.as_str(), dict))
}

pub fn parse_factory_parameters<'a, 'b>(input: &'a str, dict: &'b Dictionary) -> Result<FactoryParameters<'b>, String> {
    let mut params = FactoryParameters::new(dict);
    for (i, line) in input.lines().enumerate() {
        if let Some(line) = check_line(line) {
            match parse_factory_value_line(line, &mut params) {
                Err(err) => {
                    return Err(format!("{}\n=> At line {}: '{}'.", err, i + 1, line));
                },
                _ => {}
            }
        }
    }
    params.apply_value_processors();
    Ok(params)
}


fn parse_factory_value_line(mut line: &str, params: &mut FactoryParameters) -> Result<(), String> {

    let entry_identifier = parse_identifier(&mut line)?;

    let (value, dict) = params.mut_value_with_dict(entry_identifier).ok_or_else(||
        format!("Unknown entry identifier '{}'.", entry_identifier)
    )?;

    parse_whitespaces(&mut line, None).unwrap();

    match line.chars().next() {
        Some('=') => line = &line[1..],
        _ => return Err(format!("Expected an equal '=' after the entry identifier."))
    }

    parse_whitespaces(&mut line, None).unwrap();

    parse_factory_value(line, entry_identifier, value, Some(dict))?;

    Ok(())
}


fn parse_factory_value(slice: &str, entry_identifier: &str, value: &mut FactoryValue, dict: Option<&Dictionary>) -> Result<(), String> {

    macro_rules! gen_int_match {
        ($($e:ident: $t:ty),+) => {
            match value {
                $(FactoryValue::$e(actual_value, range) => {
                    Some(match <$t>::from_str_with_radix(slice) {
                        Ok(parsed_value) => {
                            if range.contains(parsed_value) {
                                *actual_value = parsed_value;
                                Ok(())
                            } else {
                                Err(format!("Integer not in range {} to {} included.", range.min, range.max))
                            }
                        },
                        _ => Err(format!("Invalid integer."))
                    })
                },)+
                _ => None
            }
        };
    }

    gen_int_match!(UInt8: u8, Int8: i8, UInt16: u16, Int16: i16, UInt32: u32, Int32: i32)
        .unwrap_or_else(|| {
            match value {
                FactoryValue::Float(actual_value) => {
                    match f32::from_str(slice) {
                        Ok(value) => {
                            *actual_value = value;
                            Ok(())
                        },
                        Err(_) => Err(format!("Invalid float."))
                    }
                },
                FactoryValue::Bool(actual_value) => {
                    match slice {
                        "true" => {
                            *actual_value = true;
                            Ok(())
                        },
                        "false" => {
                            *actual_value = false;
                            Ok(())
                        },
                        _ => Err(format!("Invalid boolean, must be 'true' of 'false'."))
                    }
                },
                FactoryValue::String(actual_value, length) => {
                    let mut slice = slice;
                    match parse_string(&mut slice) {
                        Err(err) => Err(err),
                        Ok(str) => {
                            if str.len() > *length as usize {
                                Err(format!("The string cannot fit in {} bytes.", *length))
                            } else {
                                *actual_value = str;
                                Ok(())
                            }
                        }
                    }
                },
                FactoryValue::Ref(actual_value) => {
                    match dict {
                        Some(dict) => {
                            let mut slice = slice;
                            match parse_identifier(&mut slice) {
                                Err(err) => Err(err),
                                Ok(id) => {
                                    match dict.get_entry(id) {
                                        None => Err(format!("Invalid referenced entry, not found in the dictionary.")),
                                        Some(ref_entry) => {
                                            if !matches!(ref_entry.kind, EntryKind::Variable) {
                                                Err(format!("Invalid referenced entry, not a variable."))
                                            } else if ref_entry.index == 0 {
                                                Err(format!("Invalid referenced entry, null index."))
                                            } else {
                                                *actual_value = ref_entry.index;
                                                Ok(())
                                            }
                                        }
                                    }
                                }
                            }
                        },
                        None => Err(format!("Cannot reference an entry here, the dictionary is being parsed."))
                    }
                },
                _ => Err(format!("The entry type is not supported. Contact developer."))
            }
        })
        .map_err(|err|
            format!("Failed to parse value '{}' for entry '{}'. {}",
                    slice,
                    entry_identifier,
                    err
            )
        )

}


// Common to all dictionary and factory parameters //

fn check_line(mut line: &str) -> Option<&str> {
    line = line.trim();
    if line.starts_with("#") || line.is_empty() {
        None
    } else {
        Some(line)
    }
}


fn parse_whitespaces(slice: &mut &str, followed_item: Option<&'static str>) -> Result<(), String> {
    let count = slice.chars().take_while(|&c| c == ' ').count();
    if let (Some(followed_item), 0) = (followed_item, count) {
        Err(format!("Expected a whitespace to follow the {}.", followed_item))
    } else {
        *slice = &slice[count..];
        Ok(())
    }
}


fn parse_identifier<'a, 'b>(slice: &'b mut &'a str) -> Result<&'a str, String> {

    let idx = slice.char_indices()
        .take_while(|&(_, c)| c.is_alphanumeric() || c == '_')
        .last()
        .map(|t| t.0);

    let id = match idx {
        Some(idx) => &slice[..=idx],
        None => slice
    };

    if id.is_empty() {
        Err(format!("Expected an identifier at '{}'.", slice))
    } else if !id.is_ascii() {
        Err(format!("Invalid non-ASCII character in '{}'.", id))
    } else {
        *slice = match idx {
            Some(idx) => &slice[(idx + 1)..],
            None => ""
        };
        Ok(id)
    }

}


fn parse_string(slice: &mut &str) -> Result<String, String> {

    match slice.chars().next() {
        Some('"') => {},
        _ => return Err(format!("Expected an opening string quote at '{}'.", slice))
    }

    let mut buf = String::new();
    let mut escaped = false;
    for (idx, char) in slice[1..].char_indices() {
        match char {
            '\\' if !escaped => escaped = true,
            '"' if !escaped => {
                *slice = &slice[(idx + 2)..];
                return Ok(buf);
            },
            _ => {
                buf.push(char);
                escaped = false;
            }
        }
    }

    Err(format!("Expected a closing string quote at '{}'.", slice))

}
