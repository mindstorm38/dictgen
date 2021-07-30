use std::io::{Write as IoWrite, BufWriter, Result as IoResult};
use std::fmt::Write as FmtWrite;
use std::fs::File;

use super::{Dictionary, EntryKind, EntryType, FactoryParameters};
use crate::dict::{FactoryValue, EntryAccess};
use std::path::Path;


static MACRO_ENTRY_COUNT: &str = "DICTIONARY_ENTRY_COUNT";
static TYPEDEF_VAR_TYPE: &str = "vartype_e";
static TYPEDEF_ACCESS_TYPE: &str = "accesstype_e";
static TYPEDEF_ENTRY: &str = "DictionaryEntry";
static TYPEDEF_PARAMETERS_TABLE: &str = "parametersTable_t";
static TYPEDEF_VARIABLES_TABLE: &str = "variablesTable_t";
static VAR_PARAMETERS: &str = "parameters";
static VAR_VARIABLES: &str = "variables";
static VAR_FACTORY_TABLE: &str = "factory_VarTable";
static VAR_DICTIONARY: &str = "dictionary";


#[allow(unused_must_use)]
pub fn build_header<P: AsRef<Path>, Q: AsRef<Path>>(dict: &Dictionary, path: P, include_guard: &str, header_path: Option<Q>) -> IoResult<()> {

    let mut writer = BufWriter::new(File::create(path)?);

    let header_reader = match header_path.map(|path| File::open(path)) {
        Some(Err(e)) => return Err(e),
        Some(Ok(reader)) => Some(reader),
        None => None
    };

    writeln!(writer, "#ifndef {}", include_guard);
    writeln!(writer, "#define {}\n", include_guard);

    if let Some(mut reader) = header_reader {
        std::io::copy(&mut reader, &mut writer);
        writeln!(writer);
    }

    writeln!(writer, "#define {} {}\n", MACRO_ENTRY_COUNT, dict.entries.len());

    write_typedef_enum(&mut writer, TYPEDEF_VAR_TYPE, &[
        "VAR_UINT8",
        "VAR_INT8",
        "VAR_UINT16",
        "VAR_INT16",
        "VAR_UINT32",
        "VAR_INT32",
        "VAR_FLOAT",
        "VAR_STRING"
    ]);

    writeln!(writer);

    write_typedef_enum(&mut writer, TYPEDEF_ACCESS_TYPE, &[
        "READ = 1", "WRITE = 2", "READ_WRITE = 3"
    ]);

    writeln!(writer);

    let struct_var_type = format!("const {} type", TYPEDEF_VAR_TYPE);
    let struct_access_type = format!("const {} access", TYPEDEF_ACCESS_TYPE);

    write_typedef_struct(&mut writer, TYPEDEF_ENTRY, &[
        "uint16_t index",
        "const char* name",
        struct_var_type.as_str(),
        "uint16_t length",
        struct_access_type.as_str(),
        "void* ptr",
        "const int32_t min",
        "const int32_t max"
    ]);

    writeln!(writer);

    let mut parameters_struct = String::new();
    let mut variables_struct = String::new();

    for entry in dict.get_entries() {

        let struct_buf = match entry.kind {
            EntryKind::Parameter => &mut parameters_struct,
            EntryKind::Variable => &mut variables_struct
        };

        let field_type = match entry.typ {
            EntryType::UInt8(_) => "uint8_t",
            EntryType::Int8(_) => "int8_t",
            EntryType::UInt16(_) => "uint16_t",
            EntryType::Int16(_) => "int16_t",
            EntryType::UInt32(_) => "uint32_t",
            EntryType::Int32(_) => "int32_t",
            EntryType::Float => "float",
            EntryType::Bool => "uint8_t",
            EntryType::String(_) => "uint8_t",
            EntryType::Ref => "uint16_t",
        };

        let field_array = match entry.typ {
            EntryType::String(length) => format!("[{}]", length),
            _ => "".to_string()
        };

        writeln!(struct_buf, "\t{} {}{}; // {}", field_type, entry.identifier, field_array, entry.description);

    }

    writeln!(writer, "typedef struct {{");
    write!(writer, "{}", parameters_struct);
    writeln!(writer, "}} {};\n", TYPEDEF_PARAMETERS_TABLE);
    writeln!(writer, "typedef struct {{");
    write!(writer, "{}", variables_struct);
    writeln!(writer, "}} {};\n", TYPEDEF_VARIABLES_TABLE);
    writeln!(writer, "extern {} {};", TYPEDEF_PARAMETERS_TABLE, VAR_PARAMETERS);
    writeln!(writer, "extern {} {};", TYPEDEF_VARIABLES_TABLE, VAR_VARIABLES);
    writeln!(writer, "extern const {} {};", TYPEDEF_PARAMETERS_TABLE, VAR_FACTORY_TABLE);
    writeln!(writer, "extern const {} {}[];\n", TYPEDEF_ENTRY, VAR_DICTIONARY);
    writeln!(writer, "#endif");

    Ok(())

}


#[allow(unused_must_use)]
pub fn build_source<P: AsRef<Path>>(factory_parameters: &FactoryParameters, path: P, include_path: &str) -> IoResult<()> {

    let mut writer = BufWriter::new(File::create(path)?);

    writeln!(writer, "#include \"{}\"\n", include_path);

    writeln!(writer, "{} {};", TYPEDEF_PARAMETERS_TABLE, VAR_PARAMETERS);
    writeln!(writer, "{} {};", TYPEDEF_VARIABLES_TABLE, VAR_VARIABLES);

    writeln!(writer, "\nconst {} {} = {{", TYPEDEF_PARAMETERS_TABLE, VAR_FACTORY_TABLE);

    factory_parameters.each_values_ordered(|entry, value| {
        writeln!(writer, "\t{}, // {} ({})", get_factory_value_repr(value), entry.identifier, entry.description);
    });

    writeln!(writer, "}};\n");

    writeln!(writer, "const {} {}[] = {{", TYPEDEF_ENTRY, VAR_DICTIONARY);

    for entry in factory_parameters.dict.get_entries() {

        let entry = &**entry;
        if entry.index != 0 {

            let (type_enum, type_len, min, max) = match entry.typ {
                EntryType::UInt8(ref range) => ("VAR_UINT8", 1, range.min as i32, range.max as i32),
                EntryType::Int8(ref range) => ("VAR_INT8", 1, range.min as i32, range.max as i32),
                EntryType::UInt16(ref range) => ("VAR_UINT16", 2, range.min as i32, range.max as i32),
                EntryType::Int16(ref range) => ("VAR_INT16", 2, range.min as i32, range.max as i32),
                EntryType::UInt32(ref range) => ("VAR_UINT32", 4, range.min.max(i32::MAX as u32) as i32, range.max.max(i32::MAX as u32) as i32),
                EntryType::Int32(ref range) => ("VAR_INT32", 4, range.min as i32, range.max as i32),
                EntryType::Float => ("VAR_FLOAT", 4, 0, 0),
                EntryType::Bool => ("VAR_UINT8", 1, 0, 1),
                EntryType::String(len) => ("VAR_STRING", len, 0, 0),
                EntryType::Ref => ("VAR_UINT16", 2, 0, u16::MAX as i32)
            };

            let access_type_enum = match entry.access {
                EntryAccess::Read => "READ",
                EntryAccess::Write => "WRITE",
                EntryAccess::ReadWrite => "READ_WRITE"
            };

            let pointer_prefix = match entry.kind {
                EntryKind::Parameter => VAR_PARAMETERS,
                EntryKind::Variable => VAR_VARIABLES,
            };

            writeln!(writer, "\t{{ 0x{:04X}, \"{}\", {}, {}, {}, &{}.{}, {}, {} }},",
                     entry.index,
                     entry.identifier,
                     type_enum,
                     type_len,
                     access_type_enum,
                     pointer_prefix,
                     entry.identifier,
                     min, max
            );

        }

    }
    writeln!(writer, "}};\n");

    Ok(())

}


#[allow(unused_must_use)]
fn write_typedef_enum(writer: &mut impl IoWrite, name: &str, items: &[&str]) -> IoResult<()> {
    writeln!(writer, "typedef enum {{");
    for &typ in items {
        writeln!(writer, "\t{},", typ);
    }
    writeln!(writer, "}} {};", name);
    Ok(())
}


#[allow(unused_must_use)]
fn write_typedef_struct(writer: &mut impl IoWrite, name: &str, items: &[&str]) -> IoResult<()> {
    writeln!(writer, "typedef struct {{");
    for &typ in items {
        writeln!(writer, "\t{};", typ);
    }
    writeln!(writer, "}} {};", name);
    Ok(())
}


fn get_factory_value_repr(value: &FactoryValue) -> String {
    match value {
        FactoryValue::UInt8(val, _) => val.to_string(),
        FactoryValue::Int8(val, _) => val.to_string(),
        FactoryValue::UInt16(val, _) => val.to_string(),
        FactoryValue::Int16(val, _) => val.to_string(),
        FactoryValue::UInt32(val, _) => val.to_string(),
        FactoryValue::Int32(val, _) => val.to_string(),
        FactoryValue::Float(val) => val.to_string(),
        FactoryValue::Bool(val) => (if *val { "1" } else { "0" }).to_string(),
        FactoryValue::String(val, _) => format!("{:?}", val),
        FactoryValue::Ref(val) => format!("0x{:04X}", *val)
    }
}
