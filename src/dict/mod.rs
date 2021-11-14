use std::collections::HashMap;
use std::rc::Rc;

pub mod parser;
pub mod source;
pub mod lexer;


#[derive(Debug)]
pub enum EntryType {
    UInt8(IntRange<u8>),
    Int8(IntRange<i8>),
    UInt16(IntRange<u16>),
    Int16(IntRange<i16>),
    UInt32(IntRange<u32>),
    Int32(IntRange<i32>),
    Float,
    Bool,
    String(u32),
    Ref,
    Region,
    Struct(String)
}

#[derive(Debug, Clone)]
pub struct IntRange<T> {
    min: T,
    max: T
}

macro_rules! impl_int_ranges {
    ($($t:ty),+) => {
        $(
        impl Default for IntRange<$t> {
            fn default() -> Self {
                Self { min: <$t>::MIN, max: <$t>::MAX }
            }
        }
        impl IntRange<$t> {
            fn contains(&self, n: $t) -> bool {
                self.min <= n && n <= self.max
            }
        }
        )+
    };
}

impl_int_ranges!(u8, i8, u16, i16, u32, i32);


#[derive(Debug, Copy, Clone)]
pub enum EntryAccess {
    Read,
    Write,
    ReadWrite
}


#[derive(Debug, Copy, Clone)]
pub enum EntryKind {
    Parameter,
    Variable
}


#[derive(Debug)]
pub enum EntryValueProcessor {
    Constant(FactoryValue),
    Size,
    None,
}


#[derive(Debug)]
pub struct Entry {
    pub index: u16,
    pub identifier: String,
    pub kind: EntryKind,
    pub typ: EntryType,
    pub description: String,
    pub access: EntryAccess,
    pub value_processor: EntryValueProcessor
}

impl Entry {

    pub fn new(index: u16, identifier: &str, kind: EntryKind, typ: EntryType, description: String) -> Entry {
        Entry {
            index,
            identifier: identifier.to_string(),
            kind,
            typ,
            description,
            access: EntryAccess::ReadWrite,
            value_processor: EntryValueProcessor::None
        }
    }

}


#[derive(Debug)]
pub struct Dictionary {
    entries: Vec<Rc<Entry>>,
    entries_by_id: HashMap<String, Rc<Entry>>,
    parameters_count: usize,
    defines: Vec<String>
}

impl Dictionary {

    pub fn new() -> Dictionary {
        Dictionary {
            entries: Vec::new(),
            entries_by_id: HashMap::new(),
            parameters_count: 0,
            defines: Vec::new()
        }
    }

    pub fn add_entry(&mut self, entry: Entry) {
        if let EntryKind::Parameter = entry.kind {
            self.parameters_count += 1;
        }
        let entry = Rc::new(entry);
        self.entries_by_id.insert(entry.identifier.clone(), Rc::clone(&entry));
        self.entries.push(entry);
    }

    pub fn get_entry(&self, identifier: &str) -> Option<&Rc<Entry>> {
        self.entries_by_id.get(identifier)
    }

    pub fn get_entries(&self) -> &[Rc<Entry>] {
        &self.entries[..]
    }

    pub fn add_define(&mut self, define: String) {
        self.defines.push(define);
    }

    pub fn get_defines(&self) -> &[String] {
        &self.defines[..]
    }

}


#[derive(Debug, Clone)]
pub enum FactoryValue {
    UInt8(u8),
    Int8(i8),
    UInt16(u16),
    Int16(i16),
    UInt32(u32),
    Int32(i32),
    Float(f32),
    Bool(bool),
    String(String),
    Ref(u16)
}

impl FactoryValue {

    pub fn default_for_type(typ: &EntryType) -> Option<Self> {
        Some(match typ {
            EntryType::UInt8(range) => FactoryValue::UInt8(range.min),
            EntryType::Int8(range) => FactoryValue::Int8(range.min),
            EntryType::UInt16(range) => FactoryValue::UInt16(range.min),
            EntryType::Int16(range) => FactoryValue::Int16(range.min),
            EntryType::UInt32(range) => FactoryValue::UInt32(range.min),
            EntryType::Int32(range) => FactoryValue::Int32(range.min),
            EntryType::Float => FactoryValue::Float(0.0),
            EntryType::Bool => FactoryValue::Bool(false),
            EntryType::String(_) => FactoryValue::String(String::new()),
            EntryType::Ref => FactoryValue::Ref(0),
            EntryType::Region => return None,
            EntryType::Struct(_) => return None
        })
    }

    pub fn is_type_supported(typ: &EntryType) -> bool {
        !matches!(typ, EntryType::Region | EntryType::Struct(_))
    }

}


pub struct FactoryParameters<'a> {
    pub dict: &'a Dictionary,
    values: HashMap<String, FactoryValue>
}

impl<'a> FactoryParameters<'a> {

    pub fn new(dict: &'a Dictionary) -> FactoryParameters<'a> {
        FactoryParameters {
            dict,
            values: HashMap::new()
        }
    }

    pub fn add_value_unchecked(&mut self, identifier: String, value: FactoryValue) {
        self.values.insert(identifier, value);
    }

    pub fn fix_missing_values(&mut self) {

        let mut entries_with_size_proc = Vec::new();

        for entry in &self.dict.entries {
            if let EntryKind::Parameter = entry.kind {
                if !self.values.contains_key(&entry.identifier) {

                    match entry.value_processor {
                        EntryValueProcessor::Constant(ref value) => {
                            self.add_value_unchecked(entry.identifier.clone(), value.clone());
                        }
                        EntryValueProcessor::Size => {
                            if let EntryType::UInt16(_) = entry.typ {
                                entries_with_size_proc.push(&**entry);
                            } else {
                                panic!("Unexpected size value processor for non-u16 parameter.");
                            }
                        }
                        EntryValueProcessor::None => {
                            match FactoryValue::default_for_type(&entry.typ) {
                                Some(value ) => self.add_value_unchecked(entry.identifier.clone(), value),
                                None => {
                                    // REGION and STRUCT entries does not support factory values.
                                    // panic!("Unexpected entry type '{:?}', factory parameter is not supported for it.", entry.typ)
                                }
                            }
                        }
                    }

                }
            }
        }

        for entry_with_size_proc in entries_with_size_proc {
            self.add_value_unchecked(
                entry_with_size_proc.identifier.clone(),
                FactoryValue::UInt16(self.dict.parameters_count as u16)
            );
        }

        // This assertion is no longer needed because with no longer panic if the no factory value
        // is supported.
        // assert_eq!(self.values.len(), self.dict.parameters_count, "Factory parameters length and parameters count in dictionary should be equals after fix.");

    }

    pub fn each_values_ordered(&self, mut callback: impl FnMut(&Entry, &FactoryValue)) {
        for entry in &self.dict.entries {
            if let EntryKind::Parameter = entry.kind {
                if let Some(value) = self.values.get(&entry.identifier) {
                    callback(&**entry, value);
                }
            }
        }
    }

}
