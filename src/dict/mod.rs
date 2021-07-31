use std::collections::HashMap;
use std::rc::Rc;

pub mod parser;
pub mod source;


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
    entries_by_id: HashMap<String, Rc<Entry>>
}

impl Dictionary {

    pub fn new() -> Dictionary {
        Dictionary {
            entries: Vec::new(),
            entries_by_id: HashMap::new()
        }
    }

    pub fn add_entry(&mut self, entry: Entry) {
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

}


#[derive(Debug, Clone)]
pub enum FactoryValue {
    UInt8(u8, IntRange<u8>),
    Int8(i8, IntRange<i8>),
    UInt16(u16, IntRange<u16>),
    Int16(i16, IntRange<i16>),
    UInt32(u32, IntRange<u32>),
    Int32(i32, IntRange<i32>),
    Float(f32),
    Bool(bool),
    String(String, u32),
    Ref(u16)
}

impl FactoryValue {

    pub fn default_for_type(typ: &EntryType) -> Self {
        match typ {
            EntryType::UInt8(range) => FactoryValue::UInt8(range.min, range.clone()),
            EntryType::Int8(range) => FactoryValue::Int8(range.min, range.clone()),
            EntryType::UInt16(range) => FactoryValue::UInt16(range.min, range.clone()),
            EntryType::Int16(range) => FactoryValue::Int16(range.min, range.clone()),
            EntryType::UInt32(range) => FactoryValue::UInt32(range.min, range.clone()),
            EntryType::Int32(range) => FactoryValue::Int32(range.min, range.clone()),
            EntryType::Float => FactoryValue::Float(0.0),
            EntryType::Bool => FactoryValue::Bool(false),
            EntryType::String(length) => FactoryValue::String(String::new(), *length),
            EntryType::Ref => FactoryValue::Ref(0)
        }
    }

}


pub struct FactoryParameters<'a> {
    pub dict: &'a Dictionary,
    values: HashMap<String, FactoryValue>
}

impl<'a> FactoryParameters<'a> {

    pub fn new(dict: &'a Dictionary) -> FactoryParameters<'a> {

        let mut post_proc_size = Vec::new();

        let mut values = HashMap::new();
        for entry in &dict.entries {
            if let EntryKind::Parameter = entry.kind {

                let mut value = FactoryValue::default_for_type(&entry.typ);

                match &entry.value_processor {
                    EntryValueProcessor::Constant(const_value) => {
                        if std::mem::discriminant(&value) == std::mem::discriminant(&const_value) {
                            value = const_value.clone();
                        }
                    }
                    EntryValueProcessor::Size => {
                        post_proc_size.push(&entry.identifier);
                    }
                    EntryValueProcessor::None => {}
                }

                values.insert(entry.identifier.clone(), value);

            }
        }

        let values_count = values.len();
        for entry_id in post_proc_size {
            if let Some(value) = values.get_mut(entry_id) {
                if let FactoryValue::UInt16(size, _) = value {
                    *size = values_count as u16
                }
            }
        }

        FactoryParameters {
            dict,
            values
        }

    }

    pub fn mut_value_with_dict(&mut self, identifier: &str) -> Option<(&mut FactoryValue, &Dictionary)> {
        let values = &mut self.values;
        let dict = self.dict;
        values.get_mut(identifier).map(|v| (v, dict))
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
