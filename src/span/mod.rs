use std::collections::HashMap;

pub struct StringInterner {
    strings: Vec<&'static str>,
    map: HashMap<&'static str, usize>
}

impl StringInterner {
    pub fn new() -> Self {
        Self {
            strings: vec![],
            map: HashMap::new()
        }
    }

    pub fn intern(&mut self, string: &str) -> Symbol {
        if let Some(index) = self.map.get(string) {
            return Symbol { index: *index };
        }

        let boxed_slice: Box<str> = string.into();
        let leaked: &'static str = Box::leak(boxed_slice);

        let symbol = Symbol { index: self.strings.len() };
        self.strings.push(leaked);
        self.map.insert(leaked, symbol.index);

        symbol
    }

    pub fn get(&self, symbol: &Symbol) -> Option<&'static str> {
        if let Some(string) = self.strings.get(symbol.index) {
            Some(*string)
        }
        else {
            None
        }
    }
}

#[derive(Debug)]
pub struct Symbol {
    pub index: usize
}

#[repr(transparent)]
#[derive(Debug, Copy, Clone)]
pub struct BytePos(pub usize);