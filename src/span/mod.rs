use std::collections::HashMap;
use std::sync::RwLock;
use std::ops::{Deref, DerefMut};

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

    pub fn get_sym(&self, string: &str) -> Option<Symbol> {
        Some(Symbol {
            index: *self.map.get(string)?
        })
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct Symbol {
    pub index: usize
}

#[repr(transparent)]
#[derive(Debug, Copy, Clone)]
pub struct BytePos(pub usize);

pub struct SessionGlobals {
    pub interner: RwLock<StringInterner>,
    // We guarantee that the source string will not be freed before the SessionGlobals structs is destroyed
    pub src: &'static str
}

scoped_thread_local!(static SESSION_GLOBALS: SessionGlobals);

impl SessionGlobals {
    pub fn new(src: &'static str) -> Self {
        Self {
            interner: RwLock::new(StringInterner::new()),
            src
        }
    }

    pub fn new_set(src: &'static str, f: impl FnOnce()) {
        SESSION_GLOBALS.set(&Self::new(src), f);
    }

    pub fn with<T>(f: impl FnOnce(&Self) -> T) -> T {
        SESSION_GLOBALS.with(f)
    }

    pub fn with_interner<T>(f: impl FnOnce(&StringInterner) -> T) -> T {
        Self::with(|session_globals| {
            let interner = session_globals.interner.read().unwrap();
            f(interner.deref())
        })
    }

    pub fn with_interner_mut<T>(f: impl FnOnce(&mut StringInterner) -> T) -> T {
        Self::with(|session_globals| {
            let mut interner = session_globals.interner.write().unwrap();
            f(interner.deref_mut())
        })
    }

    pub fn with_src<T>(f: impl FnOnce(&str) -> T) -> T {
        Self::with(|session_globals| {
            f(session_globals.src)
        })
    }
}