use std::{
    collections::HashSet,
    sync::{LazyLock, RwLock},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct SymbolId(&'static str);

impl SymbolId {
    pub fn from_str(s: &str) -> Self {
        // Fast path: check if already exists
        if let Some(&existing) = GLOBAL_SYMBOL_STORE.read().unwrap().get(s) {
            return SymbolId(existing);
        }

        let mut gss = GLOBAL_SYMBOL_STORE.write().unwrap();

        // Double-check after acquiring write lock
        if let Some(&existing) = gss.get(s) {
            return SymbolId(existing);
        }

        let static_str: &'static str = Box::leak(s.to_owned().into_boxed_str());
        gss.insert(static_str);

        SymbolId(static_str)
    }

    pub fn as_str(&self) -> &'static str {
        self.0
    }
}

impl std::fmt::Display for SymbolId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub static GLOBAL_SYMBOL_STORE: LazyLock<RwLock<HashSet<&'static str>>> =
    LazyLock::new(|| RwLock::new(HashSet::new()));
