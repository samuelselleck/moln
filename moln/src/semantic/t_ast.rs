use std::collections::HashMap;

use crate::symbol_interning::SymbolId;

pub struct TAst {
    pub functions: HashMap<SymbolId, TFunction>,
}

pub struct TFunction {}
