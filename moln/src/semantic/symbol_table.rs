use std::collections::HashMap;

use crate::{
    error::{CResult, CompilerError},
    lexer::Span,
    parser::ast::Type,
    symbol_interning::SymbolId,
};

use super::SemanticContext;

#[derive(Clone, Copy)]
pub struct ScopeId(usize);

struct Scope {
    pub values: HashMap<SymbolId, ValueInfo>,
    pub types: HashMap<SymbolId, TypeDefinition>,
}

#[derive(Clone)]
pub struct ValueInfo {
    // type inference fills this in if possible.
    pub known_type: Option<Type>,
    pub declaration_span: Span,
}

pub enum TypeDefinition {
    //TODO add enums etc.
    Struct { span: Span },
    Primitive,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            types: HashMap::new(),
        }
    }
}

pub struct SymbolTable {
    scopes: Vec<Scope>,
    scope_stack: Vec<ScopeId>,
}

pub const I64_SYMBOL: &'static str = "i64";
pub const F64_SYMBOL: &'static str = "f64";
pub const BOOL_SYMBOL: &'static str = "bool";
pub const STRING_SYMBOL: &'static str = "str";

impl SymbolTable {
    pub fn new() -> Self {
        let mut table = Self {
            // builtin global scope
            scopes: Vec::from_iter([Scope::new()]),
            scope_stack: Vec::from_iter([ScopeId(0)]),
        };

        table
            .define_type(SymbolId::from_str(I64_SYMBOL), TypeDefinition::Primitive)
            .unwrap();
        table
            .define_type(SymbolId::from_str(F64_SYMBOL), TypeDefinition::Primitive)
            .unwrap();
        table
            .define_type(SymbolId::from_str(BOOL_SYMBOL), TypeDefinition::Primitive)
            .unwrap();
        table
            .define_type(SymbolId::from_str(STRING_SYMBOL), TypeDefinition::Primitive)
            .unwrap();

        table
    }

    pub fn resolve_value_mut(&mut self, id: &SymbolId) -> Option<&mut ValueInfo> {
        for &ScopeId(scope_id) in self.scope_stack.iter().rev() {
            if self.scopes[scope_id].values.contains_key(id) {
                return self.scopes[scope_id].values.get_mut(id);
            }
        }
        None
    }

    pub fn resolve_type_mut(&mut self, id: &SymbolId) -> Option<&mut TypeDefinition> {
        for &ScopeId(scope_id) in self.scope_stack.iter().rev() {
            if self.scopes[scope_id].types.contains_key(id) {
                return self.scopes[scope_id].types.get_mut(id);
            }
        }
        None
    }

    pub fn resolve_value(&mut self, id: &SymbolId) -> Option<&ValueInfo> {
        for &ScopeId(scope_id) in self.scope_stack.iter().rev() {
            if self.scopes[scope_id].values.contains_key(id) {
                return self.scopes[scope_id].values.get(id);
            }
        }
        None
    }

    pub fn resolve_type(&mut self, id: &SymbolId) -> Option<&TypeDefinition> {
        for &ScopeId(scope_id) in self.scope_stack.iter().rev() {
            if self.scopes[scope_id].types.contains_key(id) {
                return self.scopes[scope_id].types.get(id);
            }
        }
        None
    }

    pub fn define_value(&mut self, id: SymbolId, initial_info: ValueInfo) -> CResult<()> {
        let current_scope_id = self.scope_stack.last().expect("no active scope");
        let scope_symbols = &mut self.scopes[current_scope_id.0].values;
        if let Some(last_decl) = scope_symbols.insert(id.clone(), initial_info.clone()) {
            // TODO make multiple symbols in same scope be OK?
            return Err(CompilerError::new("symbol already exists in this scope")
                .annotation(initial_info.declaration_span, "new declaraction")
                .annotation(last_decl.declaration_span, "existing declaration"));
        }
        Ok(())
    }

    pub fn define_type(&mut self, id: SymbolId, initial_info: TypeDefinition) -> CResult<()> {
        let current_scope_id = self.scope_stack.last().expect("no active scope");
        let scope_symbols = &mut self.scopes[current_scope_id.0].types;
        if scope_symbols.insert(id.clone(), initial_info).is_some() {
            // TODO make multiple symbols in same scope be OK?
            return Err(CompilerError::new("type already exists in this scope"));
        }
        Ok(())
    }

    pub fn validate(&self) -> CResult<()> {
        // TODO:
        // - check that all types referenced by values exist in a scope at or above self.
        // - cycle detection.
        Ok(())
    }
}

impl SemanticContext {
    pub fn scope<T>(&mut self, f: impl FnOnce(&mut Self) -> CResult<T>) -> CResult<T> {
        let new_scope_id = ScopeId(self.table.scopes.len());
        self.table.scopes.push(Scope::new());
        self.table.scope_stack.push(new_scope_id);
        let result = f(self);
        self.table.scope_stack.pop().expect("scope stack underflow");
        result
    }
}
