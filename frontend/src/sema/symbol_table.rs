use crate::ast::Type;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Symbol {
    pub name: String,
    pub ty: Type,
    pub is_mut: bool,
}

pub struct SymbolTable {
    scopes: Vec<HashMap<String, Symbol>>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            scopes: vec![HashMap::new()], // Start with global/root scope
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        } else {
            panic!("Cannot exit global scope");
        }
    }

    pub fn define(&mut self, name: String, ty: Type, is_mut: bool) -> Result<(), String> {
        let current_scope = self.scopes.last_mut().unwrap();
        if current_scope.contains_key(&name) {
            return Err(format!("Symbol '{}' already defined in this scope", name));
        }
        current_scope.insert(name.clone(), Symbol { name, ty, is_mut });
        Ok(())
    }

    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }
}
