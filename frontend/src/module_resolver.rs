use crate::ast::Program;
use crate::parser::Parser;
use crate::tokenize;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum ModuleError {
    #[error("Module not found: {0:?}")]
    NotFound(Vec<String>),

    #[error("IO error reading {0}: {1}")]
    IoError(PathBuf, #[source] std::io::Error),

    #[error("Parse error in {0}: {1}")]
    ParseError(PathBuf, #[source] crate::parser::ParseError),
}

pub struct ModuleResolver {
    root_path: PathBuf,
    module_cache: HashMap<PathBuf, Program>,
}

impl ModuleResolver {
    pub fn new(root_path: PathBuf) -> Self {
        Self {
            root_path,
            module_cache: HashMap::new(),
        }
    }

    /// Resolve module path to file path
    /// e.g., ["std", "collections"] -> "std/collections.stc"
    pub fn resolve_path(&self, module_path: &[String]) -> Result<PathBuf, ModuleError> {
        if module_path.is_empty() {
            return Err(ModuleError::NotFound(vec![]));
        }

        // Try the full path first: a/b/c.stc
        let mut full_path = self.root_path.clone();
        for segment in module_path {
            full_path.push(segment);
        }
        full_path.set_extension("stc");

        if full_path.exists() && full_path.is_file() {
            return Ok(full_path);
        }

        // Try prefixes: if path is ["a", "b", "c"], try "a/b.stc", then "a.stc"
        for i in (1..module_path.len()).rev() {
            let mut prefix_path = self.root_path.clone();
            for segment in &module_path[..i] {
                prefix_path.push(segment);
            }
            prefix_path.set_extension("stc");

            if prefix_path.exists() && prefix_path.is_file() {
                return Ok(prefix_path);
            }
        }

        Err(ModuleError::NotFound(module_path.to_vec()))
    }

    /// Load and parse a module
    pub fn load_module(&mut self, module_path: &[String]) -> Result<&Program, ModuleError> {
        // Resolve file path
        let file_path = self.resolve_path(module_path)?;

        // Check cache first
        if self.module_cache.contains_key(&file_path) {
            return Ok(self.module_cache.get(&file_path).unwrap());
        }

        // Read source
        let source = fs::read_to_string(&file_path)
            .map_err(|e| ModuleError::IoError(file_path.clone(), e))?;

        // Parse using the tokenize helper
        let tokens = tokenize(&source);
        let mut parser = Parser::new(tokens.into_iter());
        let program = parser
            .parse_program()
            .map_err(|e| ModuleError::ParseError(file_path.clone(), e))?;

        // Cache and return
        self.module_cache.insert(file_path.clone(), program);
        Ok(self.module_cache.get(&file_path).unwrap())
    }

    /// Get all exported symbols from a module
    pub fn get_exports(&self, file_path: &PathBuf) -> Option<Vec<String>> {
        let program = self.module_cache.get(file_path)?;
        let mut exports = Vec::new();

        for item in &program.items {
            match item {
                crate::ast::Item::Function(f) if f.is_pub => {
                    exports.push(f.name.clone());
                }
                crate::ast::Item::Struct(s) => {
                    exports.push(s.name.clone());
                }
                crate::ast::Item::Enum(e) => {
                    exports.push(e.name.clone());
                }
                crate::ast::Item::Trait(t) => {
                    exports.push(t.name.clone());
                }
                _ => {}
            }
        }

        Some(exports)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_resolve_simple_module() {
        let temp_dir = TempDir::new().unwrap();
        let root = temp_dir.path();

        // Create test module
        let module_path = root.join("test.stc");
        fs::write(&module_path, "struct Point { x: u64, y: u64 }").unwrap();

        let resolver = ModuleResolver::new(root.to_path_buf());
        let resolved = resolver.resolve_path(&["test".to_string()]).unwrap();

        assert_eq!(resolved, module_path);
    }

    #[test]
    fn test_module_not_found() {
        let temp_dir = TempDir::new().unwrap();
        let resolver = ModuleResolver::new(temp_dir.path().to_path_buf());

        let result = resolver.resolve_path(&["nonexistent".to_string()]);
        assert!(matches!(result, Err(ModuleError::NotFound(_))));
    }
}
