use crate::ast::*;
use crate::sema::symbol_table::SymbolTable;
use thiserror::Error;

#[derive(Error, Debug)]
pub enum SemanticError {
    #[error("Undefined symbol: {0}")]
    UndefinedSymbol(String),
    #[error("Symbol already defined: {0}")]
    AlreadyDefined(String),
    #[error("Type mismatch: expected {0}, found {1}")]
    TypeMismatch(String, String),
    #[error("Not mutable: {0}")]
    NotMutable(String),
    #[error("Control flow outside loop: {0}")]
    ControlFlowOutsideLoop(String),
    #[error("Generic function call missing type arguments: {0}")]
    GenericCallMissingTypeArgs(String),
    #[error("Non-generic function called with type arguments: {0}")]
    NonGenericCallWithTypeArgs(String),
    #[error("Trait implementation error: {0}")]
    TraitError(String),
    #[error("Custom error: {0}")]
    Custom(String),
    #[error("Module error: {0}")]
    ModuleError(String),
}

pub struct Analyzer {
    symbol_table: SymbolTable,
    loop_depth: usize,
    structs: std::collections::HashMap<String, Struct>,
    enums: std::collections::HashMap<String, Enum>,
    traits: std::collections::HashMap<String, Trait>,
    pub contracts: std::collections::HashMap<String, Contract>,
    pub generic_functions: std::collections::HashSet<String>, // Just track names
    pub functions: std::collections::HashMap<String, Function>,
    module_resolver: Option<crate::module_resolver::ModuleResolver>,
    current_return_type: Option<Type>,
}

impl Default for Analyzer {
    fn default() -> Self {
        Self::new()
    }
}

impl Analyzer {
    pub fn new() -> Self {
        let mut symbol_table = SymbolTable::new();
        // Define global intrinsics
        symbol_table
            .define("msg".to_string(), Type::Path("Msg".into()), false)
            .unwrap();
        symbol_table
            .define("Ok".to_string(), Type::Path("Result".into()), false)
            .unwrap();
        symbol_table
            .define("Err".to_string(), Type::Path("Result".into()), false)
            .unwrap();
        symbol_table
            .define("emit".to_string(), Type::Path("function".into()), false)
            .unwrap();

        Analyzer {
            symbol_table,
            loop_depth: 0,
            structs: std::collections::HashMap::new(),
            enums: std::collections::HashMap::new(),
            traits: std::collections::HashMap::new(),
            contracts: std::collections::HashMap::new(),
            generic_functions: std::collections::HashSet::new(),
            functions: std::collections::HashMap::new(),
            module_resolver: None,
            current_return_type: None,
        }
    }

    pub fn with_root(mut self, root: std::path::PathBuf) -> Self {
        self.module_resolver = Some(crate::module_resolver::ModuleResolver::new(root));
        self
    }

    fn process_use_declaration(&mut self, use_decl: &UseDeclaration) -> Result<(), SemanticError> {
        let resolver = self.module_resolver.as_mut().ok_or_else(|| {
            SemanticError::ModuleError("Module resolver not initialized (no root path)".into())
        })?;

        // Load the module
        let module = resolver.load_module(&use_decl.path).map_err(|e| {
            SemanticError::ModuleError(format!("Failed to load module {:?}: {}", use_decl.path, e))
        })?;

        // Items and aliases to resolve
        let mut symbols_to_import = std::collections::HashMap::new();

        if let Some(items) = &use_decl.items {
            for item_name in items {
                symbols_to_import.insert(item_name.clone(), item_name.clone());
            }
        } else if let Some(alias) = &use_decl.alias {
            let original = use_decl
                .path
                .last()
                .ok_or_else(|| SemanticError::ModuleError("Empty use path".into()))?
                .clone();
            symbols_to_import.insert(original, alias.clone());
        } else {
            let name = use_decl
                .path
                .last()
                .ok_or_else(|| SemanticError::ModuleError("Empty use path".into()))?
                .clone();
            symbols_to_import.insert(name.clone(), name.clone());
        }

        // Pull in definitions
        for (original_name, alias) in symbols_to_import {
            let mut found = false;
            for item in &module.items {
                match item {
                    Item::Struct(s) if s.name == original_name => {
                        let mut imported_s = s.clone();
                        imported_s.name = alias.clone();
                        self.structs.insert(alias.clone(), imported_s);
                        found = true;
                    }
                    Item::Function(f) if f.name == original_name => {
                        let mut imported_f = f.clone();
                        imported_f.name = alias.clone();
                        self.functions.insert(alias.clone(), imported_f);
                        found = true;
                    }
                    Item::Enum(e) if e.name == original_name => {
                        let mut imported_e = e.clone();
                        imported_e.name = alias.clone();
                        self.enums.insert(alias.clone(), imported_e);
                        found = true;
                    }
                    Item::Trait(t) if t.name == original_name => {
                        let mut imported_t = t.clone();
                        imported_t.name = alias.clone();
                        self.traits.insert(alias.clone(), imported_t);
                        found = true;
                    }
                    Item::Resource(r) if r.name == original_name => {
                        let mut imported_r = r.clone();
                        imported_r.name = alias.clone();
                        self.structs.insert(alias.clone(), imported_r);
                        found = true;
                    }
                    _ => {}
                }
            }
            if !found {
                return Err(SemanticError::ModuleError(format!(
                    "Symbol {} not found in module {:?}",
                    original_name, use_decl.path
                )));
            }
        }

        Ok(())
    }

    pub fn get_linked_items(&self) -> Vec<Item> {
        let mut items = Vec::new();

        // Add all structs
        for s in self.structs.values() {
            items.push(Item::Struct(s.clone()));
        }

        // Add all enums
        for e in self.enums.values() {
            items.push(Item::Enum(e.clone()));
        }

        // Add all functions
        for f in self.functions.values() {
            items.push(Item::Function(f.clone()));
        }

        // Add all traits
        for t in self.traits.values() {
            items.push(Item::Trait(t.clone()));
        }

        // Add contracts (main program contracts)
        for c in self.contracts.values() {
            items.push(Item::Contract(c.clone()));
        }

        items
    }

    pub fn check_program(&mut self, program: &Program) -> Result<(), SemanticError> {
        // Phase 0: Resolve imports
        for item in &program.items {
            if let Item::Use(use_decl) = item {
                self.process_use_declaration(use_decl)?;
            }
        }

        // First pass: Register all structs
        // First pass: Register all structs, enums, and generic functions
        for item in &program.items {
            if let Item::Struct(s) = item {
                if self.structs.contains_key(&s.name) {
                    return Err(SemanticError::AlreadyDefined(s.name.clone()));
                }
                self.structs.insert(s.name.clone(), s.clone());
            } else if let Item::Enum(e) = item {
                if self.enums.contains_key(&e.name) {
                    return Err(SemanticError::AlreadyDefined(e.name.clone()));
                }
                self.enums.insert(e.name.clone(), e.clone());
            } else if let Item::Resource(r) = item {
                if self.structs.contains_key(&r.name) {
                    return Err(SemanticError::AlreadyDefined(r.name.clone()));
                }
                self.structs.insert(r.name.clone(), r.clone());
            } else if let Item::Trait(t) = item {
                if self.traits.contains_key(&t.name) {
                    return Err(SemanticError::AlreadyDefined(t.name.clone()));
                }
                self.traits.insert(t.name.clone(), t.clone());
            } else if let Item::Contract(c) = item {
                if self.contracts.contains_key(&c.name) {
                    return Err(SemanticError::AlreadyDefined(c.name.clone()));
                }
                self.contracts.insert(c.name.clone(), c.clone());
            } else if let Item::Function(func) = item {
                // Register all top-level functions in symbol table
                self.symbol_table
                    .define(func.name.clone(), Type::Path("function".into()), false)
                    .map_err(|_| SemanticError::AlreadyDefined(func.name.clone()))?;

                if !func.generics.is_empty() {
                    self.generic_functions.insert(func.name.clone());
                }
                self.functions.insert(func.name.clone(), func.clone());
            }
        }

        for item in &program.items {
            match item {
                Item::Contract(contract) => self.check_contract(contract)?,
                Item::Function(func) => self.check_function(func, None)?,
                Item::Impl(im) => self.check_impl(im)?,
                Item::Trait(_) => {} // Nothing to check yet without default methods
                Item::Struct(_) => {} // Already registered
                Item::Resource(_) => {} // Already registered
                Item::Enum(_) => {}
                Item::Dummy => {}
                Item::Use(_) => {} // TODO: Implement module resolution
            }
        }
        Ok(())
    }

    fn check_impl(&mut self, im: &Impl) -> Result<(), SemanticError> {
        // Verify target exists
        if !self.structs.contains_key(&im.target_name) && !self.enums.contains_key(&im.target_name)
        {
            return Err(SemanticError::UndefinedSymbol(im.target_name.clone()));
        }

        if let Some(trait_name) = &im.trait_name {
            let tr = self
                .traits
                .get(trait_name)
                .ok_or_else(|| SemanticError::UndefinedSymbol(trait_name.clone()))?;

            // Basic validation: all trait methods must be implemented
            for trait_method in &tr.methods {
                let found = im.methods.iter().any(|m| m.name == trait_method.name);
                if !found {
                    return Err(SemanticError::TraitError(format!(
                        "Method {} not implemented for trait {}",
                        trait_method.name, trait_name
                    )));
                }
            }
        }

        // Check each method in the impl
        for func in &im.methods {
            self.check_function(func, None)?;
        }

        Ok(())
    }

    fn check_contract(&mut self, contract: &Contract) -> Result<(), SemanticError> {
        self.symbol_table.enter_scope();

        // First pass: Register all function names so they can call each other
        for member in &contract.members {
            if let ContractMember::Function(func) = member {
                // Register function with placeholder type (function signature)
                self.symbol_table
                    .define(
                        func.name.clone(),
                        Type::Path("function".into()), // Placeholder for function type
                        false,
                    )
                    .map_err(|_| SemanticError::AlreadyDefined(func.name.clone()))?;
            } else if let ContractMember::Init(func) = member {
                self.symbol_table
                    .define(func.name.clone(), Type::Path("function".into()), false)
                    .map_err(|_| SemanticError::AlreadyDefined(func.name.clone()))?;
            } else if let ContractMember::Storage(fields) = member {
                for field in fields {
                    self.symbol_table
                        .define(field.name.clone(), field.ty.clone(), true)
                        .map_err(|_| SemanticError::AlreadyDefined(field.name.clone()))?;
                }
            }
        }

        // Second pass: Check function bodies
        for member in &contract.members {
            match member {
                ContractMember::Function(func) => {
                    self.check_function(func, Some(&contract.name))?
                }
                ContractMember::Init(func) => self.check_function(func, Some(&contract.name))?,
                ContractMember::Storage(_) => {} // Already registered
                ContractMember::Field(field) => {
                    self.symbol_table
                        .define(field.name.clone(), field.ty.clone(), true)
                        .map_err(|_| SemanticError::AlreadyDefined(field.name.clone()))?;
                }
            }
        }
        self.symbol_table.exit_scope();
        Ok(())
    }

    fn check_function(
        &mut self,
        func: &Function,
        contract_name: Option<&String>,
    ) -> Result<(), SemanticError> {
        self.symbol_table.enter_scope();

        // Define 'self' if in contract
        if let Some(c_name) = contract_name {
            self.symbol_table
                .define(
                    "self".to_string(),
                    Type::Path(c_name.clone()),
                    true, // self is mutable reference for now
                )
                .map_err(|_| SemanticError::AlreadyDefined("self".to_string()))?;
        }

        // Define params
        for param in &func.params {
            self.symbol_table
                .define(
                    param.name.clone(),
                    param.ty.clone(),
                    false, // params immutable by default
                )
                .map_err(|_| SemanticError::AlreadyDefined(param.name.clone()))?;
        }

        self.current_return_type = func.return_type.clone();
        self.check_block(&func.body)?;
        self.current_return_type = None;

        self.symbol_table.exit_scope();
        Ok(())
    }

    fn check_block(&mut self, block: &Block) -> Result<(), SemanticError> {
        // Blocks can introduce new scope in some languages, lets say yes
        self.symbol_table.enter_scope();
        self.check_block_no_scope(block)?;
        self.symbol_table.exit_scope();
        Ok(())
    }

    fn check_block_no_scope(&mut self, block: &Block) -> Result<(), SemanticError> {
        for stmt in &block.stmts {
            self.check_stmt(stmt)?;
        }
        Ok(())
    }

    fn check_stmt(&mut self, stmt: &Statement) -> Result<(), SemanticError> {
        match stmt {
            Statement::Let {
                name,
                destruct_names,
                ty,
                init,
                is_mut,
            } => {
                let init_ty = self.check_expr(init)?;

                // If explicit type, check match (omitted for destructuring logic simplicity in MVP)
                if let Some(decl_ty) = ty {
                    let is_unknown =
                        |t: &Type| matches!(t, Type::Path(s) if s.starts_with("unknown"));
                    if !is_unknown(decl_ty) && !is_unknown(&init_ty) && *decl_ty != init_ty {
                        return Err(SemanticError::TypeMismatch(
                            format!("{:?}", decl_ty),
                            format!("{:?}", init_ty),
                        ));
                    }
                }

                // Define primary variable (if not partial destructuring, but likely is struct name here)
                // In "let Token { amount }", "Token" is Type, not a var?
                // The parser treats "Token" as 'name'. If destruct_names is not empty, 'name' is likely the type or ignored in this specific grammar?
                // Actually SwiftSC-Lang grammar: let <Name> { ... } = ...
                // If 'name' is used as Type, we shouldn't define it as variable.

                if destruct_names.is_empty() {
                    let type_to_store = ty.clone().unwrap_or(init_ty);
                    self.symbol_table
                        .define(name.clone(), type_to_store, *is_mut)
                        .map_err(|_| SemanticError::AlreadyDefined(name.clone()))?;
                } else {
                    // Define destructured names with unknown type for now
                    for dname in destruct_names {
                        self.symbol_table
                            .define(
                                dname.clone(),
                                Type::Path("unknown_destruct".into()),
                                *is_mut,
                            )
                            .map_err(|_| SemanticError::AlreadyDefined(dname.clone()))?;
                    }
                }
            }
            Statement::Expr(expr) => {
                self.check_expr(expr)?;
            }
            Statement::Return(Some(expr)) => {
                self.check_expr(expr)?;
                // Check against function return type (requires storing context)
            }
            Statement::Return(None) => {}
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_ty = self.check_expr(condition)?;
                if cond_ty != Type::Path("bool".into()) && cond_ty != Type::Path("unknown".into()) {
                    return Err(SemanticError::TypeMismatch(
                        "bool".into(),
                        format!("{:?}", cond_ty),
                    ));
                }
                self.check_block(then_branch)?;
                if let Some(eb) = else_branch {
                    self.check_block(eb)?;
                }
            }
            Statement::While { condition, body } => {
                let cond_ty = self.check_expr(condition)?;
                if cond_ty != Type::Path("bool".into()) && cond_ty != Type::Path("unknown".into()) {
                    return Err(SemanticError::TypeMismatch(
                        "bool".into(),
                        format!("{:?}", cond_ty),
                    ));
                }
                self.loop_depth += 1;
                self.check_block(body)?;
                self.loop_depth -= 1;
            }
            Statement::For {
                var_name,
                start,
                end,
                body,
            } => {
                let start_ty = self.check_expr(start)?;
                let end_ty = self.check_expr(end)?;

                // For MVP, assume u64 for ranges
                if start_ty != Type::Path("u64".into()) && start_ty != Type::Path("unknown".into())
                {
                    return Err(SemanticError::TypeMismatch(
                        "u64".into(),
                        format!("{:?}", start_ty),
                    ));
                }
                if end_ty != Type::Path("u64".into()) && end_ty != Type::Path("unknown".into()) {
                    return Err(SemanticError::TypeMismatch(
                        "u64".into(),
                        format!("{:?}", end_ty),
                    ));
                }

                self.symbol_table.enter_scope();
                self.symbol_table
                    .define(var_name.clone(), Type::Path("u64".into()), false)
                    .map_err(|_| SemanticError::AlreadyDefined(var_name.clone()))?;

                self.loop_depth += 1;
                self.check_block_no_scope(body)?; // check_block enters scope, but we already entered one for the loop var
                self.loop_depth -= 1;

                self.symbol_table.exit_scope();
            }
            Statement::Break => {
                if self.loop_depth == 0 {
                    return Err(SemanticError::ControlFlowOutsideLoop("break".into()));
                }
            }
            Statement::Continue => {
                if self.loop_depth == 0 {
                    return Err(SemanticError::ControlFlowOutsideLoop("continue".into()));
                }
            }
        }
        Ok(())
    }

    fn check_expr(&mut self, expr: &Expression) -> Result<Type, SemanticError> {
        match expr {
            Expression::Literal(lit) => match lit {
                Literal::Int(_) => Ok(Type::Path("u64".into())), // Default integer
                Literal::String(_) => Ok(Type::Path("String".into())),
                Literal::Bool(_) => Ok(Type::Path("bool".into())),
                Literal::Unit => Ok(Type::Path("unit".into())),
            },
            Expression::Identifier(name) => {
                if let Some(sym) = self.symbol_table.resolve(name) {
                    Ok(sym.ty.clone())
                } else if self.structs.contains_key(name)
                    || self.enums.contains_key(name)
                    || self.functions.contains_key(name)
                {
                    Ok(Type::Path(name.clone()))
                } else {
                    Err(SemanticError::UndefinedSymbol(name.clone()))
                }
            }

            Expression::Binary { left, op, right } => {
                let l_ty = self.check_expr(left)?;
                let r_ty = self.check_expr(right)?;
                // Allow unknown types to pass
                let is_unknown = |t: &Type| matches!(t, Type::Path(s) if s.starts_with("unknown"));
                if !is_unknown(&l_ty) && !is_unknown(&r_ty) && l_ty != r_ty {
                    // Hack for MVP: allow u64 and integer literals to mix if needed, or stricter?
                    // For now strictly equal unless unknown
                    return Err(SemanticError::TypeMismatch(
                        format!("{:?}", l_ty),
                        format!("{:?}", r_ty),
                    ));
                }
                // Determine result type
                // For comparison, result is bool
                match op {
                    BinaryOp::Eq
                    | BinaryOp::Ne
                    | BinaryOp::Lt
                    | BinaryOp::Gt
                    | BinaryOp::Le
                    | BinaryOp::Ge => Ok(Type::Path("bool".into())),
                    _ => Ok(l_ty), // Add/Sub etc return same type
                }
            }
            Expression::Call {
                func,
                args,
                type_args,
            } => {
                // Specialized logic for msg.sender() as a function call
                if let Expression::FieldAccess { expr: obj, field } = &**func {
                    if let Expression::Identifier(obj_name) = &**obj {
                        if obj_name == "msg" {
                            match field.as_str() {
                                "sender" => return Ok(Type::Path("Address".into())),
                                "value" | "data" => return Ok(Type::Path("u64".into())),
                                _ => {}
                            }
                        }
                    }

                    // Specialized logic for String methods to return correct types
                    // We need to check the type of obj. We'll supress duplicate errors if check_expr(obj) fails here (it will fail later too)
                    if let Ok(Type::Path(ty_name)) = self.check_expr(obj) {
                        if ty_name == "String" {
                            match field.as_str() {
                                "len" | "as_bytes" | "get_rc" => {
                                    return Ok(Type::Path("u64".into()))
                                }
                                "is_empty" => return Ok(Type::Path("bool".into())),
                                _ => {}
                            }
                        }
                    }
                }

                // Check function call
                let func_name = match &**func {
                    Expression::Identifier(name) => name.clone(),
                    _ => "".to_string(), // Not a simple identifier
                };

                if !func_name.is_empty() {
                    let is_generic = self.generic_functions.contains(&func_name);

                    // If generic function and no type args provided, try to infer from arguments
                    let inferred_type_args = if is_generic && type_args.is_empty() {
                        // Simple inference: use type of first argument as T
                        // For MVP, assume single generic parameter
                        if let Some(first_arg) = args.first() {
                            let arg_ty = self.check_expr(first_arg)?;
                            vec![arg_ty]
                        } else {
                            vec![]
                        }
                    } else {
                        type_args.clone()
                    };

                    if is_generic && inferred_type_args.is_empty() {
                        return Err(SemanticError::GenericCallMissingTypeArgs(func_name));
                    }
                    if !is_generic && !type_args.is_empty() {
                        return Err(SemanticError::NonGenericCallWithTypeArgs(func_name));
                    }
                }

                // Resolve func type
                self.check_expr(func)?;
                for arg in args {
                    self.check_expr(arg)?;
                }

                // better return type inference
                if let Expression::Identifier(name) = &**func {
                    if let Some(f) = self.functions.get(name) {
                        return Ok(f.return_type.clone().unwrap_or(Type::Path("unit".into())));
                    }
                } else if let Expression::FieldAccess { expr: obj, field } = &**func {
                    if let Ok(obj_ty) = self.check_expr(obj) {
                        let ty_name = match obj_ty {
                            Type::Path(n) => n,
                            Type::Generic(n, _) => n,
                            _ => "".to_string(),
                        };
                        if let Some(c_def) = self.contracts.get(&ty_name) {
                            for member in &c_def.members {
                                if let ContractMember::Function(f) = member {
                                    if f.name == *field {
                                        return Ok(f
                                            .return_type
                                            .clone()
                                            .unwrap_or(Type::Path("unit".into())));
                                    }
                                }
                            }
                        }
                    }
                }

                Ok(Type::Path("unknown".into()))
            }
            Expression::Index { expr, index } => {
                let obj_type = self.check_expr(expr)?;
                let index_type = self.check_expr(index)?;

                match obj_type {
                    Type::Map(key_ty, val_ty) => {
                        if *key_ty != index_type {
                            return Err(SemanticError::TypeMismatch(
                                format!("{:?}", key_ty),
                                format!("{:?}", index_type),
                            ));
                        }
                        Ok((*val_ty).clone())
                    }
                    _ => Err(SemanticError::Custom(format!(
                        "Indexing on non-map type: {:?}",
                        obj_type
                    ))),
                }
            }
            Expression::StructInit {
                name,
                fields,
                type_args,
            } => {
                let s_def = self
                    .structs
                    .get(name)
                    .cloned() // Clone to avoid borrowing self
                    .ok_or_else(|| SemanticError::UndefinedSymbol(name.clone()))?;

                // Check type args count
                if s_def.generics.len() != type_args.len() {
                    return Err(SemanticError::Custom(format!(
                        "Struct {} expects {} type arguments, found {}",
                        name,
                        s_def.generics.len(),
                        type_args.len()
                    )));
                }

                for (f_name, f_expr) in fields {
                    let field_def = s_def
                        .fields
                        .iter()
                        .find(|f| f.name == *f_name)
                        .ok_or_else(|| SemanticError::UndefinedSymbol(f_name.clone()))?;
                    let val_ty = self.check_expr(f_expr)?;

                    // NOTE: In a full compiler we would substitute generics here.
                    // For MVP, we allow comparison if we are not doing deep type checking of generic fields.
                    if field_def.ty != val_ty
                        && !matches!(val_ty, Type::Path(ref s) if s.starts_with("unknown"))
                        && !matches!(field_def.ty, Type::Path(ref s) if s_def.generics.contains(s))
                    {
                        return Err(SemanticError::TypeMismatch(
                            format!("{:?}", field_def.ty),
                            format!("{:?}", val_ty),
                        ));
                    }
                }
                if type_args.is_empty() {
                    Ok(Type::Path(name.clone()))
                } else {
                    Ok(Type::Generic(name.clone(), type_args.clone()))
                }
            }
            Expression::Try(inner) => {
                let inner_ty = self.check_expr(inner)?;
                match inner_ty {
                    Type::Generic(name, args) if name == "Result" && args.len() == 2 => {
                        let ok_ty = args[0].clone();
                        let err_ty = args[1].clone();

                        // Check if current function returns a Result with matching error type
                        if let Some(Type::Generic(ret_name, ret_args)) = &self.current_return_type {
                            if ret_name == "Result" && ret_args.len() == 2 {
                                let ret_err_ty = &ret_args[1];
                                if &err_ty != ret_err_ty {
                                    return Err(SemanticError::TypeMismatch(
                                        format!("{:?}", ret_err_ty),
                                        format!("{:?}", err_ty),
                                    ));
                                }
                                Ok(ok_ty)
                            } else {
                                Err(SemanticError::Custom(
                                    "? operator can only be used in functions returning Result"
                                        .into(),
                                ))
                            }
                        } else {
                            Err(SemanticError::Custom(
                                "? operator can only be used in functions returning Result".into(),
                            ))
                        }
                    }
                    _ => Err(SemanticError::Custom(
                        "? operator can only be used on Result types".into(),
                    )),
                }
            }
            Expression::GenericInst { target, type_args } => {
                let base_ty = self.check_expr(target)?;
                if let Type::Path(name) = base_ty {
                    Ok(Type::Generic(name, type_args.clone()))
                } else {
                    Ok(base_ty)
                }
            }
            Expression::FieldAccess { expr, field } => {
                let obj_type = self.check_expr(expr)?;
                match obj_type {
                    Type::Path(struct_name) | Type::Generic(struct_name, _) => {
                        if let Some(s_def) = self.structs.get(&struct_name) {
                            if let Some(f_def) = s_def.fields.iter().find(|f| f.name == *field) {
                                return Ok(f_def.ty.clone());
                            } else {
                                // Assume method call
                                return Ok(Type::Path("function".into()));
                            }
                        }

                        // Check if it's a contract
                        if let Some(c_def) = self.contracts.get(&struct_name) {
                            for member in &c_def.members {
                                if let ContractMember::Storage(fields) = member {
                                    if let Some(f) = fields.iter().find(|f| f.name == *field) {
                                        return Ok(f.ty.clone());
                                    }
                                }
                            }
                            // Contract method call?
                            return Ok(Type::Path("function".into()));
                        }

                        // Fallback for msg.sender/value/data
                        if struct_name == "Msg" {
                            return Ok(Type::Path("function".into()));
                        }

                        // Handle built-in String methods
                        if struct_name == "String" {
                            match field.as_str() {
                                "len" | "is_empty" | "as_bytes" | "get_rc" => {
                                    return Ok(Type::Path("function".into()))
                                }
                                _ => return Err(SemanticError::UndefinedSymbol(field.clone())),
                            }
                        }

                        Err(SemanticError::UndefinedSymbol(field.clone()))
                    }

                    _ => Err(SemanticError::Custom(format!(
                        "Field access on non-struct type: {:?}",
                        obj_type
                    ))),
                }
            }
            Expression::EnumVariant {
                enum_name,
                variant_name,
            } => {
                if let Some(e_def) = self.enums.get(enum_name) {
                    if !e_def.variants.contains(variant_name) {
                        return Err(SemanticError::UndefinedSymbol(format!(
                            "{}::{}",
                            enum_name, variant_name
                        )));
                    }
                    Ok(Type::Path(enum_name.clone()))
                } else if self.structs.contains_key(enum_name) {
                    // Static method call: Point::new
                    Ok(Type::Path("function".into()))
                } else {
                    Err(SemanticError::UndefinedSymbol(enum_name.clone()))
                }
            }
            Expression::Match { value, arms } => {
                let val_ty = self.check_expr(value)?;

                // Ensure value type is an enum (or simple type we can match on)
                // For MVP, only support matching on Enums
                if let Type::Path(ref name) = val_ty {
                    if !self.enums.contains_key(name) {
                        // could be matching on integers eventually
                    }
                }

                // Check arms
                let mut ret_ty = Type::Path("unknown".into());
                for (i, arm) in arms.iter().enumerate() {
                    // Check pattern validity
                    match &arm.pattern {
                        Pattern::EnumVariant {
                            enum_name,
                            variant_name,
                        } => {
                            // Ensure matches value type
                            if let Type::Path(ref v_name) = val_ty {
                                if v_name != enum_name {
                                    return Err(SemanticError::TypeMismatch(
                                        v_name.clone(),
                                        enum_name.clone(),
                                    ));
                                }
                            }
                            // Ensure variant exists
                            let e_def = self
                                .enums
                                .get(enum_name)
                                .ok_or(SemanticError::UndefinedSymbol(enum_name.clone()))?;
                            if !e_def.variants.contains(variant_name) {
                                return Err(SemanticError::UndefinedSymbol(variant_name.clone()));
                            }
                        }
                        Pattern::Wildcard => {}
                        _ => {}
                    }

                    // Check body type
                    let arm_ty = self.check_expr(&arm.body)?;
                    if i == 0 {
                        ret_ty = arm_ty;
                    } else if ret_ty != arm_ty {
                        return Err(SemanticError::TypeMismatch(
                            format!("{:?}", ret_ty),
                            format!("{:?}", arm_ty),
                        ));
                    }
                }
                Ok(ret_ty)
            }
            Expression::Closure { params, body } => {
                // Closures create a new scope
                self.symbol_table.enter_scope();
                for param in params {
                    self.symbol_table
                        .define(param.name.clone(), param.ty.clone(), false)
                        .map_err(|_| SemanticError::AlreadyDefined(param.name.clone()))?;
                }
                // Check body
                self.check_block(body)?;
                self.symbol_table.exit_scope();

                Ok(Type::Path("closure".into()))
            }
        }
    }
}
