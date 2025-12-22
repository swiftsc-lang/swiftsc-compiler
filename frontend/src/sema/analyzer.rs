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
}

pub struct Analyzer {
    symbol_table: SymbolTable,
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

        Analyzer { symbol_table }
    }

    pub fn check_program(&mut self, program: &Program) -> Result<(), SemanticError> {
        for item in &program.items {
            match item {
                Item::Contract(contract) => self.check_contract(contract)?,
                Item::Function(func) => self.check_function(func, None)?,
                Item::Dummy => {}
            }
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
                    .map_err(SemanticError::AlreadyDefined)?;
            } else if let ContractMember::Init(func) = member {
                self.symbol_table
                    .define(func.name.clone(), Type::Path("function".into()), false)
                    .map_err(SemanticError::AlreadyDefined)?;
            }
        }

        // Second pass: Check function bodies
        for member in &contract.members {
            if let ContractMember::Function(func) = member {
                self.check_function(func, Some(&contract.name))?;
            } else if let ContractMember::Init(func) = member {
                self.check_function(func, Some(&contract.name))?;
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
                .map_err(SemanticError::AlreadyDefined)?;
        }

        // Define params
        for param in &func.params {
            self.symbol_table
                .define(
                    param.name.clone(),
                    param.ty.clone(),
                    false, // params immutable by default
                )
                .map_err(SemanticError::AlreadyDefined)?;
        }

        self.check_block(&func.body)?;

        self.symbol_table.exit_scope();
        Ok(())
    }

    fn check_block(&mut self, block: &Block) -> Result<(), SemanticError> {
        // Blocks can introduce new scope in some languages, lets say yes
        self.symbol_table.enter_scope();
        for stmt in &block.stmts {
            self.check_stmt(stmt)?;
        }
        self.symbol_table.exit_scope();
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
                let sym = self
                    .symbol_table
                    .resolve(name)
                    .ok_or(SemanticError::UndefinedSymbol(name.clone()))?;
                Ok(sym.ty.clone())
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
            Expression::Call { func, args } => {
                // Resolve func type
                self.check_expr(func)?;
                for arg in args {
                    self.check_expr(arg)?;
                }
                // Assume unknown return type for now if not tracking function signatures strictly
                // Requires Function Type in AST or Symbol definition
                Ok(Type::Path("unknown".into()))
            }
            Expression::FieldAccess { expr, field: _ } => {
                let _ = self.check_expr(expr)?;
                Ok(Type::Path("unknown_field".into()))
            }
            Expression::Index { expr, index } => {
                let _ = self.check_expr(expr)?;
                let _ = self.check_expr(index)?;
                Ok(Type::Path("unknown_element".into()))
            }
        }
    }
}
