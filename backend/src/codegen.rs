use crate::wasm::WasmModule;
use anyhow::Result;
use swiftsc_frontend::ast::*;
use wasm_encoder::{ConstExpr, Function as WasmFunction, GlobalType, Instruction, ValType};

pub struct CodeGenerator {
    module: WasmModule,
    function_map: std::collections::HashMap<String, u32>,
    control_stack: Vec<ControlFrame>,
    struct_layouts: std::collections::HashMap<String, Vec<FieldLayout>>,
    struct_sizes: std::collections::HashMap<String, u32>,
    enum_variants: std::collections::HashMap<String, std::collections::HashMap<String, u64>>,

    // Generics Support
    generic_functions: std::collections::HashMap<String, Function>,
    monomorphized_instances: std::collections::HashMap<(String, Vec<Type>), String>, // (name, types) -> mangled_name
    generic_context: std::collections::HashMap<String, Type>, // Current context for compiling generic function

    malloc_func: u32, // Function index for malloc
    closure_counter: u32,
    storage_keys: std::collections::HashMap<String, i64>,
    scratch_base: u32,
    scratch_depth: u32,
    function_returns: std::collections::HashMap<String, Option<Type>>,

    // NEW
    generic_impls: std::collections::HashMap<String, Impl>,
    pending_instantiations: std::collections::HashSet<(String, Vec<Type>)>,
    storage_types: std::collections::HashMap<String, Type>,

    // ARC
    retain_func: u32,
    release_func: u32,
    scope_stack: Vec<Vec<u32>>,
    current_contract: Option<String>,
    gas_counter_global: u32,
}

struct FieldLayout {
    name: String,
    offset: u32,
}

#[derive(Clone, Copy)]
enum ControlFrame {
    Block { scope_depth: usize },
    Loop { scope_depth: usize },
}

impl Default for CodeGenerator {
    fn default() -> Self {
        Self::new().expect("Failed to create CodeGenerator")
    }
}

impl CodeGenerator {
    pub fn new() -> Result<Self> {
        let mut module = WasmModule::new();

        // host functions
        let sr_ty = module.add_type(vec![ValType::I64], vec![ValType::I64]);
        let storage_read_idx = module.import_function("env", "storage_read", sr_ty);

        let sw_ty = module.add_type(vec![ValType::I64, ValType::I64], vec![]);
        let storage_write_idx = module.import_function("env", "storage_write", sw_ty);

        let gc_ty = module.add_type(vec![], vec![ValType::I64]);
        let get_caller_idx = module.import_function("env", "get_caller", gc_ty);

        let gv_ty = module.add_type(vec![], vec![ValType::I64]);
        let get_value_idx = module.import_function("env", "get_value", gv_ty);

        let gd_ty = module.add_type(vec![], vec![ValType::I64]);
        let get_data_idx = module.import_function("env", "get_data", gd_ty);

        let ee_ty = module.add_type(vec![ValType::I64, ValType::I64], vec![]);
        let emit_event_idx = module.import_function("env", "emit_event", ee_ty);

        let h64_ty = module.add_type(vec![ValType::I64, ValType::I64], vec![ValType::I64]);
        let hash_i64_idx = module.import_function("env", "hash_i64", h64_ty);

        let mut function_returns = std::collections::HashMap::new();
        function_returns.insert("storage_read".to_string(), Some(Type::Path("u64".into())));
        function_returns.insert("storage_write".to_string(), None);
        function_returns.insert("get_caller".to_string(), Some(Type::Path("u64".into())));
        function_returns.insert("get_value".to_string(), Some(Type::Path("u64".into())));
        function_returns.insert("get_data".to_string(), Some(Type::Path("u64".into())));
        function_returns.insert("emit_event".to_string(), None);
        function_returns.insert("hash_i64".to_string(), Some(Type::Path("u64".into())));
        function_returns.insert("malloc".to_string(), Some(Type::Path("u64".into())));
        function_returns.insert("free".to_string(), None);
        function_returns.insert("mem_alloc".to_string(), Some(Type::Path("u64".into())));
        function_returns.insert("mem_free".to_string(), None);

        // Add memory (1 page = 64KB)
        module.add_memory(1, Some(10));

        // Add heap_ptr global (mutable, initialized to 1024)
        let heap_init = ConstExpr::i64_const(1024);
        let heap_ptr_global = module.add_global(
            GlobalType {
                val_type: ValType::I64,
                mutable: true,
            },
            &heap_init,
        );
        module.export_global("heap_ptr", heap_ptr_global);

        // Add gas_counter global (mutable, initialized to 0)
        let gas_init = ConstExpr::i64_const(0);
        let gas_counter_global = module.add_global(
            GlobalType {
                val_type: ValType::I64,
                mutable: true,
            },
            &gas_init,
        );
        module.export_global("gas_counter", gas_counter_global);

        // Create malloc function
        let malloc_func = Self::create_malloc_function(&mut module, heap_ptr_global)?;

        let free_func = Self::create_free_function(&mut module)?;
        let retain_func = Self::create_retain_function(&mut module)?;
        let release_func = Self::create_release_function(&mut module, free_func)?;

        let mut function_map = std::collections::HashMap::new();
        // Register host functions in the function map
        function_map.insert("get_caller".to_string(), get_caller_idx);
        function_map.insert("get_value".to_string(), get_value_idx);
        function_map.insert("get_data".to_string(), get_data_idx);
        function_map.insert("storage_read".to_string(), storage_read_idx);
        function_map.insert("storage_write".to_string(), storage_write_idx);
        function_map.insert("emit_event".to_string(), emit_event_idx);
        function_map.insert("hash_i64".to_string(), hash_i64_idx);
        function_map.insert("malloc".to_string(), malloc_func);
        function_map.insert("free".to_string(), free_func);
        // User-facing aliases to avoid 'duplicate export' if user defines dummy
        function_map.insert("mem_alloc".to_string(), malloc_func);
        function_map.insert("mem_free".to_string(), free_func);

        Ok(CodeGenerator {
            module,
            function_map,
            control_stack: Vec::new(),
            struct_layouts: std::collections::HashMap::new(),
            struct_sizes: std::collections::HashMap::new(),
            enum_variants: std::collections::HashMap::new(),
            generic_functions: std::collections::HashMap::new(),
            monomorphized_instances: std::collections::HashMap::new(),
            generic_context: std::collections::HashMap::new(),
            malloc_func,
            closure_counter: 0,
            storage_keys: std::collections::HashMap::new(),
            storage_types: std::collections::HashMap::new(),
            scratch_base: 0,
            scratch_depth: 0,
            function_returns,
            generic_impls: std::collections::HashMap::new(),
            pending_instantiations: std::collections::HashSet::new(),

            retain_func,
            release_func,
            scope_stack: Vec::new(),
            current_contract: None,
            gas_counter_global,
        })
    }

    fn create_malloc_function(module: &mut WasmModule, heap_ptr_global: u32) -> Result<u32> {
        // malloc(size: i64) -> i64
        let malloc_type = module.add_type(vec![ValType::I64], vec![ValType::I64]);
        let malloc_idx = module.add_function(malloc_type);

        // Function body:
        // (local $ptr i64)
        // global.get $heap_ptr
        // local.set $ptr
        // local.get $ptr
        // local.get $size (param 0)
        // i64.add
        // global.set $heap_ptr
        // local.get $ptr
        let mut body = WasmFunction::new(vec![(1, ValType::I64)]); // 1 local for $ptr

        body.instruction(&Instruction::GlobalGet(heap_ptr_global));
        body.instruction(&Instruction::LocalSet(1)); // local 1 is $ptr (param 0 is $size)
        body.instruction(&Instruction::LocalGet(1));
        body.instruction(&Instruction::LocalGet(0)); // param $size
        body.instruction(&Instruction::I64Add);
        body.instruction(&Instruction::GlobalSet(heap_ptr_global));
        body.instruction(&Instruction::LocalGet(1));
        body.instruction(&Instruction::End);

        module.add_code(body);
        module.export_function("malloc", malloc_idx);

        Ok(malloc_idx)
    }

    fn create_free_function(module: &mut WasmModule) -> Result<u32> {
        let free_type = module.add_type(vec![ValType::I64], vec![]);
        let free_idx = module.add_function(free_type);
        let mut body = WasmFunction::new(vec![]);
        body.instruction(&Instruction::End); // No-op
        module.add_code(body);
        module.export_function("free", free_idx);
        Ok(free_idx)
    }

    fn create_retain_function(module: &mut WasmModule) -> Result<u32> {
        let type_idx = module.add_type(vec![ValType::I64], vec![]);
        let func_idx = module.add_function(type_idx);
        let mut body = WasmFunction::new(vec![(1, ValType::I64)]); // Local $rc

        // rc = load(ptr + 24)
        body.instruction(&Instruction::LocalGet(0));
        body.instruction(&Instruction::I32WrapI64);
        body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }));

        // rc += 1
        body.instruction(&Instruction::I64Const(1));
        body.instruction(&Instruction::I64Add);
        body.instruction(&Instruction::LocalSet(1));

        // store(ptr + 24, rc)
        body.instruction(&Instruction::LocalGet(0));
        body.instruction(&Instruction::I32WrapI64);
        body.instruction(&Instruction::LocalGet(1)); // rc
        body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }));

        body.instruction(&Instruction::End);
        module.add_code(body);
        Ok(func_idx)
    }

    fn create_release_function(module: &mut WasmModule, free_idx: u32) -> Result<u32> {
        let type_idx = module.add_type(vec![ValType::I64], vec![]);
        let func_idx = module.add_function(type_idx);
        let mut body = WasmFunction::new(vec![(1, ValType::I64)]); // Local $rc

        // rc = load(ptr + 24)
        body.instruction(&Instruction::LocalGet(0));
        body.instruction(&Instruction::I32WrapI64);
        body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }));

        // rc -= 1
        body.instruction(&Instruction::I64Const(1));
        body.instruction(&Instruction::I64Sub);
        body.instruction(&Instruction::LocalTee(1));

        // store(ptr + 24, rc)
        body.instruction(&Instruction::Drop); // Drop rc from stack
        body.instruction(&Instruction::LocalGet(0));
        body.instruction(&Instruction::I32WrapI64);
        body.instruction(&Instruction::LocalGet(1));
        body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
            offset: 24,
            align: 3,
            memory_index: 0,
        }));

        // if rc == 0
        body.instruction(&Instruction::LocalGet(1));
        body.instruction(&Instruction::I64Eqz);
        body.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));

        // Free data (offset 0)
        body.instruction(&Instruction::LocalGet(0));
        body.instruction(&Instruction::I32WrapI64);
        body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
            offset: 0,
            align: 3,
            memory_index: 0,
        }));
        body.instruction(&Instruction::Call(free_idx));

        // Free struct
        body.instruction(&Instruction::LocalGet(0));
        body.instruction(&Instruction::Call(free_idx));

        body.instruction(&Instruction::End); // End If

        body.instruction(&Instruction::End);
        module.add_code(body);
        Ok(func_idx)
    }

    fn enter_scope(&mut self) {
        self.scope_stack.push(Vec::new());
    }

    fn exit_scope(
        &mut self,
        body: &mut WasmFunction,
        local_types: &std::collections::HashMap<u32, Type>,
    ) {
        if let Some(locals) = self.scope_stack.pop() {
            for &idx in locals.iter().rev() {
                if local_types
                    .get(&idx)
                    .is_some_and(|ty| ty == &Type::Path("String".into()))
                {
                    body.instruction(&Instruction::LocalGet(idx));
                    body.instruction(&Instruction::Call(self.release_func));
                }
            }
        }
    }

    fn register_local(&mut self, idx: u32) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.push(idx);
        }
    }

    fn emit_cleanup_until(
        &self,
        target_depth: usize,
        body: &mut WasmFunction,
        local_types: &std::collections::HashMap<u32, Type>,
    ) {
        let current_depth = self.scope_stack.len();
        for i in (target_depth..current_depth).rev() {
            if let Some(scope) = self.scope_stack.get(i) {
                for &idx in scope.iter().rev() {
                    if local_types
                        .get(&idx)
                        .is_some_and(|ty| ty == &Type::Path("String".into()))
                    {
                        body.instruction(&Instruction::LocalGet(idx));
                        body.instruction(&Instruction::Call(self.release_func));
                    }
                }
            }
        }
    }

    fn emit_cleanup_for_all_scopes(
        &self,
        body: &mut WasmFunction,
        local_types: &std::collections::HashMap<u32, Type>,
    ) {
        for scope in self.scope_stack.iter().rev() {
            for &idx in scope.iter().rev() {
                if local_types
                    .get(&idx)
                    .is_some_and(|ty| ty == &Type::Path("String".into()))
                {
                    body.instruction(&Instruction::LocalGet(idx));
                    body.instruction(&Instruction::Call(self.release_func));
                }
            }
        }
    }

    pub fn compile_program(mut self, program: &Program) -> Result<Vec<u8>> {
        // Pre-scan all functions to know their return types
        for item in &program.items {
            match item {
                Item::Function(f) => {
                    self.function_returns
                        .insert(f.name.clone(), f.return_type.clone());
                }
                Item::Contract(c) => {
                    for member in &c.members {
                        match member {
                            ContractMember::Function(f) | ContractMember::Init(f) => {
                                self.function_returns
                                    .insert(f.name.clone(), f.return_type.clone());
                                let mangled = format!("{}__impl__{}", c.name, f.name);
                                self.function_returns.insert(mangled, f.return_type.clone());
                            }
                            _ => {}
                        }
                    }
                }
                Item::Impl(im) => {
                    for func in &im.methods {
                        self.function_returns
                            .insert(func.name.clone(), func.return_type.clone());
                        let mangled = format!("{}__impl__{}", im.target_name, func.name);
                        self.function_returns
                            .insert(mangled, func.return_type.clone());
                    }
                }
                _ => {}
            }
        }
        // First pass: Calculate struct layouts
        for item in &program.items {
            if let Item::Struct(s) = item {
                let mut offset = 0;
                let mut layouts = Vec::new();
                for field in &s.fields {
                    // All fields are 8 bytes (i64) for MVP
                    let size = 8;
                    layouts.push(FieldLayout {
                        name: field.name.clone(),
                        offset,
                    });
                    offset += size;
                }
                self.struct_sizes.insert(s.name.clone(), offset);
                self.struct_layouts.insert(s.name.clone(), layouts);
            } else if let Item::Resource(r) = item {
                let mut layouts = Vec::new();
                let mut offset = 0;
                for field in &r.fields {
                    let size = 8; // i64
                    layouts.push(FieldLayout {
                        name: field.name.clone(),
                        offset,
                    });
                    offset += size;
                }
                self.struct_sizes.insert(r.name.clone(), offset);
                self.struct_layouts.insert(r.name.clone(), layouts);
            } else if let Item::Enum(e) = item {
                let mut variant_map = std::collections::HashMap::new();
                for (i, v) in e.variants.iter().enumerate() {
                    variant_map.insert(v.clone(), i as u64);
                }
                self.enum_variants.insert(e.name.clone(), variant_map);
            }
        }

        for item in &program.items {
            match item {
                Item::Contract(c) => {
                    self.current_contract = Some(c.name.clone());
                    self.compile_contract(c)?;
                    self.current_contract = None;
                }
                Item::Function(f) => {
                    if !f.generics.is_empty() {
                        self.generic_functions.insert(f.name.clone(), f.clone());
                    } else {
                        self.compile_function(f)?;
                    }
                }
                Item::Struct(_) => {}
                Item::Resource(_) => {}
                Item::Enum(_) => {}
                Item::Trait(_) => {}
                Item::Impl(im) => {
                    if !im.generics.is_empty() {
                        self.generic_impls
                            .insert(im.target_name.clone(), im.clone());
                    } else {
                        self.compile_impl(im)?;
                    }
                }
                Item::Dummy => {}
                Item::Use(_) => {} // Handled in semantic analysis
            }
        }

        // Process pending instantiations
        while !self.pending_instantiations.is_empty() {
            let pending: Vec<_> = self.pending_instantiations.iter().cloned().collect();
            self.pending_instantiations.clear();
            for (target_name, type_args) in pending {
                self.instantiate_impl(&target_name, &type_args)?;
            }
        }

        Ok(self.module.finish())
    }

    fn compile_contract(&mut self, contract: &Contract) -> Result<()> {
        let mut next_key = 0;
        // First pass: assign storage keys
        for member in &contract.members {
            if let ContractMember::Storage(fields) = member {
                for field in fields {
                    self.storage_keys.insert(field.name.clone(), next_key);
                    self.storage_types
                        .insert(field.name.clone(), field.ty.clone());

                    if field.ty == Type::Path("String".into()) {
                        next_key += 3;
                    } else {
                        next_key += 1;
                    }
                }
            }
        }

        // First pass: Register all function names for forward references
        for member in &contract.members {
            if let ContractMember::Function(f) = member {
                let mangled_name = format!("{}__impl__{}", contract.name, f.name);
                self.register_function(f, &mangled_name);

                if f.is_pub {
                    self.register_function(f, &f.name);
                }
            } else if let ContractMember::Init(f) = member {
                self.register_function(f, &f.name);
            }
        }

        // Second pass: Compile bodies
        for member in &contract.members {
            if let ContractMember::Function(f) = member {
                self.compile_function_body(f, true)?;

                if f.is_pub {
                    self.compile_function_body(f, false)?;
                }
            } else if let ContractMember::Init(f) = member {
                self.compile_function_body(f, false)?;
            }
        }
        Ok(())
    }

    fn compile_function(&mut self, func: &Function) -> Result<()> {
        self.compile_function_with_name(func, &func.name)
    }

    fn compile_function_with_name(&mut self, func: &Function, mangled_name: &str) -> Result<()> {
        self.register_function(func, mangled_name);
        let is_actual_method = mangled_name.contains("__impl__");
        self.compile_function_body(func, is_actual_method)
    }

    fn emit_gas_increment(&self, body: &mut WasmFunction, amount: i64) {
        body.instruction(&Instruction::GlobalGet(self.gas_counter_global));
        body.instruction(&Instruction::I64Const(amount));
        body.instruction(&Instruction::I64Add);
        body.instruction(&Instruction::GlobalSet(self.gas_counter_global));
    }

    fn register_function(&mut self, func: &Function, mangled_name: &str) -> u32 {
        let is_actual_method = mangled_name.contains("__impl__");
        let mut params: Vec<ValType> = func.params.iter().map(|_| ValType::I64).collect();
        if is_actual_method {
            params.insert(0, ValType::I64);
        }

        let results: Vec<ValType> = if func.return_type.is_some() {
            vec![ValType::I64]
        } else {
            vec![]
        };

        let type_idx = self.module.add_type(params, results);
        let func_idx = self.module.add_function(type_idx);

        self.function_map.insert(mangled_name.to_string(), func_idx);
        self.module.export_function(mangled_name, func_idx);
        func_idx
    }

    fn compile_function_body(&mut self, func: &Function, is_actual_method: bool) -> Result<()> {
        let is_method = self.current_contract.is_some();

        // Locals mapping - params come first
        let mut locals: std::collections::HashMap<String, u32> = std::collections::HashMap::new();
        let mut local_types: std::collections::HashMap<u32, Type> =
            std::collections::HashMap::new();

        self.enter_scope();

        let mut param_offset = 0;
        if is_actual_method {
            locals.insert("self".to_string(), 0);
            local_types.insert(
                0,
                Type::Path(self.current_contract.clone().unwrap_or("Contract".into())),
            );
            param_offset = 1;
        } else if is_method {
            // It's a contract function but not called via __impl__ (e.g. entry point)
            // Define 'self' as a local at the end of params
            let self_idx = func.params.len() as u32;
            locals.insert("self".to_string(), self_idx);
            local_types.insert(
                self_idx,
                Type::Path(self.current_contract.clone().unwrap_or("Contract".into())),
            );
        }

        for (i, param) in func.params.iter().enumerate() {
            let idx = (i as u32) + param_offset;
            locals.insert(param.name.clone(), idx);
            local_types.insert(idx, param.ty.clone());
            if param.ty == Type::Path("String".into()) {
                self.register_local(idx);
            }
        }

        // Count additional local variables needed (beyond params)
        let mut num_locals = Self::count_locals(&func.body);
        let self_local_base = if is_method && !is_actual_method {
            num_locals += 1;
            true
        } else {
            false
        };

        // Declare locals in WASM function
        // All locals are i64 for MVP
        // Add buffer for match expression scratch variables (nested matches)
        let wasm_local_types = vec![(num_locals + 16, ValType::I64)];
        let total_params = (func.params.len() as u32) + if is_actual_method { 1 } else { 0 };
        self.scratch_base = total_params + num_locals;
        self.scratch_depth = 0;
        let mut body = WasmFunction::new(wasm_local_types);

        if self_local_base {
            // Initialize 'self' local (at index params.len()) to 0
            body.instruction(&Instruction::I64Const(0));
            body.instruction(&Instruction::LocalSet(func.params.len() as u32));
        }

        // GAS INSTRUMENTATION: Function entry
        self.emit_gas_increment(&mut body, 10);

        let num_stmts = func.body.stmts.len();
        for (i, stmt) in func.body.stmts.iter().enumerate() {
            let is_last = i == num_stmts - 1;
            self.compile_stmt(
                stmt,
                &mut body,
                &mut locals,
                &mut local_types,
                is_last,
                func.return_type.is_some(),
            )?;
        }

        self.emit_cleanup_for_all_scopes(&mut body, &local_types);
        self.exit_scope(&mut body, &local_types);

        body.instruction(&Instruction::End);
        self.module.add_code(body);
        Ok(())
    }

    fn infer_type(
        &self,
        expr: &Expression,
        locals: &std::collections::HashMap<String, u32>,
        local_types: &std::collections::HashMap<u32, Type>,
    ) -> Type {
        match expr {
            Expression::Literal(Literal::Int(_)) => Type::Path("u64".into()),
            Expression::Literal(Literal::Bool(_)) => Type::Path("bool".into()),
            Expression::Literal(Literal::String(_)) => Type::Path("String".into()),
            Expression::Literal(Literal::Unit) => Type::Path("Unit".into()),
            Expression::Identifier(id) => {
                if id == "self" {
                    return Type::Path(self.current_contract.clone().unwrap_or("Contract".into()));
                }
                if let Some(&idx) = locals.get(id) {
                    local_types
                        .get(&idx)
                        .cloned()
                        .unwrap_or(Type::Path("u64".into()))
                } else if self.struct_layouts.contains_key(id)
                    || self.enum_variants.contains_key(id)
                    || self.generic_impls.contains_key(id)
                {
                    Type::Path(id.clone())
                } else {
                    Type::Path("u64".into())
                }
            }
            Expression::StructInit {
                name, type_args, ..
            } => {
                if type_args.is_empty() {
                    Type::Path(name.clone())
                } else {
                    Type::Generic(name.clone(), type_args.clone())
                }
            }
            Expression::EnumVariant { enum_name, .. } => Type::Path(enum_name.clone()),
            Expression::GenericInst { target, type_args } => {
                let base = self.infer_type(target, locals, local_types);
                if let Type::Path(name) = base {
                    Type::Generic(name, type_args.clone())
                } else {
                    base
                }
            }
            Expression::Call {
                func,
                args,
                type_args,
            } => {
                if let Expression::Identifier(s) = &**func {
                    if s == "ptr_read" {
                        if let Some(ty) = type_args.first() {
                            return ty.clone();
                        }
                        return Type::Path("u64".into());
                    }
                    if s == "ptr_write" {
                        return Type::Path("Unit".into());
                    }
                    if s == "sizeof" {
                        return Type::Path("u64".into());
                    }
                    if s == "emit" {
                        return Type::Path("Unit".into());
                    }
                    if s == "Ok" || s == "Err" {
                        if let Some(arg) = args.first() {
                            let arg_ty = self.infer_type(arg, locals, local_types);
                            if arg_ty == Type::Path("Unit".into()) {
                                return Type::Path("Unit".into());
                            }
                            return arg_ty;
                        }
                        return Type::Path("Unit".into());
                    }
                    // Check pre-scanned function returns
                    if let Some(ret_ty) = self.function_returns.get(s) {
                        return ret_ty.clone().unwrap_or(Type::Path("Unit".into()));
                    }
                }
                if let Expression::FieldAccess { expr, field } = &**func {
                    let obj_ty = self.infer_type(expr, locals, local_types);
                    let type_name = mangle_type(&obj_ty);
                    let naked_mangled = format!("{}__impl__{}", type_name, field);
                    if let Some(ret_ty) = self.function_returns.get(&naked_mangled) {
                        return ret_ty.clone().unwrap_or(Type::Path("Unit".into()));
                    }

                    // Fallback for msg properties if called
                    if let Expression::Identifier(obj) = &**expr
                        && obj == "msg"
                        && field == "sender"
                    {
                        return Type::Path("u64".into());
                    }
                }
                Type::Path("u64".into()) // Default for calls
            }
            Expression::FieldAccess { expr, field } => {
                let obj_ty = self.infer_type(expr, locals, local_types);
                match obj_ty {
                    Type::Path(name) if name == "Contract" => self
                        .storage_types
                        .get(field)
                        .cloned()
                        .unwrap_or(Type::Path("u64".into())),
                    _ => Type::Path("u64".into()),
                }
            }
            Expression::Binary { op, .. } => match op {
                BinaryOp::Assign => Type::Path("Unit".into()),
                BinaryOp::Eq
                | BinaryOp::Ne
                | BinaryOp::Lt
                | BinaryOp::Gt
                | BinaryOp::Le
                | BinaryOp::Ge => Type::Path("bool".into()),
                _ => Type::Path("u64".into()),
            },
            Expression::Index { .. } => Type::Path("u64".into()),
            Expression::Try(inner) => {
                let inner_ty = self.infer_type(inner, locals, local_types);
                match inner_ty {
                    Type::Generic(name, args) if name == "Result" && args.len() == 2 => {
                        args[0].clone()
                    }
                    _ => Type::Path("u64".into()),
                }
            }
            _ => Type::Path("u64".into()),
        }
    }

    fn compile_impl(&mut self, im: &Impl) -> Result<()> {
        let trait_prefix = if let Some(tr) = &im.trait_name {
            format!("{}__", tr)
        } else {
            "".to_string()
        };

        for m in &im.methods {
            let mangled_name = format!("{}{}__impl__{}", trait_prefix, im.target_name, m.name);
            self.compile_function_with_name(m, &mangled_name)?;
        }
        Ok(())
    }

    fn get_local_field_layout(
        &self,
        expr: &Expression,
        field: &str,
        locals: &std::collections::HashMap<String, u32>,
        local_types: &std::collections::HashMap<u32, Type>,
    ) -> Option<(u32, &FieldLayout)> {
        let name = match expr {
            Expression::Identifier(s) => s,
            _ => return None,
        };
        let idx = *locals.get(name)?;
        let struct_name = match local_types.get(&idx)? {
            Type::Path(s) => s,
            Type::Generic(s, _) => s,
            Type::Map(_, _) => return None, // Maps don't have fields in this sense
        };
        let layout = self.struct_layouts.get(struct_name)?;
        let f_layout = layout.iter().find(|fl| fl.name == *field)?;
        Some((idx, f_layout))
    }

    fn count_locals(block: &Block) -> u32 {
        let mut count = 0;
        for stmt in &block.stmts {
            if let Statement::Let { destruct_names, .. } = stmt {
                if destruct_names.is_empty() {
                    count += 1;
                } else {
                    count += destruct_names.len() as u32;
                }
            } else if let Statement::For { body, .. } = stmt {
                count += 1; // loop variable
                count += Self::count_locals(body);
            } else if let Statement::If {
                then_branch,
                else_branch,
                ..
            } = stmt
            {
                count += Self::count_locals(then_branch);
                if let Some(eb) = else_branch {
                    count += Self::count_locals(eb);
                }
            } else if let Statement::While { body, .. } = stmt {
                count += Self::count_locals(body);
            }
        }
        count
    }

    fn get_type_size(&self, ty: &Type) -> u32 {
        match ty {
            Type::Path(name) => {
                if let Some(size) = self.struct_sizes.get(name) {
                    return *size;
                }
                8
            }
            Type::Generic(name, _) => {
                if let Some(size) = self.struct_sizes.get(name) {
                    return *size;
                }
                8
            }
            _ => 8,
        }
    }

    fn compile_stmt(
        &mut self,
        stmt: &Statement,
        body: &mut WasmFunction,
        locals: &mut std::collections::HashMap<String, u32>,
        local_types: &mut std::collections::HashMap<u32, Type>,
        is_last: bool,
        expected_return: bool,
    ) -> Result<()> {
        match stmt {
            Statement::Let {
                name,
                destruct_names,
                ty,
                init,
                is_mut: _,
            } => {
                // Compile the initializer expression
                self.compile_expr(init, body, locals, local_types)?;

                // Calculate local index (params + existing locals)
                let local_idx = locals.values().max().map(|&v| v + 1).unwrap_or(0);

                // If no destructuring, simple case
                if destruct_names.is_empty() {
                    locals.insert(name.clone(), local_idx);

                    // Infer type if not provided
                    let inferred_ty = if let Some(t) = ty {
                        // Resolve generic type T -> u64
                        if let Type::Path(p) = t {
                            if let Some(concrete) = self.generic_context.get(p) {
                                concrete.clone()
                            } else {
                                t.clone()
                            }
                        } else {
                            t.clone()
                        }
                    } else {
                        self.infer_type(init, locals, local_types)
                    };
                    local_types.insert(local_idx, inferred_ty.clone());

                    // ARC: If String, Retain
                    let is_string = inferred_ty == Type::Path("String".into());
                    if is_string {
                        let scratch = self.scratch_base + self.scratch_depth;
                        body.instruction(&Instruction::LocalTee(scratch));
                        body.instruction(&Instruction::LocalGet(scratch));
                        body.instruction(&Instruction::Call(self.retain_func));
                        self.register_local(local_idx);
                    }

                    body.instruction(&Instruction::LocalSet(local_idx));
                } else {
                    // For destructuring, bind each name
                    for (i, dname) in destruct_names.iter().enumerate() {
                        let idx = local_idx + i as u32;
                        locals.insert(dname.clone(), idx);
                        local_types.insert(idx, Type::Path("u64".into())); // Assume u64 for now
                        if i == 0 {
                            body.instruction(&Instruction::LocalSet(idx));
                        }
                    }
                }
            }
            Statement::Expr(expr) => {
                self.compile_expr(expr, body, locals, local_types)?;
                let ty = self.infer_type(expr, locals, local_types);

                // Only drop result if it pushes to stack (non-Unit) and isn't used as return value
                if ty != Type::Path("Unit".into()) && (!is_last || !expected_return) {
                    body.instruction(&Instruction::Drop);
                }
            }
            Statement::Return(Some(expr)) => {
                self.compile_expr(expr, body, locals, local_types)?;
                self.emit_cleanup_for_all_scopes(body, local_types);
                body.instruction(&Instruction::Return);
            }
            Statement::Return(None) => {
                self.emit_cleanup_for_all_scopes(body, local_types);
                body.instruction(&Instruction::Return);
            }
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.compile_expr(condition, body, locals, local_types)?;
                body.instruction(&Instruction::I32WrapI64);

                body.instruction(&Instruction::If(wasm_encoder::BlockType::Empty));
                let num_then = then_branch.stmts.len();
                for (i, stmt) in then_branch.stmts.iter().enumerate() {
                    let is_last_in_block = i == num_then - 1;
                    self.compile_stmt(
                        stmt,
                        body,
                        locals,
                        local_types,
                        is_last && is_last_in_block,
                        expected_return,
                    )?;
                }

                if let Some(eb) = else_branch {
                    body.instruction(&Instruction::Else);
                    let num_else = eb.stmts.len();
                    for (i, stmt) in eb.stmts.iter().enumerate() {
                        let is_last_in_block = i == num_else - 1;
                        self.compile_stmt(
                            stmt,
                            body,
                            locals,
                            local_types,
                            is_last && is_last_in_block,
                            expected_return,
                        )?;
                    }
                }
                body.instruction(&Instruction::End);
            }
            Statement::While {
                condition,
                body: loop_body,
            } => {
                body.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
                self.control_stack.push(ControlFrame::Block {
                    scope_depth: self.scope_stack.len(),
                });

                body.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
                self.control_stack.push(ControlFrame::Loop {
                    scope_depth: self.scope_stack.len(),
                });

                self.compile_expr(condition, body, locals, local_types)?;
                body.instruction(&Instruction::I32WrapI64);
                body.instruction(&Instruction::I32Eqz);
                body.instruction(&Instruction::BrIf(1));

                self.enter_scope();
                self.emit_gas_increment(body, 5); // While loop iteration overhead
                for stmt in &loop_body.stmts {
                    self.compile_stmt(stmt, body, locals, local_types, false, false)?;
                }
                self.exit_scope(body, local_types);

                body.instruction(&Instruction::Br(0));
                body.instruction(&Instruction::End);
                self.control_stack.pop();
                body.instruction(&Instruction::End);
                self.control_stack.pop();
            }
            Statement::For {
                var_name,
                start,
                end,
                body: loop_body,
            } => {
                self.compile_expr(start, body, locals, local_types)?;

                let local_idx = locals.values().max().map(|&v| v + 1).unwrap_or(0);
                locals.insert(var_name.clone(), local_idx);
                local_types.insert(local_idx, Type::Path("u64".into()));
                body.instruction(&Instruction::LocalSet(local_idx));

                body.instruction(&Instruction::Block(wasm_encoder::BlockType::Empty));
                self.control_stack.push(ControlFrame::Block {
                    scope_depth: self.scope_stack.len(),
                });
                body.instruction(&Instruction::Loop(wasm_encoder::BlockType::Empty));
                self.control_stack.push(ControlFrame::Loop {
                    scope_depth: self.scope_stack.len(),
                });

                body.instruction(&Instruction::LocalGet(local_idx));
                self.compile_expr(end, body, locals, local_types)?;
                body.instruction(&Instruction::I64GtU);
                body.instruction(&Instruction::BrIf(1));

                self.enter_scope();
                self.emit_gas_increment(body, 5); // For loop iteration overhead
                for stmt in &loop_body.stmts {
                    self.compile_stmt(stmt, body, locals, local_types, false, false)?;
                }
                self.exit_scope(body, local_types);

                body.instruction(&Instruction::LocalGet(local_idx));
                body.instruction(&Instruction::I64Const(1));
                body.instruction(&Instruction::I64Add);
                body.instruction(&Instruction::LocalSet(local_idx));

                body.instruction(&Instruction::Br(0));
                body.instruction(&Instruction::End);
                self.control_stack.pop();
                body.instruction(&Instruction::End);
                self.control_stack.pop();
            }
            Statement::Break => {
                // Find nearest Block wrapping a Loop
                let mut relative_depth = 0;
                for frame in self.control_stack.iter().rev() {
                    match frame {
                        ControlFrame::Block { scope_depth } => {
                            self.emit_cleanup_until(*scope_depth, body, local_types);
                            body.instruction(&Instruction::Br(relative_depth));
                            break;
                        }
                        ControlFrame::Loop { .. } => {
                            relative_depth += 1;
                        }
                    }
                }
            }
            Statement::Continue => {
                // Find nearest Loop
                let mut relative_depth = 0;
                for frame in self.control_stack.iter().rev() {
                    match frame {
                        ControlFrame::Loop { scope_depth } => {
                            self.emit_cleanup_until(*scope_depth, body, local_types);
                            body.instruction(&Instruction::Br(relative_depth));
                            break;
                        }
                        ControlFrame::Block { .. } => {
                            relative_depth += 1;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn compile_expr(
        &mut self,
        expr: &Expression,
        body: &mut WasmFunction,
        locals: &mut std::collections::HashMap<String, u32>,
        local_types: &mut std::collections::HashMap<u32, Type>,
    ) -> Result<()> {
        match expr {
            Expression::Literal(lit) => match lit {
                Literal::Int(v) => {
                    body.instruction(&Instruction::I64Const(*v as i64));
                }
                Literal::Bool(b) => {
                    body.instruction(&Instruction::I32Const(if *b { 1 } else { 0 }));
                    body.instruction(&Instruction::I64ExtendI32S);
                }
                Literal::String(s) => {
                    // Store string bytes in data section
                    let bytes = s.as_bytes().to_vec();
                    let len = bytes.len() as i64;
                    let data_offset = self.module.add_data_segment(bytes);

                    // Allocate String struct: 32 bytes (ptr + len + capacity + rc)
                    body.instruction(&Instruction::I64Const(32));
                    let malloc_idx = self.malloc_func;
                    body.instruction(&Instruction::Call(malloc_idx));

                    // Store pointer to struct in a local (we'll need it multiple times)
                    let struct_ptr_local = self.scratch_base + self.scratch_depth;
                    self.scratch_depth += 1;
                    body.instruction(&Instruction::LocalTee(struct_ptr_local));

                    // Store data pointer (offset 0)
                    body.instruction(&Instruction::I32WrapI64); // Consume LocalTee result
                    body.instruction(&Instruction::I64Const(data_offset as i64));
                    body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                        offset: 0,
                        align: 3,
                        memory_index: 0,
                    }));

                    // Store length (offset 8)
                    body.instruction(&Instruction::LocalGet(struct_ptr_local));
                    body.instruction(&Instruction::I32WrapI64);
                    body.instruction(&Instruction::I64Const(len));
                    body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                        offset: 8,
                        align: 3,
                        memory_index: 0,
                    }));

                    // Store capacity (offset 16) - same as length for literals
                    body.instruction(&Instruction::LocalGet(struct_ptr_local));
                    body.instruction(&Instruction::I32WrapI64);
                    body.instruction(&Instruction::I64Const(len));
                    body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                        offset: 16,
                        align: 3,
                        memory_index: 0,
                    }));

                    // Store RC (offset 24) = 1
                    body.instruction(&Instruction::LocalGet(struct_ptr_local));
                    body.instruction(&Instruction::I32WrapI64);
                    body.instruction(&Instruction::I64Const(1));
                    body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                        offset: 24,
                        align: 3,
                        memory_index: 0,
                    }));

                    // Result: struct pointer
                    body.instruction(&Instruction::LocalGet(struct_ptr_local));
                    self.scratch_depth -= 1;
                }
                Literal::Unit => {
                    // Unit is empty stack
                }
            },
            Expression::Identifier(name) => {
                if let Some(idx) = locals.get(name) {
                    body.instruction(&Instruction::LocalGet(*idx));
                }
            }
            Expression::Binary { left, op, right } => {
                if *op == BinaryOp::Assign {
                    // Handle assignment
                    match &**left {
                        Expression::Identifier(name) => {
                            if let Some(idx) = locals.get(name).cloned() {
                                self.compile_expr(right, body, locals, local_types)?;

                                // ARC: If String, Retain new, Release old
                                let is_string =
                                    local_types.get(&idx) == Some(&Type::Path("String".into()));
                                if is_string {
                                    let scratch = self.scratch_base + self.scratch_depth;
                                    body.instruction(&Instruction::LocalTee(scratch));
                                    body.instruction(&Instruction::LocalGet(scratch));
                                    body.instruction(&Instruction::Call(self.retain_func));

                                    body.instruction(&Instruction::LocalGet(idx));
                                    body.instruction(&Instruction::Call(self.release_func));
                                }

                                body.instruction(&Instruction::LocalSet(idx));
                            } else if let Some(key) = self.storage_keys.get(name).cloned() {
                                // Check if String
                                let is_string = self.storage_types.get(name)
                                    == Some(&Type::Path("String".into()));

                                if is_string {
                                    self.compile_expr(right, body, locals, local_types)?;

                                    // String struct ptr is on stack
                                    let struct_ptr_local = self.scratch_base + self.scratch_depth;
                                    self.scratch_depth += 1;
                                    body.instruction(&Instruction::LocalSet(struct_ptr_local));

                                    // ARC: Retain storage value
                                    body.instruction(&Instruction::LocalGet(struct_ptr_local));
                                    body.instruction(&Instruction::Call(self.retain_func));

                                    let sw_idx = *self.function_map.get("storage_write").unwrap();

                                    // Write ptr (offset 0) to key
                                    body.instruction(&Instruction::I64Const(key));
                                    body.instruction(&Instruction::LocalGet(struct_ptr_local));
                                    body.instruction(&Instruction::I32WrapI64);
                                    body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                                        offset: 0,
                                        align: 3,
                                        memory_index: 0,
                                    }));
                                    body.instruction(&Instruction::Call(sw_idx));

                                    // Write len (offset 8) to key + 1
                                    body.instruction(&Instruction::I64Const(key + 1));
                                    body.instruction(&Instruction::LocalGet(struct_ptr_local));
                                    body.instruction(&Instruction::I32WrapI64);
                                    body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                                        offset: 8,
                                        align: 3,
                                        memory_index: 0,
                                    }));
                                    body.instruction(&Instruction::Call(sw_idx));

                                    // Write capacity (offset 16) to key + 2
                                    body.instruction(&Instruction::I64Const(key + 2));
                                    body.instruction(&Instruction::LocalGet(struct_ptr_local));
                                    body.instruction(&Instruction::I32WrapI64);
                                    body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                                        offset: 16,
                                        align: 3,
                                        memory_index: 0,
                                    }));
                                    body.instruction(&Instruction::Call(sw_idx));

                                    self.scratch_depth -= 1;
                                } else {
                                    // Simple storage field (u64)
                                    body.instruction(&Instruction::I64Const(key));
                                    self.compile_expr(right, body, locals, local_types)?;
                                    let sw_idx = *self.function_map.get("storage_write").unwrap();
                                    self.emit_gas_increment(body, 100); // Storage write cost
                                    body.instruction(&Instruction::Call(sw_idx));
                                }
                            }
                        }
                        Expression::FieldAccess { expr: obj, field } => {
                            if let Expression::Identifier(obj_name) = &**obj
                                && obj_name == "self"
                                && let Some(key) = self.storage_keys.get(field).copied()
                            {
                                // Check if String
                                let is_string = self.storage_types.get(field)
                                    == Some(&Type::Path("String".into()));

                                if is_string {
                                    self.compile_expr(right, body, locals, local_types)?;

                                    // String struct ptr is on stack
                                    let struct_ptr_local = self.scratch_base + self.scratch_depth;
                                    self.scratch_depth += 1;
                                    body.instruction(&Instruction::LocalSet(struct_ptr_local));

                                    // ARC: Retain storage value
                                    body.instruction(&Instruction::LocalGet(struct_ptr_local));
                                    body.instruction(&Instruction::Call(self.retain_func));

                                    let sw_idx = *self.function_map.get("storage_write").unwrap();
                                    self.emit_gas_increment(body, 100); // Storage write cost

                                    // Write ptr (offset 0) to key
                                    body.instruction(&Instruction::I64Const(key));
                                    body.instruction(&Instruction::LocalGet(struct_ptr_local));
                                    body.instruction(&Instruction::I32WrapI64);
                                    body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                                        offset: 0,
                                        align: 3,
                                        memory_index: 0,
                                    }));
                                    body.instruction(&Instruction::Call(sw_idx));

                                    // Write len (offset 8) to key + 1
                                    body.instruction(&Instruction::I64Const(key + 1));
                                    body.instruction(&Instruction::LocalGet(struct_ptr_local));
                                    body.instruction(&Instruction::I32WrapI64);
                                    body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                                        offset: 8,
                                        align: 3,
                                        memory_index: 0,
                                    }));
                                    body.instruction(&Instruction::Call(sw_idx));

                                    // Write capacity (offset 16) to key + 2
                                    body.instruction(&Instruction::I64Const(key + 2));
                                    body.instruction(&Instruction::LocalGet(struct_ptr_local));
                                    body.instruction(&Instruction::I32WrapI64);
                                    body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                                        offset: 16,
                                        align: 3,
                                        memory_index: 0,
                                    }));
                                    body.instruction(&Instruction::Call(sw_idx));

                                    self.scratch_depth -= 1;
                                } else {
                                    body.instruction(&Instruction::I64Const(key));
                                    self.compile_expr(right, body, locals, local_types)?;
                                    let sw_idx = *self.function_map.get("storage_write").unwrap();
                                    body.instruction(&Instruction::Call(sw_idx));
                                }
                            }
                        }
                        Expression::Index { expr: obj, index } => {
                            // Map entry assignment: self.balances[to] = value
                            if let Expression::FieldAccess { expr: base, field } = &**obj
                                && let Expression::Identifier(base_name) = &**base
                                && base_name == "self"
                                && let Some(field_key) = self.storage_keys.get(field)
                            {
                                // Key = hash(field_key, index_value)
                                body.instruction(&Instruction::I64Const(*field_key));
                                self.compile_expr(index, body, locals, local_types)?;
                                let h64_idx = *self.function_map.get("hash_i64").unwrap();
                                body.instruction(&Instruction::Call(h64_idx));

                                self.compile_expr(right, body, locals, local_types)?;
                                let sw_idx = *self.function_map.get("storage_write").unwrap();
                                body.instruction(&Instruction::Call(sw_idx));
                            }
                        }
                        _ => {}
                    }
                    return Ok(());
                }

                self.compile_expr(left, body, locals, local_types)?;
                self.compile_expr(right, body, locals, local_types)?;
                match op {
                    BinaryOp::Add => {
                        // Check if this is string concatenation
                        let left_ty = self.infer_type(left, locals, local_types);
                        let right_ty = self.infer_type(right, locals, local_types);

                        if left_ty == Type::Path("String".into())
                            && right_ty == Type::Path("String".into())
                        {
                            // String concatenation
                            // Stack: [left_ptr, right_ptr]

                            // Store pointers in scratch locals
                            let left_local = self.scratch_base + self.scratch_depth;
                            let right_local = left_local + 1;
                            self.scratch_depth += 2;

                            body.instruction(&Instruction::LocalSet(right_local));
                            body.instruction(&Instruction::LocalSet(left_local));

                            // Load left.len (offset 8)
                            body.instruction(&Instruction::LocalGet(left_local));
                            body.instruction(&Instruction::I32WrapI64);
                            body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                                offset: 8,
                                align: 3,
                                memory_index: 0,
                            }));
                            let left_len_local = left_local + 2;
                            body.instruction(&Instruction::LocalSet(left_len_local));

                            // Load right.len (offset 8)
                            body.instruction(&Instruction::LocalGet(right_local));
                            body.instruction(&Instruction::I32WrapI64);
                            body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                                offset: 8,
                                align: 3,
                                memory_index: 0,
                            }));
                            let right_len_local = left_local + 3;
                            body.instruction(&Instruction::LocalSet(right_len_local));

                            // Calculate total length
                            body.instruction(&Instruction::LocalGet(left_len_local));
                            body.instruction(&Instruction::LocalGet(right_len_local));
                            body.instruction(&Instruction::I64Add);
                            let total_len_local = left_local + 4;
                            body.instruction(&Instruction::LocalTee(total_len_local));

                            // Allocate buffer for concatenated data
                            let malloc_idx = self.malloc_func;
                            body.instruction(&Instruction::Call(malloc_idx));
                            let data_ptr_local = left_local + 5;
                            body.instruction(&Instruction::LocalTee(data_ptr_local));

                            // Copy left string data
                            // memory.copy(dest, src, len)
                            body.instruction(&Instruction::I32WrapI64); // dest
                            body.instruction(&Instruction::LocalGet(left_local));
                            body.instruction(&Instruction::I32WrapI64);
                            body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                                offset: 0,
                                align: 3,
                                memory_index: 0,
                            }));
                            body.instruction(&Instruction::I32WrapI64); // src (left.ptr)
                            body.instruction(&Instruction::LocalGet(left_len_local));
                            body.instruction(&Instruction::I32WrapI64); // len
                            body.instruction(&Instruction::MemoryCopy {
                                src_mem: 0,
                                dst_mem: 0,
                            });

                            // Copy right string data (offset by left.len)
                            body.instruction(&Instruction::LocalGet(data_ptr_local));
                            body.instruction(&Instruction::LocalGet(left_len_local));
                            body.instruction(&Instruction::I64Add);
                            body.instruction(&Instruction::I32WrapI64); // dest + left.len
                            body.instruction(&Instruction::LocalGet(right_local));
                            body.instruction(&Instruction::I32WrapI64);
                            body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                                offset: 0,
                                align: 3,
                                memory_index: 0,
                            }));
                            body.instruction(&Instruction::I32WrapI64); // src (right.ptr)
                            body.instruction(&Instruction::LocalGet(right_len_local));
                            body.instruction(&Instruction::I32WrapI64); // len
                            body.instruction(&Instruction::MemoryCopy {
                                src_mem: 0,
                                dst_mem: 0,
                            });

                            // Allocate new String struct (32 bytes)
                            body.instruction(&Instruction::I64Const(32));
                            body.instruction(&Instruction::Call(malloc_idx));
                            let result_local = left_local + 6;
                            body.instruction(&Instruction::LocalTee(result_local));

                            // Store data pointer (offset 0)
                            body.instruction(&Instruction::I32WrapI64); // Consume LocalTee result
                            body.instruction(&Instruction::LocalGet(data_ptr_local));
                            body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                                offset: 0,
                                align: 3,
                                memory_index: 0,
                            }));

                            // Store length (offset 8)
                            body.instruction(&Instruction::LocalGet(result_local));
                            body.instruction(&Instruction::I32WrapI64);
                            body.instruction(&Instruction::LocalGet(total_len_local));
                            body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                                offset: 8,
                                align: 3,
                                memory_index: 0,
                            }));

                            // Store capacity (offset 16)
                            body.instruction(&Instruction::LocalGet(result_local));
                            body.instruction(&Instruction::I32WrapI64);
                            body.instruction(&Instruction::LocalGet(total_len_local));
                            body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                                offset: 16,
                                align: 3,
                                memory_index: 0,
                            }));

                            // Store RC (offset 24) = 1
                            body.instruction(&Instruction::LocalGet(result_local));
                            body.instruction(&Instruction::I32WrapI64);
                            body.instruction(&Instruction::I64Const(1));
                            body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                                offset: 24,
                                align: 3,
                                memory_index: 0,
                            }));

                            // Push result pointer
                            body.instruction(&Instruction::LocalGet(result_local));
                            self.scratch_depth -= 2;
                        } else {
                            // Integer addition
                            body.instruction(&Instruction::I64Add);
                        }
                    }
                    BinaryOp::Sub => {
                        body.instruction(&Instruction::I64Sub);
                    }
                    BinaryOp::Mul => {
                        body.instruction(&Instruction::I64Mul);
                    }
                    BinaryOp::Div => {
                        body.instruction(&Instruction::I64DivU);
                    }
                    BinaryOp::Mod => {
                        body.instruction(&Instruction::I64RemU);
                    }
                    BinaryOp::Eq => {
                        body.instruction(&Instruction::I64Eq);
                        body.instruction(&Instruction::I64ExtendI32U);
                    }
                    BinaryOp::Ne => {
                        body.instruction(&Instruction::I64Ne);
                        body.instruction(&Instruction::I64ExtendI32U);
                    }
                    BinaryOp::Lt => {
                        body.instruction(&Instruction::I64LtU);
                        body.instruction(&Instruction::I64ExtendI32U);
                    }
                    BinaryOp::Gt => {
                        body.instruction(&Instruction::I64GtU);
                        body.instruction(&Instruction::I64ExtendI32U);
                    }
                    BinaryOp::Le => {
                        body.instruction(&Instruction::I64LeU);
                        body.instruction(&Instruction::I64ExtendI32U);
                    }
                    BinaryOp::Ge => {
                        body.instruction(&Instruction::I64GeU);
                        body.instruction(&Instruction::I64ExtendI32U);
                    }
                    _ => {
                        // Default fallback
                        body.instruction(&Instruction::Drop);
                    }
                }
            }
            Expression::Call {
                func,
                args,
                type_args,
            } => {
                let mut final_args = args.clone();
                let mut is_indirect = false;
                let func_name = match &**func {
                    Expression::Identifier(s) => {
                        if s == "ptr_write" {
                            let ptr_arg = &args[0];
                            let val_arg = &args[1];
                            let val_ty = self.infer_type(val_arg, locals, local_types);
                            let size = self.get_type_size(&val_ty);

                            self.compile_expr(ptr_arg, body, locals, local_types)?; // Dst (I64)
                            if size <= 8 {
                                body.instruction(&Instruction::I32WrapI64); // Addr (I32)
                                self.compile_expr(val_arg, body, locals, local_types)?; // Val (I64)
                                body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                                    offset: 0,
                                    align: 3,
                                    memory_index: 0,
                                }));
                            } else {
                                // Alloc/Copy for > 8 bytes
                                // ptr is dst. val is src (pointer to struct).
                                body.instruction(&Instruction::I32WrapI64); // Dst Addr (I32)

                                self.compile_expr(val_arg, body, locals, local_types)?; // Src (I64)
                                body.instruction(&Instruction::I32WrapI64); // Src Addr (I32)

                                body.instruction(&Instruction::I32Const(size as i32));
                                body.instruction(&Instruction::MemoryCopy {
                                    src_mem: 0,
                                    dst_mem: 0,
                                });
                            }
                            return Ok(());
                        }
                        if s == "ptr_read" {
                            let ptr_arg = &args[0];
                            let ty = type_args
                                .first()
                                .cloned()
                                .unwrap_or(Type::Path("u64".into()));
                            let size = self.get_type_size(&ty);

                            if size <= 8 {
                                self.compile_expr(ptr_arg, body, locals, local_types)?;
                                body.instruction(&Instruction::I32WrapI64);
                                body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                                    offset: 0,
                                    align: 3,
                                    memory_index: 0,
                                }));
                            } else {
                                // Alloc & Copy
                                body.instruction(&Instruction::I64Const(size as i64));
                                body.instruction(&Instruction::Call(self.malloc_func));

                                let scratch = self.scratch_base + self.scratch_depth;
                                self.scratch_depth += 1;
                                body.instruction(&Instruction::LocalTee(scratch)); // Copy 1

                                body.instruction(&Instruction::I32WrapI64); // Dst (I32)

                                self.compile_expr(ptr_arg, body, locals, local_types)?; // Ptr (I64)
                                body.instruction(&Instruction::I32WrapI64); // Src (I32)

                                body.instruction(&Instruction::I32Const(size as i32));

                                body.instruction(&Instruction::MemoryCopy {
                                    src_mem: 0,
                                    dst_mem: 0,
                                });

                                body.instruction(&Instruction::LocalGet(scratch));
                                self.scratch_depth -= 1;
                            }
                            return Ok(());
                        }

                        if s == "sizeof" {
                            let ty = type_args
                                .first()
                                .cloned()
                                .unwrap_or(Type::Path("u64".into()));
                            let size = self.get_type_size(&ty);
                            body.instruction(&Instruction::I64Const(size as i64));
                            return Ok(());
                        }

                        if s == "Ok" || s == "Err" {
                            // Allocate 16 bytes for Result: [tag: i64, data: i64]
                            body.instruction(&Instruction::I64Const(16));
                            body.instruction(&Instruction::Call(self.malloc_func));

                            let ptr_local = self.scratch_base + self.scratch_depth;
                            self.scratch_depth += 1;
                            body.instruction(&Instruction::LocalTee(ptr_local));

                            // Store tag: 0 for Ok, 1 for Err
                            body.instruction(&Instruction::I32WrapI64);
                            body.instruction(&Instruction::I64Const(if s == "Ok" { 0 } else { 1 }));
                            body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                                offset: 0,
                                align: 3,
                                memory_index: 0,
                            }));

                            // Store data (arg 0)
                            body.instruction(&Instruction::LocalGet(ptr_local));
                            body.instruction(&Instruction::I32WrapI64);
                            self.compile_expr(&args[0], body, locals, local_types)?;
                            body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                                offset: 8,
                                align: 3,
                                memory_index: 0,
                            }));

                            // Result: pointer
                            body.instruction(&Instruction::LocalGet(ptr_local));
                            self.scratch_depth -= 1;
                            return Ok(());
                        }

                        if s == "emit" {
                            // emit(EventStruct { ... })
                            let arg = &args[0];
                            let event_ty = self.infer_type(arg, locals, local_types);
                            if let Type::Path(event_name) = event_ty {
                                let id = event_name.chars().map(|c| c as i64).sum::<i64>();
                                body.instruction(&Instruction::I64Const(id));
                                self.compile_expr(arg, body, locals, local_types)?;
                                let emit_idx = *self.function_map.get("emit_event").unwrap();
                                body.instruction(&Instruction::Call(emit_idx));
                                return Ok(());
                            }
                        }
                        if s == "Ok" || s == "Err" {
                            // Result intrinsics - for now just return the inner value
                            if !args.is_empty() {
                                self.compile_expr(&args[0], body, locals, local_types)?;
                            }
                            return Ok(()).map_err(|e: anyhow::Error| e); // Type hint for compiler
                        }
                        if locals.contains_key(s) {
                            is_indirect = true;
                            s.clone()
                        } else if let Some(c) = &self.current_contract {
                            let mangled = format!("{}__impl__{}", c, s);
                            if self.function_map.contains_key(&mangled)
                                && locals.contains_key("self")
                            {
                                is_indirect = false;
                                final_args.insert(0, Expression::Identifier("self".to_string()));
                                mangled
                            } else {
                                s.clone()
                            }
                        } else {
                            s.clone()
                        }
                    }
                    Expression::FieldAccess { expr, field } => {
                        // Method call: obj.method()
                        if let Expression::Identifier(obj_name) = &**expr
                            && obj_name == "msg"
                        {
                            match field.as_str() {
                                "sender" => {
                                    let func_idx = *self.function_map.get("get_caller").unwrap();
                                    body.instruction(&Instruction::Call(func_idx));
                                    return Ok(());
                                }
                                "value" => {
                                    let func_idx = *self.function_map.get("get_value").unwrap();
                                    body.instruction(&Instruction::Call(func_idx));
                                    return Ok(());
                                }
                                "data" => {
                                    let func_idx = *self.function_map.get("get_data").unwrap();
                                    body.instruction(&Instruction::Call(func_idx));
                                    return Ok(());
                                }
                                _ => {}
                            }
                        }

                        let obj_ty = self.infer_type(expr, locals, local_types);

                        // Handle built-in String methods
                        if obj_ty == Type::Path("String".into()) {
                            match field.as_str() {
                                "len" => {
                                    // Load String struct pointer
                                    self.compile_expr(expr, body, locals, local_types)?;
                                    // Load len field (offset 8)
                                    body.instruction(&Instruction::I32WrapI64);
                                    body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                                        offset: 8,
                                        align: 3,
                                        memory_index: 0,
                                    }));
                                }
                                "get_rc" => {
                                    // Load String struct pointer
                                    self.compile_expr(expr, body, locals, local_types)?;
                                    // Load rc field (offset 24)
                                    body.instruction(&Instruction::I32WrapI64);
                                    body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                                        offset: 24,
                                        align: 3,
                                        memory_index: 0,
                                    }));
                                    return Ok(());
                                }
                                "is_empty" => {
                                    // Load String struct pointer
                                    self.compile_expr(expr, body, locals, local_types)?;
                                    // Load len field (offset 8)
                                    body.instruction(&Instruction::I32WrapI64);
                                    body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                                        offset: 8,
                                        align: 3,
                                        memory_index: 0,
                                    }));
                                    // Check if len == 0
                                    body.instruction(&Instruction::I64Eqz);
                                    body.instruction(&Instruction::I64ExtendI32U);
                                    return Ok(());
                                }
                                "as_bytes" => {
                                    // Load String struct pointer
                                    self.compile_expr(expr, body, locals, local_types)?;
                                    // Load ptr field (offset 0)
                                    body.instruction(&Instruction::I32WrapI64);
                                    body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                                        offset: 0,
                                        align: 3,
                                        memory_index: 0,
                                    }));
                                    return Ok(());
                                }
                                _ => {}
                            }
                        }

                        if let Type::Generic(base_name, type_args) = &obj_ty
                            && self.generic_impls.contains_key(base_name)
                        {
                            self.instantiate_impl(base_name, type_args)?;
                        }
                        let type_name = mangle_type(&obj_ty);
                        // Try naked impl first: TypeName__impl__MethodName
                        let naked_mangled = format!("{}__impl__{}", type_name, field);
                        let resolved_name = if self.function_map.contains_key(&naked_mangled) {
                            naked_mangled
                        } else {
                            // Search for trait impls: Trait__Type__impl__method
                            let suffix = format!("{}__impl__{}", type_name, field);
                            if let Some(found) =
                                self.function_map.keys().find(|k| k.ends_with(&suffix))
                            {
                                found.clone()
                            } else {
                                naked_mangled // Fallback
                            }
                        };
                        // Receiver is first argument
                        final_args.insert(0, (**expr).clone());
                        resolved_name
                    }
                    _ => {
                        is_indirect = true;
                        "".to_string()
                    }
                };

                // Check if this is a generic function
                let is_generic = self.generic_functions.contains_key(&func_name);

                // Infer type arguments if not provided
                let resolved_type_args = if is_generic && type_args.is_empty() {
                    // Infer from first argument type
                    if let Some(first_arg) = args.first() {
                        // We need to determine the type of the argument
                        // For simple cases, we can infer from the expression
                        let inferred_ty = match first_arg {
                            Expression::Literal(Literal::Int(_)) => Type::Path("u64".into()),
                            Expression::Literal(Literal::Bool(_)) => Type::Path("bool".into()),
                            Expression::Identifier(id) => {
                                // Look up in locals
                                if let Some(&idx) = locals.get(id) {
                                    local_types
                                        .get(&idx)
                                        .cloned()
                                        .unwrap_or(Type::Path("u64".into()))
                                } else {
                                    Type::Path("u64".into())
                                }
                            }
                            Expression::StructInit { name, .. } => Type::Path(name.clone()),
                            Expression::EnumVariant { enum_name, .. } => {
                                Type::Path(enum_name.clone())
                            }
                            _ => Type::Path("u64".into()),
                        };
                        vec![inferred_ty]
                    } else {
                        vec![]
                    }
                } else {
                    type_args.clone()
                };

                for arg in &final_args {
                    self.compile_expr(arg, body, locals, local_types)?;

                    let arg_ty = self.infer_type(arg, locals, local_types);
                    if arg_ty == Type::Path("String".into()) {
                        let scratch = self.scratch_base + self.scratch_depth;
                        body.instruction(&Instruction::LocalTee(scratch));
                        body.instruction(&Instruction::LocalGet(scratch));
                        body.instruction(&Instruction::Call(self.retain_func));
                    }
                }

                if is_indirect {
                    // Stack: [args..., table_index]
                    self.compile_expr(func, body, locals, local_types)?;

                    let arg_types = vec![ValType::I64; final_args.len()];
                    let sig_idx = self.module.add_type(arg_types, vec![ValType::I64]);

                    body.instruction(&Instruction::CallIndirect {
                        ty: sig_idx,
                        table: 0,
                    });
                } else {
                    let target_name = if !resolved_type_args.is_empty() {
                        let key = (func_name.clone(), resolved_type_args.clone());
                        if let Some(mangled) = self.monomorphized_instances.get(&key) {
                            mangled.clone()
                        } else {
                            let mangled =
                                format!("{}__{}", func_name, mangle_types(&resolved_type_args));
                            self.monomorphized_instances
                                .insert(key.clone(), mangled.clone());

                            let mut func_node = self
                                .generic_functions
                                .get(&func_name)
                                .ok_or_else(|| {
                                    anyhow::anyhow!("Generic function not found: {}", func_name)
                                })?
                                .clone();

                            let old_context = self.generic_context.clone();
                            self.generic_context.clear();
                            for (i, g_param) in func_node.generics.iter().enumerate() {
                                if let Some(arg_ty) = resolved_type_args.get(i) {
                                    self.generic_context.insert(g_param.clone(), arg_ty.clone());
                                }
                            }

                            func_node.name = mangled.clone();
                            self.compile_function_with_name(&func_node, &mangled)?;
                            self.generic_context = old_context;

                            mangled
                        }
                    } else {
                        func_name
                    };

                    let func_idx = self.function_map.get(&target_name).ok_or_else(|| {
                        let available: Vec<_> = self.function_map.keys().cloned().collect();
                        anyhow::anyhow!(
                            "Undefined function: {}. Available: {:?}",
                            target_name,
                            available
                        )
                    })?;
                    self.emit_gas_increment(body, 10); // Call overhead
                    body.instruction(&Instruction::Call(*func_idx));
                }
            }
            Expression::FieldAccess { expr, field } => {
                // If msg property
                let maybe_msg_func = match &**expr {
                    Expression::Identifier(obj_name) if obj_name == "msg" => match field.as_str() {
                        "sender" => self.function_map.get("get_caller").copied(),
                        "value" => self.function_map.get("get_value").copied(),
                        "data" => self.function_map.get("get_data").copied(),
                        _ => None,
                    },
                    _ => None,
                };

                if let Some(func_idx) = maybe_msg_func {
                    body.instruction(&Instruction::Call(func_idx));
                    return Ok(());
                }

                // If expr is self.field, it's a storage read
                if let Expression::Identifier(id) = &**expr
                    && id == "self"
                    && let Some(key) = self.storage_keys.get(field)
                {
                    self.emit_gas_increment(body, 50); // Storage read cost
                    // Check if String
                    let is_string =
                        self.storage_types.get(field) == Some(&Type::Path("String".into()));

                    if is_string {
                        // Allocate String struct (32 bytes)
                        body.instruction(&Instruction::I64Const(32));
                        body.instruction(&Instruction::Call(self.malloc_func));

                        let struct_ptr_local = self.scratch_base + self.scratch_depth;
                        self.scratch_depth += 1;
                        body.instruction(&Instruction::LocalSet(struct_ptr_local));

                        let sr_idx = *self.function_map.get("storage_read").unwrap();

                        // Read ptr (key) -> struct offset 0
                        body.instruction(&Instruction::LocalGet(struct_ptr_local));
                        body.instruction(&Instruction::I32WrapI64);
                        body.instruction(&Instruction::I64Const(*key));
                        body.instruction(&Instruction::Call(sr_idx));
                        body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                            offset: 0,
                            align: 3,
                            memory_index: 0,
                        }));

                        // Read len (key + 1) -> struct offset 8
                        body.instruction(&Instruction::LocalGet(struct_ptr_local));
                        body.instruction(&Instruction::I32WrapI64);
                        body.instruction(&Instruction::I64Const(key + 1));
                        body.instruction(&Instruction::Call(sr_idx));
                        body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                            offset: 8,
                            align: 3,
                            memory_index: 0,
                        }));

                        // Read cap (key + 2) -> struct offset 16
                        body.instruction(&Instruction::LocalGet(struct_ptr_local));
                        body.instruction(&Instruction::I32WrapI64);
                        body.instruction(&Instruction::I64Const(key + 2));
                        body.instruction(&Instruction::Call(sr_idx));
                        body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                            offset: 16,
                            align: 3,
                            memory_index: 0,
                        }));

                        // Init RC = 1
                        body.instruction(&Instruction::LocalGet(struct_ptr_local));
                        body.instruction(&Instruction::I32WrapI64);
                        body.instruction(&Instruction::I64Const(1));
                        body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                            offset: 24,
                            align: 3,
                            memory_index: 0,
                        }));

                        // Return struct pointer
                        body.instruction(&Instruction::LocalGet(struct_ptr_local));
                        self.scratch_depth -= 1;
                    } else {
                        body.instruction(&Instruction::I64Const(*key));
                        let sr_idx = *self.function_map.get("storage_read").unwrap();
                        body.instruction(&Instruction::Call(sr_idx));
                    }
                    return Ok(());
                }

                // If expr is an identifier pointing to a struct local
                let _obj_ty = self.infer_type(expr, locals, local_types);
                if let Some((idx, f_layout)) =
                    self.get_local_field_layout(expr, field, locals, local_types)
                {
                    body.instruction(&Instruction::LocalGet(idx));
                    body.instruction(&Instruction::I64Const(f_layout.offset as i64));
                    body.instruction(&Instruction::I64Add);
                    body.instruction(&Instruction::I32WrapI64);
                    body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                        offset: 0,
                        align: 3, // 8 bytes
                        memory_index: 0,
                    }));
                    return Ok(());
                }

                // Fallback for complex expressions or unknown types
                self.compile_expr(expr, body, locals, local_types)?;
                body.instruction(&Instruction::I32WrapI64);
                body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));
            }
            Expression::Index { expr, index } => {
                // Storage Map access: self.balances[sender]
                if let Expression::FieldAccess { expr: base, field } = &**expr
                    && let Expression::Identifier(base_name) = &**base
                    && base_name == "self"
                    && let Some(field_key) = self.storage_keys.get(field)
                {
                    body.instruction(&Instruction::I64Const(*field_key));
                    self.compile_expr(index, body, locals, local_types)?;
                    let h64_idx = *self.function_map.get("hash_i64").unwrap();
                    body.instruction(&Instruction::Call(h64_idx));

                    let sr_idx = *self.function_map.get("storage_read").unwrap();
                    body.instruction(&Instruction::Call(sr_idx));
                    return Ok(());
                }
            }
            Expression::StructInit { name, fields, .. } => {
                // Calculate struct size and allocate memory
                let struct_size = if let Some(layout) = self.struct_layouts.get(name) {
                    // Size is last field's offset + 8 bytes (assuming all fields are i64)
                    layout.iter().map(|f| f.offset + 8).max().unwrap_or(8)
                } else {
                    8 // Default size if layout not found
                };

                // Call malloc to allocate memory
                body.instruction(&Instruction::I64Const(struct_size as i64));
                body.instruction(&Instruction::Call(self.malloc_func));

                // Use current scratch depth to avoid clashing in nested inits
                let temp_idx = self.scratch_base + self.scratch_depth;
                self.scratch_depth += 1;

                body.instruction(&Instruction::LocalSet(temp_idx));

                if let Some(layout) = self.struct_layouts.get(name) {
                    let field_work: Vec<(u64, &Expression)> = fields
                        .iter()
                        .filter_map(|(f_name, f_expr)| {
                            layout
                                .iter()
                                .find(|fl| fl.name == *f_name)
                                .map(|fl| (fl.offset as u64, f_expr))
                        })
                        .collect();

                    for (offset, f_expr) in field_work {
                        body.instruction(&Instruction::LocalGet(temp_idx)); // Base
                        body.instruction(&Instruction::I32WrapI64);
                        self.compile_expr(f_expr, body, locals, local_types)?; // Val
                        body.instruction(&Instruction::I64Store(wasm_encoder::MemArg {
                            offset,
                            align: 3,
                            memory_index: 0,
                        }));
                    }
                }

                body.instruction(&Instruction::LocalGet(temp_idx)); // Result pointer
                self.scratch_depth -= 1;
            }
            Expression::EnumVariant {
                enum_name,
                variant_name,
            } => {
                if let Some(val) = self
                    .enum_variants
                    .get(enum_name)
                    .and_then(|v| v.get(variant_name))
                {
                    body.instruction(&Instruction::I64Const(*val as i64));
                }
            }
            Expression::GenericInst { target, .. } => {
                self.compile_expr(target, body, locals, local_types)?;
            }
            Expression::Try(inner) => {
                // inner evaluates to Result pointer (i64)
                self.compile_expr(inner, body, locals, local_types)?;

                let ptr_local = self.scratch_base + self.scratch_depth;
                self.scratch_depth += 1;
                body.instruction(&Instruction::LocalTee(ptr_local));

                // Load tag (0 = Ok, 1 = Err)
                body.instruction(&Instruction::I32WrapI64);
                body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                    offset: 0,
                    align: 3,
                    memory_index: 0,
                }));

                // Check if tag == 0 (Ok)
                body.instruction(&Instruction::I64Eqz);
                body.instruction(&Instruction::If(wasm_encoder::BlockType::Result(
                    ValType::I64,
                )));

                // Ok branch: load data and push to stack
                body.instruction(&Instruction::LocalGet(ptr_local));
                body.instruction(&Instruction::I32WrapI64);
                body.instruction(&Instruction::I64Load(wasm_encoder::MemArg {
                    offset: 8,
                    align: 3,
                    memory_index: 0,
                }));

                body.instruction(&Instruction::Else);

                // Err branch: return the current result pointer and terminate function execution
                self.emit_cleanup_for_all_scopes(body, local_types);
                body.instruction(&Instruction::LocalGet(ptr_local));
                body.instruction(&Instruction::Return);
                body.instruction(&Instruction::I64Const(0)); // Dummy for BlockType matching

                body.instruction(&Instruction::End);
                self.scratch_depth -= 1;
            }
            Expression::Match { value, arms } => {
                // Compile value
                self.compile_expr(value, body, locals, local_types)?;

                // Store value in temporary local?
                // Creating a new local on fly is tricky in WASM one-pass without pre-alloc.
                // Easier to use stack, but we need to duplicate it for comparisons.
                // Or: assume we have a scratch local or allocate one at start of function?
                // For NOW: Just chain if-else blocks checking the value on stack.
                // But `match` value is computed once.

                let temp_idx = (locals.len() + 14) as u32; // Use buffer local

                body.instruction(&Instruction::LocalSet(temp_idx));
                // We don't insert into 'locals' map because it's temporary and we don't want to shadow user vars
                // locals.insert(".match_temp".into(), temp_idx);

                // Generate if/else chain
                // if temp == pattern1 { body1 } else { if temp == pattern2 { body2 } ... }

                let mut closing_ends = 0;

                for arm in arms {
                    match &arm.pattern {
                        Pattern::EnumVariant {
                            enum_name,
                            variant_name,
                        } => {
                            body.instruction(&Instruction::LocalGet(temp_idx));
                            if let Some(val) = self
                                .enum_variants
                                .get(enum_name)
                                .and_then(|v| v.get(variant_name))
                            {
                                body.instruction(&Instruction::I64Const(*val as i64));
                            }
                            body.instruction(&Instruction::I64Eq);
                            // If
                            body.instruction(&Instruction::If(wasm_encoder::BlockType::Empty)); // Result type? 
                            // Match is expression, so it should return a value.
                            // But our If blocks are Empty type for now (void).
                            // We need to handle result type. If match expects a result, we need block result type I64.
                            // For MVP, assume Match used as statement or returns i64.

                            self.compile_expr(&arm.body, body, locals, local_types)?;
                            body.instruction(&Instruction::Else);
                            closing_ends += 1;
                        }
                        Pattern::Wildcard => {
                            // Wildcard must be last
                            self.compile_expr(&arm.body, body, locals, local_types)?;
                        }
                        _ => {}
                    }
                }

                // Close all Ifs
                for _ in 0..closing_ends {
                    body.instruction(&Instruction::End);
                }
            }
            Expression::Closure {
                params,
                body: closure_body,
            } => {
                let closure_name = format!("closure_{}", self.closure_counter);
                self.closure_counter += 1;

                let func = Function {
                    name: closure_name.clone(),
                    params: params.clone(),
                    return_type: None,
                    body: (**closure_body).clone(),
                    is_pub: false,
                    generics: vec![],
                };

                self.compile_function_with_name(&func, &closure_name)?;

                let func_idx = *self.function_map.get(&closure_name).unwrap();
                let table_idx = self.module.add_func_to_table(func_idx);

                // Return table index as i64
                body.instruction(&Instruction::I64Const(table_idx as i64));
            }
        }
        Ok(())
    }
}

fn mangle_types(types: &[Type]) -> String {
    types.iter().map(mangle_type).collect::<Vec<_>>().join("_")
}

fn mangle_type(t: &Type) -> String {
    match t {
        Type::Path(s) => s.clone(),
        Type::Generic(s, args) => format!("{}_{}", s, mangle_types(args)),
        Type::Map(k, v) => format!("Map_{}_{}", mangle_type(k), mangle_type(v)),
    }
}

// Generics Implementation
impl CodeGenerator {
    fn instantiate_impl(&mut self, name: &str, args: &[Type]) -> Result<()> {
        let impl_key = (name.to_string(), args.to_vec());
        if self.monomorphized_instances.contains_key(&impl_key) {
            return Ok(());
        }

        let generic_impl = if let Some(im) = self.generic_impls.get(name) {
            im.clone()
        } else {
            return Ok(());
        };

        let mut type_map = std::collections::HashMap::new();
        for (i, param) in generic_impl.generics.iter().enumerate() {
            if let Some(arg) = args.get(i) {
                type_map.insert(param.clone(), arg.clone());
            }
        }

        for method in generic_impl.methods {
            let specialized = self.monomorphize_function(&method, &type_map, name, args)?;
            self.compile_function(&specialized)?;
        }

        let mangled_impl_name = format!("{}_{}", name, mangle_types(args));
        self.monomorphized_instances
            .insert(impl_key, mangled_impl_name);
        Ok(())
    }

    fn monomorphize_function(
        &self,
        func: &Function,
        type_map: &std::collections::HashMap<String, Type>,
        impl_name: &str,
        type_args: &[Type],
    ) -> Result<Function> {
        let mut specialized = func.clone();
        let mangled_type_name = format!("{}_{}", impl_name, mangle_types(type_args));
        specialized.name = format!("{}__impl__{}", mangled_type_name, func.name);
        specialized.generics.clear();

        for param in &mut specialized.params {
            param.ty = Self::substitute_type(&param.ty, type_map);
        }

        if let Some(rt) = &mut specialized.return_type {
            *rt = Self::substitute_type(rt, type_map);
        }

        specialized.body = Self::substitute_block(&specialized.body, type_map);

        Ok(specialized)
    }

    fn substitute_type(ty: &Type, mapping: &std::collections::HashMap<String, Type>) -> Type {
        match ty {
            Type::Path(s) => {
                if let Some(t) = mapping.get(s) {
                    t.clone()
                } else {
                    Type::Path(s.clone())
                }
            }
            Type::Generic(s, args) => Type::Generic(
                s.clone(),
                args.iter()
                    .map(|a| Self::substitute_type(a, mapping))
                    .collect(),
            ),
            Type::Map(k, v) => Type::Map(
                Box::new(Self::substitute_type(k, mapping)),
                Box::new(Self::substitute_type(v, mapping)),
            ),
        }
    }

    fn substitute_block(block: &Block, mapping: &std::collections::HashMap<String, Type>) -> Block {
        Block {
            stmts: block
                .stmts
                .iter()
                .map(|s| Self::substitute_statement(s, mapping))
                .collect(),
        }
    }

    fn substitute_statement(
        stmt: &Statement,
        mapping: &std::collections::HashMap<String, Type>,
    ) -> Statement {
        match stmt {
            Statement::Let {
                name,
                destruct_names,
                ty,
                init,
                is_mut,
            } => Statement::Let {
                name: name.clone(),
                destruct_names: destruct_names.clone(),
                ty: ty.as_ref().map(|t| Self::substitute_type(t, mapping)),
                init: Self::substitute_expression(init, mapping),
                is_mut: *is_mut,
            },
            Statement::Expr(e) => Statement::Expr(Self::substitute_expression(e, mapping)),
            Statement::Return(Some(e)) => {
                Statement::Return(Some(Self::substitute_expression(e, mapping)))
            }
            Statement::Return(None) => Statement::Return(None),
            Statement::If {
                condition,
                then_branch,
                else_branch,
            } => Statement::If {
                condition: Self::substitute_expression(condition, mapping),
                then_branch: Self::substitute_block(then_branch, mapping),
                else_branch: else_branch
                    .as_ref()
                    .map(|b| Self::substitute_block(b, mapping)),
            },
            Statement::While { condition, body } => Statement::While {
                condition: Self::substitute_expression(condition, mapping),
                body: Self::substitute_block(body, mapping),
            },
            Statement::For {
                var_name,
                start,
                end,
                body,
            } => Statement::For {
                var_name: var_name.clone(),
                start: Self::substitute_expression(start, mapping),
                end: Self::substitute_expression(end, mapping),
                body: Self::substitute_block(body, mapping),
            },
            Statement::Break => Statement::Break,
            Statement::Continue => Statement::Continue,
        }
    }

    fn substitute_expression(
        expr: &Expression,
        mapping: &std::collections::HashMap<String, Type>,
    ) -> Expression {
        match expr {
            Expression::Identifier(_) | Expression::Literal(_) | Expression::EnumVariant { .. } => {
                expr.clone()
            }
            Expression::Binary { left, op, right } => Expression::Binary {
                left: Box::new(Self::substitute_expression(left, mapping)),
                op: op.clone(),
                right: Box::new(Self::substitute_expression(right, mapping)),
            },
            Expression::Call {
                func,
                args,
                type_args,
            } => Expression::Call {
                func: Box::new(Self::substitute_expression(func, mapping)),
                args: args
                    .iter()
                    .map(|a| Self::substitute_expression(a, mapping))
                    .collect(),
                type_args: type_args
                    .iter()
                    .map(|t| Self::substitute_type(t, mapping))
                    .collect(),
            },
            Expression::StructInit {
                name,
                fields,
                type_args,
            } => Expression::StructInit {
                name: name.clone(),
                fields: fields
                    .iter()
                    .map(|(n, e)| (n.clone(), Self::substitute_expression(e, mapping)))
                    .collect(),
                type_args: type_args
                    .iter()
                    .map(|t| Self::substitute_type(t, mapping))
                    .collect(),
            },
            Expression::GenericInst { target, type_args } => Expression::GenericInst {
                target: Box::new(Self::substitute_expression(target, mapping)),
                type_args: type_args
                    .iter()
                    .map(|t| Self::substitute_type(t, mapping))
                    .collect(),
            },
            Expression::FieldAccess { expr, field } => Expression::FieldAccess {
                expr: Box::new(Self::substitute_expression(expr, mapping)),
                field: field.clone(),
            },
            Expression::Index { expr, index } => Expression::Index {
                expr: Box::new(Self::substitute_expression(expr, mapping)),
                index: Box::new(Self::substitute_expression(index, mapping)),
            },
            Expression::Match { value, arms } => Expression::Match {
                value: Box::new(Self::substitute_expression(value, mapping)),
                arms: arms
                    .iter()
                    .map(|a| MatchArm {
                        pattern: a.pattern.clone(),
                        body: Self::substitute_expression(&a.body, mapping),
                    })
                    .collect(),
            },
            Expression::Closure { params, body } => Expression::Closure {
                params: params
                    .iter()
                    .map(|p| Param {
                        name: p.name.clone(),
                        ty: Self::substitute_type(&p.ty, mapping),
                    })
                    .collect(),
                body: Box::new(Self::substitute_block(body, mapping)),
            },
            Expression::Try(inner) => {
                Expression::Try(Box::new(Self::substitute_expression(inner, mapping)))
            }
        }
    }
}
