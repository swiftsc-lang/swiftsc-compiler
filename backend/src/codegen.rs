use crate::wasm::WasmModule;
use anyhow::Result;
use swiftsc_frontend::ast::*;
use wasm_encoder::{Function as WasmFunction, Instruction, ValType};

pub struct CodeGenerator {
    module: WasmModule,
    function_map: std::collections::HashMap<String, u32>,
}

impl Default for CodeGenerator {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeGenerator {
    pub fn new() -> Self {
        let mut module = WasmModule::new();

        // Import blockchain host functions
        // get_caller() -> Address (represented as i64 for MVP)
        let get_caller_type = module.add_type(vec![], vec![ValType::I64]);
        let get_caller_idx = module.import_function("env", "get_caller", get_caller_type);

        // storage_read(key: i64) -> i64
        let storage_read_type = module.add_type(vec![ValType::I64], vec![ValType::I64]);
        let storage_read_idx = module.import_function("env", "storage_read", storage_read_type);

        // storage_write(key: i64, value: i64)
        let storage_write_type = module.add_type(vec![ValType::I64, ValType::I64], vec![]);
        let storage_write_idx = module.import_function("env", "storage_write", storage_write_type);

        // emit_event(event_id: i64, data: i64)
        let emit_event_type = module.add_type(vec![ValType::I64, ValType::I64], vec![]);
        let emit_event_idx = module.import_function("env", "emit_event", emit_event_type);

        // Always export memory
        module.add_memory(1, None);

        let mut function_map = std::collections::HashMap::new();
        // Register host functions in the function map
        function_map.insert("get_caller".to_string(), get_caller_idx);
        function_map.insert("storage_read".to_string(), storage_read_idx);
        function_map.insert("storage_write".to_string(), storage_write_idx);
        function_map.insert("emit_event".to_string(), emit_event_idx);

        CodeGenerator {
            module,
            function_map,
        }
    }

    pub fn compile_program(mut self, program: &Program) -> Result<Vec<u8>> {
        for item in &program.items {
            match item {
                Item::Contract(c) => self.compile_contract(c)?,
                Item::Function(f) => self.compile_function(f)?,
                Item::Dummy => {}
            }
        }
        Ok(self.module.finish())
    }

    fn compile_contract(&mut self, contract: &Contract) -> Result<()> {
        // Compile members (functions/init)
        for member in &contract.members {
            if let ContractMember::Function(f) = member {
                self.compile_function(f)?;
            } else if let ContractMember::Init(f) = member {
                self.compile_function(f)?;
            }
        }
        Ok(())
    }

    fn compile_function(&mut self, func: &Function) -> Result<()> {
        // Define Type (Signature)
        let params: Vec<ValType> = func.params.iter().map(|_| ValType::I64).collect();
        let results: Vec<ValType> = if func.return_type.is_some() {
            vec![ValType::I64]
        } else {
            vec![]
        };

        let type_idx = self.module.add_type(params, results);
        let func_idx = self.module.add_function(type_idx);

        // Store function index for call resolution
        self.function_map.insert(func.name.clone(), func_idx);

        self.module.export_function(&func.name, func_idx);

        // Locals mapping - params come first
        let mut locals: std::collections::HashMap<String, u32> = std::collections::HashMap::new();
        for (i, param) in func.params.iter().enumerate() {
            locals.insert(param.name.clone(), i as u32);
        }

        // Count additional local variables needed (beyond params)
        let num_locals = self.count_locals(&func.body);

        // Declare locals in WASM function
        // All locals are i64 for MVP
        let local_types = vec![(num_locals, ValType::I64)];
        let mut body = WasmFunction::new(local_types);

        for stmt in &func.body.stmts {
            self.compile_stmt(stmt, &mut body, &mut locals)?;
        }

        body.instruction(&Instruction::End);
        self.module.add_code(body);

        Ok(())
    }

    fn count_locals(&self, block: &Block) -> u32 {
        let mut count = 0;
        for stmt in &block.stmts {
            if let Statement::Let { destruct_names, .. } = stmt {
                if destruct_names.is_empty() {
                    count += 1;
                } else {
                    count += destruct_names.len() as u32;
                }
            }
        }
        count
    }

    fn compile_stmt(
        &mut self,
        stmt: &Statement,
        body: &mut WasmFunction,
        locals: &mut std::collections::HashMap<String, u32>,
    ) -> Result<()> {
        match stmt {
            Statement::Let {
                name,
                destruct_names,
                ty: _,
                init,
                is_mut: _,
            } => {
                // Compile the initializer expression
                self.compile_expr(init, body, locals)?;

                // Calculate local index (params + existing locals)
                // Find the next available local index
                let local_idx = locals.values().max().map(|&v| v + 1).unwrap_or(0);

                // If no destructuring, simple case
                if destruct_names.is_empty() {
                    locals.insert(name.clone(), local_idx);
                    body.instruction(&Instruction::LocalSet(local_idx));
                } else {
                    // For destructuring, bind each name
                    for (i, dname) in destruct_names.iter().enumerate() {
                        let idx = local_idx + i as u32;
                        locals.insert(dname.clone(), idx);
                        if i == 0 {
                            body.instruction(&Instruction::LocalSet(idx));
                        }
                    }
                }
            }
            Statement::Expr(expr) => {
                self.compile_expr(expr, body, locals)?;
                // If expression. assumed i64 result need drop
                body.instruction(&Instruction::Drop);
            }
            Statement::Return(Some(expr)) => {
                self.compile_expr(expr, body, locals)?;
                body.instruction(&Instruction::Return);
            }
            Statement::Return(None) => {
                body.instruction(&Instruction::Return);
            }
        }
        Ok(())
    }

    fn compile_expr(
        &mut self,
        expr: &Expression,
        body: &mut WasmFunction,
        locals: &mut std::collections::HashMap<String, u32>,
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
                _ => {}
            },
            Expression::Identifier(name) => {
                if let Some(idx) = locals.get(name) {
                    body.instruction(&Instruction::LocalGet(*idx));
                } else {
                    // Fail or assume global? Fail for now
                    // Or check params? Params are in locals.
                    // If not found, panic or error (sem analysis should have caught, but for codegen integrity...)
                }
            }
            Expression::Binary { left, op, right } => {
                self.compile_expr(left, body, locals)?;
                self.compile_expr(right, body, locals)?;
                match op {
                    BinaryOp::Add => {
                        body.instruction(&Instruction::I64Add);
                    }
                    BinaryOp::Sub => {
                        body.instruction(&Instruction::I64Sub);
                    }
                    BinaryOp::Mul => {
                        body.instruction(&Instruction::I64Mul);
                    }
                    _ => {}
                }
            }
            Expression::Call { func, args } => {
                // Compile arguments first (left to right)
                for arg in args {
                    self.compile_expr(arg, body, locals)?;
                }

                // Resolve function name
                if let Expression::Identifier(func_name) = &**func {
                    if let Some(&func_idx) = self.function_map.get(func_name) {
                        body.instruction(&Instruction::Call(func_idx));
                    } else {
                        // Function not found - semantic analysis should have caught this
                    }
                } else {
                    // Complex function expressions not supported yet
                }
            }
            Expression::FieldAccess { expr, field } => {
                // Handle blockchain intrinsics like msg.sender
                if let Expression::Identifier(obj_name) = &**expr {
                    if obj_name == "msg" && field == "sender" {
                        // Call get_caller host function
                        if let Some(&func_idx) = self.function_map.get("get_caller") {
                            body.instruction(&Instruction::Call(func_idx));
                        }
                    } else {
                        // Other field access not yet supported
                    }
                } else {
                    // Complex field access not yet supported
                }
            }
            _ => {}
        }
        Ok(())
    }
}
