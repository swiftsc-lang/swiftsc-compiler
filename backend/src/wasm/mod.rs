use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, ElementSection, Elements, EntityType, ExportKind,
    ExportSection, FunctionSection, GlobalSection, GlobalType, ImportSection, MemorySection,
    MemoryType, Module, RefType, TableSection, TableType, TypeSection, ValType,
};

pub struct WasmModule {
    types: TypeSection,
    imports: ImportSection,
    funcs: FunctionSection,
    code: CodeSection,
    exports: ExportSection,
    memories: MemorySection,
    globals: GlobalSection,
    tables: TableSection,
    elements: ElementSection,
    data: DataSection,
    num_imported_funcs: u32,
    table_funcs: Vec<u32>,
    data_offset: u32,
}

impl Default for WasmModule {
    fn default() -> Self {
        Self::new()
    }
}

impl WasmModule {
    pub fn new() -> Self {
        WasmModule {
            types: TypeSection::new(),
            imports: ImportSection::new(),
            funcs: FunctionSection::new(),
            code: CodeSection::new(),
            exports: ExportSection::new(),
            memories: MemorySection::new(),
            globals: GlobalSection::new(),
            tables: TableSection::new(),
            elements: ElementSection::new(),
            data: DataSection::new(),
            num_imported_funcs: 0,
            table_funcs: Vec::new(),
            data_offset: 0,
        }
    }

    pub fn add_type<P, R>(&mut self, params: P, results: R) -> u32
    where
        P: IntoIterator<Item = ValType>,
        P::IntoIter: ExactSizeIterator,
        R: IntoIterator<Item = ValType>,
        R::IntoIter: ExactSizeIterator,
    {
        self.types.function(params, results);
        self.types.len() - 1
    }

    pub fn import_function(&mut self, module: &str, name: &str, type_index: u32) -> u32 {
        self.imports
            .import(module, name, EntityType::Function(type_index));
        let func_idx = self.num_imported_funcs;
        self.num_imported_funcs += 1;
        func_idx
    }

    pub fn add_function(&mut self, type_index: u32) -> u32 {
        self.funcs.function(type_index);
        // Function indices start after imported functions
        self.num_imported_funcs + (self.funcs.len() - 1)
    }

    pub fn add_code(&mut self, func_body: wasm_encoder::Function) {
        self.code.function(&func_body);
    }

    pub fn export_function(&mut self, name: &str, func_index: u32) {
        self.exports.export(name, ExportKind::Func, func_index);
    }

    pub fn add_memory(&mut self, min_pages: u64, max_pages: Option<u64>) {
        self.memories.memory(MemoryType {
            minimum: min_pages,
            maximum: max_pages,
            memory64: false,
            shared: false,
        });
        self.exports.export("memory", ExportKind::Memory, 0);
    }

    pub fn add_global(&mut self, global_type: GlobalType, init_expr: &ConstExpr) -> u32 {
        self.globals.global(global_type, init_expr);
        self.globals.len() - 1
    }

    pub fn export_global(&mut self, name: &str, global_index: u32) {
        self.exports.export(name, ExportKind::Global, global_index);
    }

    pub fn add_table(&mut self, min: u32, max: Option<u32>) {
        self.tables.table(TableType {
            element_type: RefType::FUNCREF,
            minimum: min,
            maximum: max,
        });
    }

    pub fn add_func_to_table(&mut self, func_index: u32) -> u32 {
        let table_idx = self.table_funcs.len() as u32;
        self.table_funcs.push(func_index);
        table_idx
    }

    fn finalize_table(&mut self) {
        if !self.table_funcs.is_empty() {
            // Add a single table if not already added
            if self.tables.is_empty() {
                self.add_table(self.table_funcs.len() as u32, None);
            }

            // Create elements entry
            self.elements.active(
                None, // Table 0
                &ConstExpr::i32_const(0),
                Elements::Functions(&self.table_funcs),
            );
        }
    }

    pub fn add_data_segment(&mut self, data: Vec<u8>) -> u32 {
        let offset = self.data_offset;
        let len = data.len() as u32;
        self.data.active(
            0, // Memory index 0
            &ConstExpr::i32_const(offset as i32),
            data,
        );
        self.data_offset += len;
        offset
    }

    pub fn finish(mut self) -> Vec<u8> {
        self.finalize_table();
        let mut module = Module::new();
        module.section(&self.types);
        module.section(&self.imports);
        module.section(&self.funcs);
        module.section(&self.tables);
        module.section(&self.memories);
        module.section(&self.globals);
        module.section(&self.exports);
        module.section(&self.elements);
        module.section(&self.code);
        module.section(&self.data);
        module.finish()
    }
}
