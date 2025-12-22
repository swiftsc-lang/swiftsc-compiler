use wasm_encoder::{
    CodeSection, EntityType, ExportKind, ExportSection, FunctionSection, ImportSection,
    MemorySection, MemoryType, Module, TypeSection, ValType,
};

pub struct WasmModule {
    types: TypeSection,
    imports: ImportSection,
    funcs: FunctionSection,
    code: CodeSection,
    exports: ExportSection,
    memories: MemorySection,
    num_imported_funcs: u32,
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
            num_imported_funcs: 0,
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
        (self.types.len() - 1)
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
        let func_idx = self.num_imported_funcs + (self.funcs.len() - 1);
        func_idx
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

    pub fn finish(self) -> Vec<u8> {
        let mut module = Module::new();
        module.section(&self.types);
        module.section(&self.imports);
        module.section(&self.funcs);
        module.section(&self.memories);
        module.section(&self.exports);
        module.section(&self.code);
        module.finish()
    }
}
