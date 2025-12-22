pub mod codegen;
pub mod gas;
pub mod wasm;

use anyhow::Result;
use swiftsc_frontend::ast::Program;

pub use gas::{GasConfig, GasCosts, GasMeter};

pub fn compile(program: &Program) -> Result<Vec<u8>> {
    let generator = codegen::CodeGenerator::new();
    generator.compile_program(program)
}
