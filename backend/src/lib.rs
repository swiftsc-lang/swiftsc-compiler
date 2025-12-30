pub mod codegen;
pub mod gas;
pub mod wasm;

use anyhow::Result;
use swiftsc_frontend::ast::Program;

pub use gas::{GasConfig, GasCosts, GasMeter};

pub fn compile(program: &Program) -> Result<Vec<u8>> {
    let generator = codegen::CodeGenerator::new()?;
    generator.compile_program(program)
}
#[cfg(test)]
mod tests {
    
    use wasmparser::{Parser, Payload};

    #[test]
    fn test_wasm_disasm() {
        println!("RUNNING WASM DISASM TEST");
        let hex = "0061736d01000000012c0960017e017e60027e7e006000017e60027e7e0060017e017e60017e0060027e7e017e60017e017e60017e00024a0403656e760c73746f726167655f72656164000003656e760d73746f726167655f7772697465000103656e760a6765745f63616c6c6572000203656e760a656d69745f6576656e740003030605040506070804010005040101010a0607017e014280080b074407066d656d6f7279020008686561705f7074720300066d616c6c6f63000404696e69740005087472616e7366657200060a6d696e745f746f6b656e0007046275726e00080901000aa801051101017e23002101200120007c240020010b1d01117e42002000100142001a1002210142022001852000100142001a0b4b01127e100221024202200285100021032003200154ada704400f0b42022002854202200285100020017d100142001a42022000854202200085100020017c100142001a4101ac1a42000f0b1301107e4208100421012001200037030020010b1601117e2000210142004200100020017d100142001a0b";
        let bytes = (0..hex.len())
            .step_by(2)
            .map(|i| u8::from_str_radix(&hex[i..i + 2], 16).unwrap())
            .collect::<Vec<u8>>();

        let parser = Parser::new(0);
        let mut func_idx = 0;
        use std::io::Write;
        let mut file = std::fs::File::create("/tmp/wasm_disasm.txt").unwrap();
        for payload in parser.parse_all(&bytes) {
            let p = payload.unwrap();
            writeln!(file, "Payload: {:?}", p).unwrap();
            if let Payload::CodeSectionEntry(body) = p {
                writeln!(file, "Function {}: range {:?}", func_idx, body.range()).unwrap();
                let reader = body.get_operators_reader().unwrap();
                for item in reader.into_iter_with_offsets() {
                    let (op, offset) = item.unwrap();
                    writeln!(file, "  {:04x}: {:?}", offset, op).unwrap();
                }
                func_idx += 1;
            }
        }
    }
}
