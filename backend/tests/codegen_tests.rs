use swiftsc_backend::compile;
use swiftsc_frontend::{analyze, parse};
use wasmtime::*;

fn setup_linker(engine: &Engine) -> Linker<()> {
    let mut linker = Linker::new(engine);
    linker
        .func_wrap("env", "get_caller", || -> i64 { 0 })
        .unwrap();
    linker
        .func_wrap("env", "get_value", || -> i64 { 0 })
        .unwrap();
    linker
        .func_wrap("env", "get_data", || -> i64 { 0 })
        .unwrap();
    linker
        .func_wrap("env", "storage_read", |_key: i64| -> i64 { 0 })
        .unwrap();
    linker
        .func_wrap("env", "storage_write", |_key: i64, _value: i64| {})
        .unwrap();
    linker
        .func_wrap("env", "emit_event", |_event_id: i64, _data: i64| {})
        .unwrap();
    linker
        .func_wrap("env", "hash_i64", |_a: i64, _b: i64| -> i64 { 0 })
        .unwrap();
    linker
}

#[test]
fn test_compile_and_run_arithmetic() {
    let source = "contract Test {
        pub fn add(a: u64, b: u64) -> u64 {
            return a + b;
        }
    }";

    let ast = parse(source).expect("Parse failed");
    analyze(&ast, None).expect("Analysis failed");
    let wasm_bytes = compile(&ast).expect("Compilation failed");

    let engine = Engine::default();
    let module = Module::new(&engine, &wasm_bytes).expect("Invalid WASM module");

    let linker = setup_linker(&engine);
    let mut store = Store::new(&engine, ());
    let instance = linker
        .instantiate(&mut store, &module)
        .expect("Failed to instantiate");

    let add = instance
        .get_typed_func::<(i64, i64), i64>(&mut store, "add")
        .expect("Function not found");
    let result = add.call(&mut store, (10, 20)).expect("Execution failed");

    assert_eq!(result, 30);
}

#[test]
fn test_function_call() {
    let source = "contract Test {
        pub fn double(x: u64) -> u64 {
            return x + x;
        }
        
        pub fn quadruple(x: u64) -> u64 {
            return double(double(x));
        }
    }";

    let ast = parse(source).expect("Parse failed");
    analyze(&ast, None).expect("Analysis failed");
    let wasm_bytes = compile(&ast).expect("Compilation failed");

    let engine = Engine::default();
    let module = Module::new(&engine, &wasm_bytes).expect("Invalid WASM module");

    let linker = setup_linker(&engine);
    let mut store = Store::new(&engine, ());
    let instance = linker
        .instantiate(&mut store, &module)
        .expect("Failed to instantiate");

    let quadruple = instance
        .get_typed_func::<i64, i64>(&mut store, "quadruple")
        .expect("Function not found");
    let result = quadruple.call(&mut store, 5).expect("Execution failed");

    assert_eq!(result, 20);
}

#[test]
fn test_let_binding() {
    let source = "contract Test {
        pub fn compute(x: u64) -> u64 {
            let y: u64 = x + 10;
            let z: u64 = y * 2;
            return z;
        }
    }";

    let ast = parse(source).expect("Parse failed");
    analyze(&ast, None).expect("Analysis failed");
    let wasm_bytes = compile(&ast).expect("Compilation failed");

    let engine = Engine::default();
    let module = Module::new(&engine, &wasm_bytes).expect("Invalid WASM module");

    let linker = setup_linker(&engine);
    let mut store = Store::new(&engine, ());
    let instance = linker
        .instantiate(&mut store, &module)
        .expect("Failed to instantiate");

    let compute = instance
        .get_typed_func::<i64, i64>(&mut store, "compute")
        .expect("Function not found");
    let result = compute.call(&mut store, 5).expect("Execution failed");

    assert_eq!(result, 30);
}

#[test]
fn test_host_function_import() {
    let source = "contract Test {
        pub fn get_sender() -> u64 {
            return msg.sender;
        }
    }";

    let ast = parse(source).expect("Parse failed");
    analyze(&ast, None).expect("Analysis failed");
    let wasm_bytes = compile(&ast).expect("Compilation failed");

    let engine = Engine::default();
    let module = Module::new(&engine, &wasm_bytes).expect("Invalid WASM module");

    let mut linker = Linker::new(&engine);
    linker
        .func_wrap("env", "get_caller", || -> i64 {
            0x1234567890ABCDEF_i64
        })
        .unwrap();
    linker
        .func_wrap("env", "get_value", || -> i64 { 0 })
        .unwrap();
    linker
        .func_wrap("env", "get_data", || -> i64 { 0 })
        .unwrap();
    linker
        .func_wrap("env", "storage_read", |_key: i64| -> i64 { 0 })
        .unwrap();
    linker
        .func_wrap("env", "storage_write", |_key: i64, _value: i64| {})
        .unwrap();
    linker
        .func_wrap("env", "emit_event", |_event_id: i64, _data: i64| {})
        .unwrap();
    linker
        .func_wrap("env", "hash_i64", |_a: i64, _b: i64| -> i64 { 0 })
        .unwrap();

    let mut store = Store::new(&engine, ());
    let instance = linker
        .instantiate(&mut store, &module)
        .expect("Failed to instantiate");

    let get_sender = instance
        .get_typed_func::<(), i64>(&mut store, "get_sender")
        .expect("Function not found");
    let result = get_sender.call(&mut store, ()).expect("Execution failed");

    assert_eq!(result, 0x1234567890ABCDEF_i64);
}
