use swiftsc_frontend::ast::Type;
use swiftsc_frontend::sema::symbol_table::SymbolTable;

#[test]
fn test_symbol_table_scope() {
    let mut table = SymbolTable::new();

    // Define in global
    table
        .define("x".to_string(), Type::Path("u64".into()), false)
        .unwrap();
    assert!(table.resolve("x").is_some());

    // Enter scope
    table.enter_scope();
    table
        .define("y".to_string(), Type::Path("bool".into()), false)
        .unwrap();
    assert!(table.resolve("x").is_some()); // Can see outer
    assert!(table.resolve("y").is_some()); // Can see inner

    // Shadowing allowed? Implementation allows it (insert checks current scope only)
    table
        .define("x".to_string(), Type::Path("bool".into()), false)
        .unwrap();
    let sym = table.resolve("x").unwrap();
    if let Type::Path(p) = &sym.ty {
        assert_eq!(p, "bool");
    } else {
        panic!("Expected bool");
    }

    // Exit scope
    table.exit_scope();
    assert!(table.resolve("x").is_some());
    // y should be gone
    assert!(table.resolve("y").is_none());
}
