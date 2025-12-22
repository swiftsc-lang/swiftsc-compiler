use swiftsc_frontend::ast::*;
use swiftsc_frontend::sema::analyzer::Analyzer;

#[test]
fn test_analyze_valid() {
    // let x: u64 = 10;
    let stmt = Statement::Let {
        name: "x".to_string(),
        destruct_names: vec![],
        ty: Some(Type::Path("u64".to_string())),
        init: Expression::Literal(Literal::Int(10)),
        is_mut: false,
    };
    let block = Block { stmts: vec![stmt] };
    let func = Function {
        name: "test".to_string(),
        params: vec![],
        return_type: None,
        body: block,
        is_pub: false,
    };
    let prog = Program {
        items: vec![Item::Function(func)],
    };

    let mut analyzer = Analyzer::new();
    assert!(analyzer.check_program(&prog).is_ok());
}

#[test]
fn test_analyze_undefined() {
    // let x = y;
    let stmt = Statement::Let {
        name: "x".to_string(),
        destruct_names: vec![],
        ty: None,
        init: Expression::Identifier("y".to_string()),
        is_mut: false,
    };
    let block = Block { stmts: vec![stmt] };
    let func = Function {
        name: "test".to_string(),
        params: vec![],
        return_type: None,
        body: block,
        is_pub: false,
    };
    let prog = Program {
        items: vec![Item::Function(func)],
    };

    let mut analyzer = Analyzer::new();
    let res = analyzer.check_program(&prog);
    assert!(res.is_err());
    assert_eq!(res.unwrap_err().to_string(), "Undefined symbol: y");
}

#[test]
fn test_analyze_type_mismatch() {
    // let x: u64 = true;
    let stmt = Statement::Let {
        name: "x".to_string(),
        destruct_names: vec![],
        ty: Some(Type::Path("u64".to_string())),
        init: Expression::Literal(Literal::Bool(true)),
        is_mut: false,
    };
    let block = Block { stmts: vec![stmt] };
    let func = Function {
        name: "test".to_string(),
        params: vec![],
        return_type: None,
        body: block,
        is_pub: false,
    };
    let prog = Program {
        items: vec![Item::Function(func)],
    };

    let mut analyzer = Analyzer::new();
    let res = analyzer.check_program(&prog);
    assert!(res.is_err());
    // Error string format: TypeMismatch(expected, found)
    // "Type mismatch: expected Path(\"u64\"), found Path(\"bool\")"
    let msg = res.unwrap_err().to_string();
    assert!(msg.contains("Type mismatch"));
    assert!(msg.contains("u64"));
    assert!(msg.contains("bool"));
}
