use serde::Serialize;
use swiftsc_frontend::ast::{ContractMember, Item, Program, Type};

#[derive(Serialize)]
pub struct ContractAbi {
    pub name: String,
    pub functions: Vec<FunctionAbi>,
}

#[derive(Serialize)]
pub struct FunctionAbi {
    pub name: String,
    pub inputs: Vec<ParamAbi>,
    pub outputs: Vec<TypeAbi>,
    pub is_init: bool,
}

#[derive(Serialize)]
pub struct ParamAbi {
    pub name: String,
    pub ty: String,
}

#[derive(Serialize)]
pub struct TypeAbi {
    pub ty: String,
}

pub fn generate_abi(program: &Program) -> Vec<ContractAbi> {
    let mut abis = Vec::new();

    for item in &program.items {
        if let Item::Contract(contract) = item {
            let mut functions = Vec::new();

            for member in &contract.members {
                match member {
                    ContractMember::Function(func) if func.is_pub => {
                        functions.push(FunctionAbi {
                            name: func.name.clone(),
                            inputs: func
                                .params
                                .iter()
                                .map(|p| ParamAbi {
                                    name: p.name.clone(),
                                    ty: format_type(&p.ty),
                                })
                                .collect(),
                            outputs: func
                                .return_type
                                .as_ref()
                                .map(|t| vec![TypeAbi { ty: format_type(t) }])
                                .unwrap_or_default(),
                            is_init: false,
                        });
                    }
                    ContractMember::Init(func) => {
                        functions.push(FunctionAbi {
                            name: "init".to_string(),
                            inputs: func
                                .params
                                .iter()
                                .map(|p| ParamAbi {
                                    name: p.name.clone(),
                                    ty: format_type(&p.ty),
                                })
                                .collect(),
                            outputs: Vec::new(),
                            is_init: true,
                        });
                    }
                    _ => {}
                }
            }

            abis.push(ContractAbi {
                name: contract.name.clone(),
                functions,
            });
        }
    }

    abis
}

fn format_type(ty: &Type) -> String {
    match ty {
        Type::Path(s) => s.clone(),
        Type::Generic(s, args) => format!(
            "{}<{}>",
            s,
            args.iter().map(format_type).collect::<Vec<_>>().join(", ")
        ),
        Type::Map(k, v) => format!("Map<{}, {}>", format_type(k), format_type(v)),
    }
}
