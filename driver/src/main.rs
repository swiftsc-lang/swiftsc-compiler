use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use std::path::PathBuf;
use swiftsc_frontend::{parse, tokenize};

mod abi;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
    #[arg(short, long, global = true)]
    root: Option<PathBuf>,
}

#[derive(Subcommand)]
enum Commands {
    /// Tokenize a source file
    Lex { path: PathBuf },
    /// Parse a source file into AST
    Parse { path: PathBuf },
    /// Parse and check semantics
    Check { path: PathBuf },
    /// Compile to WASM
    Build {
        path: PathBuf,
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
    /// Initialize a new SwiftSC-Lang project
    Init {
        #[arg(default_value = ".")]
        path: PathBuf,
    },
    /// Run tests
    Test {
        #[arg(default_value = ".")]
        path: PathBuf,
    },
    /// Deploy contract to blockchain
    Deploy {
        path: PathBuf,
        #[arg(short, long)]
        network: String,
    },
    /// Simulate contract execution
    Simulate {
        path: PathBuf,
        #[arg(short, long)]
        func: String,
        #[arg(short, long, value_delimiter = ',')]
        args: Vec<i64>,
        #[arg(long)]
        solana: bool,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Lex { path } => {
            let content = std::fs::read_to_string(path)
                .with_context(|| format!("could not read file `{}`", path.display()))?;

            println!("--- Lexing: {} ---", path.display());
            let tokens = tokenize(&content);

            for (token, span) in tokens {
                println!("{:?} => {:?}", span, token);
            }
        }
        Commands::Parse { path } => {
            let content = std::fs::read_to_string(path)
                .with_context(|| format!("could not read file `{}`", path.display()))?;

            println!("--- Parsing: {} ---", path.display());
            match parse(&content) {
                Ok(ast) => println!("{:#?}", ast),
                Err(e) => eprintln!("Error: {}", e),
            }
        }
        Commands::Check { path } => {
            let content = std::fs::read_to_string(path)
                .with_context(|| format!("could not read file `{}`", path.display()))?;

            match parse(&content) {
                Ok(ast) => match swiftsc_frontend::analyze(&ast, cli.root.clone()) {
                    Ok(_) => println!("Semantic Check Passed"),
                    Err(e) => eprintln!("Semantic Error: {}", e),
                },
                Err(e) => eprintln!("Parse Error: {}", e),
            }
        }
        Commands::Build { path, output } => {
            let content = std::fs::read_to_string(path)
                .with_context(|| format!("could not read file `{}`", path.display()))?;

            println!("--- Compiling: {} ---", path.display());
            match parse(&content) {
                Ok(ast) => match swiftsc_frontend::analyze_and_link(&ast, cli.root.clone()) {
                    Ok(linked_ast) => match swiftsc_backend::compile(&linked_ast) {
                        Ok(wasm_bytes) => {
                            let output_path = output
                                .clone()
                                .unwrap_or_else(|| path.with_extension("wasm"));
                            std::fs::write(&output_path, wasm_bytes).with_context(|| {
                                format!("could not write output file `{}`", output_path.display())
                            })?;
                            println!("Build Successful: {}", output_path.display());
                        }
                        Err(e) => eprintln!("Codegen Error: {}", e),
                    },
                    Err(e) => eprintln!("Semantic Error: {}", e),
                },
                Err(e) => eprintln!("Parse Error: {}", e),
            }
        }
        Commands::Init { path } => {
            println!(
                "--- Initializing SwiftSC-Lang project in: {} ---",
                path.display()
            );

            // Create project structure
            std::fs::create_dir_all(path.join("src"))?;
            std::fs::create_dir_all(path.join("tests"))?;

            // Create SwiftSC-Lang.toml
            let config = r#"[package]
name = "my-contract"
version = "0.1.0"

[dependencies]
# Add dependencies here

[build]
target = "wasm32-unknown-unknown"
"#;
            std::fs::write(path.join("SwiftSC-Lang.toml"), config)?;

            // Create example contract
            let example = r#"contract MyContract {
    fn hello() -> u64 {
        return 42;
    }
}
"#;
            std::fs::write(path.join("src/contract.stc"), example)?;

            println!("✓ Project initialized successfully!");
            println!("  - SwiftSC-Lang.toml");
            println!("  - src/contract.stc");
            println!("  - tests/");
        }
        Commands::Test { path } => {
            println!("--- Running tests in: {} ---", path.display());

            // Find all test files
            let test_dir = path.join("tests");
            if test_dir.exists() {
                println!("✓ Test directory found");
                println!("  (Test execution not yet implemented)");
            } else {
                eprintln!("✗ No tests directory found");
            }
        }
        Commands::Deploy { path, network } => {
            println!(
                "--- Deploying contract: {} to {} ---",
                path.display(),
                network
            );

            let content = std::fs::read_to_string(path)?;
            let ast = parse(&content).map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
            let linked_ast = swiftsc_frontend::analyze_and_link(&ast, cli.root.clone())
                .map_err(|e| anyhow::anyhow!("Semantic error: {}", e))?;
            let wasm_bytes = swiftsc_backend::compile(&linked_ast)
                .map_err(|e| anyhow::anyhow!("Codegen error: {}", e))?;

            println!("✓ Contract compiled ({} bytes)", wasm_bytes.len());

            // Generate ABI
            let abis = abi::generate_abi(&linked_ast);
            let abi_json = serde_json::to_string_pretty(&abis)?;

            // Create build directory
            let build_dir = std::env::current_dir()?.join("build");
            std::fs::create_dir_all(&build_dir)?;

            let stem = path.file_stem().unwrap().to_str().unwrap();
            let wasm_path = build_dir.join(format!("{}.wasm", stem));
            let abi_path = build_dir.join(format!("{}.abi.json", stem));

            std::fs::write(&wasm_path, &wasm_bytes)?;
            std::fs::write(&abi_path, &abi_json)?;

            println!("✓ Artifacts saved to:");
            println!("  - {}", wasm_path.display());
            println!("  - {}", abi_path.display());

            if network == "local" {
                println!("✓ Mock deployment to local simulator successful");
                println!("  Contract Address (Mock): 0x666f6f626172");
            } else {
                println!("  (Real deployment to {} not yet implemented)", network);
            }
        }
        Commands::Simulate {
            path,
            func,
            args,
            solana,
        } => {
            let content = std::fs::read_to_string(path)?;
            let ast = parse(&content).map_err(|e| anyhow::anyhow!("Parse error: {}", e))?;
            let linked_ast = swiftsc_frontend::analyze_and_link(&ast, cli.root.clone())
                .map_err(|e| anyhow::anyhow!("Semantic error: {}", e))?;
            let wasm_bytes = swiftsc_backend::compile(&linked_ast)
                .map_err(|e| anyhow::anyhow!("Codegen error: {}", e))?;

            use swiftsc_runtime::{
                BlockchainAdapter, ContractState, SimulatorAdapter, SolanaAccount, SolanaAdapter,
                SolanaContext,
            };

            let state = ContractState {
                storage: std::collections::HashMap::new(),
                caller: 42,  // Mock caller address
                value: 1000, // Mock value
                data: 1234,  // Mock data/input
                events: Vec::new(),
                solana: if *solana {
                    Some(SolanaContext {
                        program_id: 111,
                        accounts: vec![
                            SolanaAccount {
                                address: 222,
                                owner: 111, // Owned by program
                                is_writable: true,
                                ..Default::default()
                            },
                            SolanaAccount {
                                address: 333,
                                owner: 444, // Not owned by program
                                is_writable: false,
                                ..Default::default()
                            },
                        ],
                    })
                } else {
                    None
                },
            };

            println!("--- Simulating: {} ---", path.display());
            println!("Calling: {}({:?})", func, args);

            if *solana {
                let adapter = SolanaAdapter::new();
                match adapter.execute(&wasm_bytes, func, args, state) {
                    Ok((result, final_state)) => {
                        println!("✓ Solana (Conceptual) simulation successful");
                        println!("Result: {}", result);
                        if !final_state.storage.is_empty() {
                            println!("Final Storage: {:#?}", final_state.storage);
                        }
                    }
                    Err(e) => eprintln!("✗ Solana (Conceptual) simulation failed: {}", e),
                }
            } else {
                let adapter = SimulatorAdapter::new();
                match adapter.execute(&wasm_bytes, func, args, state) {
                    Ok((result, final_state)) => {
                        println!("✓ Simulation successful");
                        println!("Result: {}", result);
                        if !final_state.storage.is_empty() {
                            println!("Final Storage: {:#?}", final_state.storage);
                        }
                    }
                    Err(e) => eprintln!("✗ Simulation failed: {}", e),
                }
            }
        }
    }

    Ok(())
}
