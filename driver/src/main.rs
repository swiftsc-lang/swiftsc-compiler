use anyhow::{Context, Result};
use clap::{Parser, Subcommand};
use std::path::PathBuf;
use swiftsc_frontend::{parse, tokenize};

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
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
                Ok(ast) => match swiftsc_frontend::analyze(&ast) {
                    Ok(_) => println!("Semantic Check Passed"),
                    Err(e) => eprintln!("Semantic Error: {}", e),
                },
                Err(e) => eprintln!("Parse Error: {}", e),
            }
        }
        Commands::Build { path, output } => {
            let content = std::fs::read_to_string(&path)
                .with_context(|| format!("could not read file `{}`", path.display()))?;

            println!("--- Compiling: {} ---", path.display());
            match parse(&content) {
                Ok(ast) => match swiftsc_frontend::analyze(&ast) {
                    Ok(_) => match swiftsc_backend::compile(&ast) {
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

            // First, build the contract
            let content = std::fs::read_to_string(&path)?;
            match parse(&content) {
                Ok(ast) => match swiftsc_frontend::analyze(&ast) {
                    Ok(_) => match swiftsc_backend::compile(&ast) {
                        Ok(wasm_bytes) => {
                            println!("✓ Contract compiled ({} bytes)", wasm_bytes.len());
                            println!("  (Deployment to {} not yet implemented)", network);
                        }
                        Err(e) => eprintln!("✗ Compilation failed: {}", e),
                    },
                    Err(e) => eprintln!("✗ Semantic error: {}", e),
                },
                Err(e) => eprintln!("✗ Parse error: {}", e),
            }
        }
    }

    Ok(())
}
