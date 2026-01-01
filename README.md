# ⚙️ SwiftSC Compiler

![Version](https://img.shields.io/badge/version-1.0.3--beta-blue)
![Build Status](https://github.com/swiftsc-lang/swiftsc-compiler/actions/workflows/ci.yml/badge.svg)

The official compiler for the SwiftSC smart contract language, comprising the frontend, backend, and driver components.

## 🏗️ Components

- **swiftsc-frontend**: Lexer, parser, and semantic analyzer supporting modules.
- **swiftsc-backend**: WASM code generator with ARC and gas metering.
- **swiftsc-driver**: The unified CLI tool (`swiftsc`) for building and testing.

## 🚀 Features

- **Module Support**: Resolves and compiles multi-file projects.
- **Error Handling**: Native support for `Result` and the `?` operator.
- **Optimization**: Produces lean, efficient WebAssembly.

## 🛠️ Development

```bash
git clone https://github.com/swiftsc-lang/swiftsc-compiler
cd swiftsc-compiler
cargo build --release
cargo test --all
```

## 🤝 Contributing

Please see the [CONTRIBUTING.md](CONTRIBUTING.md) file in the root directory.

## 📄 License

MIT
