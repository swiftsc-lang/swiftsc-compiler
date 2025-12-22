# SwiftSC-Lang Compiler

The official compiler for the SwiftSC-Lang smart contract language.

> ⚠️ **v1.0 is an educational/prototype release** - See [LIMITATIONS.md](LIMITATIONS.md)

## Components

- **swiftsc-frontend** - Lexer, parser, semantic analyzer
- **swiftsc-backend** - WASM code generator
- **swiftsc-driver** - CLI tool

## Installation

```bash
cargo install swiftsc-driver
```

## Usage

```bash
swiftsc build contract.stc
swiftsc test
```

## Repository Structure

This is part of the SwiftSC-Lang ecosystem:
- [swiftsc-stdlib](https://github.com/swiftsc-lang/swiftsc-stdlib) - Standard library
- [swiftsc-docs](https://github.com/swiftsc-lang/swiftsc-docs) - Documentation
- [swiftsc-examples](https://github.com/swiftsc-lang/swiftsc-examples) - Examples

## Development

```bash
git clone https://github.com/swiftsc-lang/swiftsc-compiler
cd swiftsc-compiler
cargo build
cargo test
```

## License

MIT
