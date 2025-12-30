# SwiftSC-Lang V1.0.2-beta - Known Limitations

## ⚠️ Beta Release Notice

SwiftSC-Lang V1.0.2-beta is a **non-stable prototype** release. While it introduces significant features like a Module System and Error Handling, it is still intended for **educational and testing purposes only**. Do not use for production smart contracts or mainnet deployments.

## ✅ What's New in V1.0.2-beta

- **File-based Module System**: Support for `use` declarations and multi-file project structures using the `--root` flag.
- **Ergonomic Error Handling**: Implementation of `Result<T, E>` and the `?` operator for clean early-returns and error propagation.
- **Advanced Storage**: Early support for storage-aware `HashMap` and shared ownership logic.
- **Improved Semantic Analysis**: Better circular dependency detection and symbol resolution across modules.

## 🚧 Current Limitations

### 1. Type System Constraints
- **Recursive Structs**: Structs cannot yet contain instances of themselves directly (even via references in some cases).
- **Generic Depth**: Deeply nested generic types may encounter compilation issues in the current backend.
- **Floating Point**: Native floating-point arithmetic is intentionally restricted to ensure determinism, but soft-float libraries are not yet integrated.

### 2. Module System Edge Cases
- **Circular Imports**: While basic detection exists, complex circular dependency chains across more than 3 files may result in non-obvious compiler errors.
- **Visibility**: All functions are currently `pub` by default in terms of WASM exports, though the `pub` keyword is being refined for module-level visibility.

### 3. Error Handling
- **Custom Error Types**: While `Result<T, E>` is supported, custom error types used with `?` must currently match exactly (no automatic conversion via `From` traits yet).
- **String Errors**: String-based error messages in `Result` are heap-allocated and have significant gas overhead.

### 4. Tooling & Optimization
- **LSP Support**: The Language Server is in its early stages and may not resolve symbols from imported modules correctly in all IDEs.
- **Gas Metering**: While injected, the gas costs for complex memory operations (like `HashMap` resizing) are still being calibrated.

## 🚀 Future Roadmap

- [ ] Support for `Vec<T>` and advanced slices.
- [ ] Integration with SMT solvers for formal verification.
- [ ] Optimized gas accounting for high-performance contracts.
- [ ] Support for more WASM runtimes beyond `wasmtime` and `wasmi`.

---

**Report Bugs**: Please use the [Beta Bug Report Template](.github/ISSUE_TEMPLATE/beta_bug_report.md) for any issues encountered.
