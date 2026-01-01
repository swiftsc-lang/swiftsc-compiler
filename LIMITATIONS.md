# SwiftSC-Lang V1.0.3-beta - Known Limitations

## ⚠️ Beta Release Notice

SwiftSC-Lang V1.0.3-beta is a **Security Hardening** release. While it introduces critical features like Gas Metering, Reentrancy Protection, and a richer Standard Library, it is still intended for **educational and testing purposes only**. Do not use for production smart contracts or mainnet deployments without a comprehensive audit.

## ✅ What's New in V1.0.3-beta

- **Gas Instrumentation**: Automatic injection of gas metering instructions into WASM output to prevent infinite loops and resource exhaustion (Issue #24).
- **Security Analysis**: Static detection of Reentrancy vulnerabilities, Integer Overflows, and Uninitialized Storage variables (Issues #2, #3, #4).
- **Standard Library**: implementation of `SafeMath` for secure arithmetic and `Collections` (Vectors/Maps) for data management.
- **Enhanced IDE Support**: The Language Server Protocol (LSP) now supports Hover documentation and real-time Diagnostics from the analyzer.

## 🚧 Current Limitations

### 1. Type System Constraints
- **Recursive Structs**: Structs cannot yet contain instances of themselves directly (even via references in some cases).
- **Generic Depth**: Deeply nested generic types may encounter compilation issues in the current backend.
- **Floating Point**: Native floating-point arithmetic is intentionally restricted to ensure determinism.

### 2. Module System Edge Cases
- **Circular Imports**: While basic detection exists, complex circular dependency chains across more than 3 files may result in non-obvious compiler errors.
- **Visibility**: All functions are currently `pub` by default in terms of WASM exports, though the `pub` keyword is being refined for module-level visibility.

### 3. Error Handling
- **Custom Error Types**: While `Result<T, E>` is supported, custom error types used with `?` must currently match exactly (no automatic conversion via `From` traits yet).

### 4. Tooling & Optimization
- **Gas Calibration**: While gas metering is functional, the exact cost table for standard library operations (like `Vector` resizing) is still being fine-tuned for mainnet equivalence.
- **Static Analysis Limits**: Reentrancy detection is static; complex dynamic dispatch calls may still evade detection.

## 🚀 Future Roadmap

- [ ] Full audited Gas Cost Table.
- [ ] Integration with SMT solvers for formal verification.
- [ ] Optimizing `Vector` memory layout for lower gas usage.
- [ ] Support for more WASM runtimes beyond `wasmtime` and `wasmi`.

---

**Report Bugs**: Please use the [Beta Bug Report Template](.github/ISSUE_TEMPLATE/beta_bug_report.md) for any issues encountered.
