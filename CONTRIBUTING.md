# Contributing to SwiftSC (swiftsc-compiler)

Thank you for your interest in contributing to the SwiftSC ecosystem! We welcome contributions from developers of all skill levels.

## Code of Conduct

Please note that this project is released with a Contributor Code of Conduct. By participating in this project you agree to abide by its terms.

## Compiler Development
*   **Frontend**: Handling parsing and AST generation.
*   **Backend**: WASM code generation and gas instrumentation.
*   **Tests**: Add new syntax tests in `tests/compiler_tests.rs`.

## How to Contribute

1.  **Fork the repository** on GitHub.
2.  **Clone your fork** locally.
3.  **Create a new branch** for your feature or bugfix.
4.  **Make your changes**, ensuring you follow the project's coding standards.
5.  **Run tests** using `cargo test`.
6.  **Submit a Pull Request** to the `main` branch.

## Coding Standards

*   **Rust**: We use `rustfmt` and `clippy`. Please ensure your code passes `cargo fmt --check` and `cargo clippy`.
*   **Commits**: Use conventional commits (e.g., `feat: add new parser`, `fix: resolve memory leak`).

## Reporting Bugs

Please use the [Bug Report Template](.github/ISSUE_TEMPLATE/beta_bug_report.md) when opening an issue.
