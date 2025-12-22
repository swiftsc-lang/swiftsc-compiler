# Contributing to SwiftSC-Lang

Thank you for your interest in contributing to SwiftSC-Lang!

## Getting Started

1. Fork the repository
2. Clone your fork: `git clone https://github.com/YOUR_USERNAME/swiftsc.git`
3. Create a branch: `git checkout -b feature/your-feature`
4. Make your changes
5. Run tests: `cargo test`
6. Commit: `git commit -m "feat: your feature"`
7. Push: `git push origin feature/your-feature`
8. Open a Pull Request

## Development Setup

```bash
# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh

# Clone and build
git clone https://github.com/swiftsc-lang/swiftsc.git
cd swiftsc/swiftsc-compiler
cargo build
cargo test
```

## Commit Convention

We use [Conventional Commits](https://www.conventionalcommits.org/):

- `feat:` New feature
- `fix:` Bug fix
- `docs:` Documentation changes
- `test:` Test additions/changes
- `refactor:` Code refactoring
- `chore:` Build/tooling changes

## Pull Request Process

1. Update documentation for any API changes
2. Add tests for new functionality
3. Ensure all tests pass
4. Update CHANGELOG.md
5. Request review from maintainers

## Code Style

- Run `cargo fmt` before committing
- Run `cargo clippy` and fix warnings
- Follow Rust naming conventions
- Add doc comments for public APIs

## Testing

- Unit tests for individual functions
- Integration tests for features
- Add tests for bug fixes

## Questions?

- Open an issue for bugs
- Start a discussion for questions
- Join our Discord for chat

## License

By contributing, you agree that your contributions will be licensed under the project's license.
