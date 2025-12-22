# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 0.1.x   | :white_check_mark: |

## Reporting a Vulnerability

**Please do not report security vulnerabilities through public GitHub issues.**

Instead, please report them via email to: security@swiftsc-lang.dev

You should receive a response within 48 hours. If the issue is confirmed, we will:

1. Acknowledge receipt
2. Investigate and develop a fix
3. Release a security patch
4. Publicly disclose the vulnerability

## Security Features

SwiftSC-Lang includes:
- Static security analysis
- Integer overflow detection
- Gas metering
- Formal verification support

## Best Practices

When writing SwiftSC-Lang contracts:
- Use checked arithmetic from `std::math`
- Follow security guidelines in documentation
- Run security analyzer before deployment
- Test thoroughly

## Disclosure Policy

- Security issues fixed within 90 days
- Public disclosure after patch release
- Credit given to reporters (if desired)
