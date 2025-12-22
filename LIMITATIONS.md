# SwiftSC-Lang Compiler v1.0 - Known Limitations

## Important: Educational/Prototype Release

SwiftSC-Lang v1.0 is a **proof-of-concept compiler** for educational purposes. **Not production-ready.**

## Critical Missing Features

### 1. No Control Flow ❌
- No `if/else` statements
- No `while/for` loops  
- No pattern matching
- **Impact:** Cannot write conditional logic

### 2. Limited Type System ❌
- Only `u64` type
- No structs or enums
- No strings or arrays
- **Impact:** Cannot model complex data

### 3. No Real Blockchain Integration ❌
- Only mock testing
- Not deployed to any chain
- **Impact:** Unproven in production

## What Works ✅

- Basic arithmetic expressions
- Function calls and recursion
- Host function integration
- WASM code generation
- Type checking
- Security analysis (basic)

## Use Cases

**Good for:**
- Learning compiler design
- Understanding WASM
- Portfolio demonstration

**Not for:**
- Production smart contracts
- Real DeFi applications
- Mainnet deployments
