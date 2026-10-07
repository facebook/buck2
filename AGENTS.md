# AGENTS.md

Context file for AI agents working on buck2.

**Dual Format**: This file combines Category A (Operations Manual) and Category B (Context Guide) for comprehensive agent guidance.

## Project Overview

buck2 is a Rust project using Rust (cargo).

**Key Info:**
- **Primary Language:** Rust
- **Build System:** Rust (cargo)
- **Test Framework:** None detected
- **Total Files:** 9716
- **Test Files:** 3817
- **AI Readiness Score:** 91/100 (Agent-Optimized)

---

## 🚨 AI Policy & Operations

Extracted from CONTRIBUTING.md - operational constraints and procedures.

### Key Requirements

- In order to accept your pull request, we need you to submit a CLA. You only need

### Development Procedures

- developed internally gets reviewed, sent through CI, committed, and then
- our internal workflow where it is reviewed, sent through CI, committed and added
- GitHub) and a more thorough internal CI (building internal projects etc). Alas,
- our full test suite is not yet mirrored to the open source repo, but we hope to
- 2. If you've added code that should be tested, add tests.



## 🏗️ Architecture & Context Guide

This section provides architectural context and agent-understanding for the codebase.

### Prerequisites

- **Rust:** 1.56+ (or applicable language version)
- **Package Manager:** cargo
- **Test Runner:** None detected



### Project Structure

```
buck2/
├── Cargo.toml
├── Cargo.toml
├── Cargo.toml
├── src/                  # Source code
├── tests/                # Test suite (3817 files)
└── README.md             # Project documentation
```

### Architecture Overview

#### Key Components
- **Main Entry:** main.rs, main.rs, main.rs, main.rs, main.rs
- **Test Suite:** 3817 test files
- **Build Configuration:** Cargo.toml, Cargo.toml, Cargo.toml

#### Design Principles

1. **Modularity** - Code organized by functionality with clear separation of concerns
2. **Testability** - Comprehensive test coverage across critical paths
3. **Clarity** - Explicit naming and structure for AI agent understanding
4. **Consistency** - Uniform patterns and conventions throughout codebase
5. **Maintainability** - Well-documented code with clear intent

### Directory Map

| Directory | Purpose |
|-----------|----------|
| `docs/` | Documentation |
| `examples/` | Usage examples |
| `tests/` | Test suite |


### Development Workflow

#### Initial Setup

```bash
git clone https://github.com/jaykrishna316/buck2.git
cd buck2
cargo build
```

#### Development Commands

**Running Tests:**
```bash
cargo build               # Build project
cargo test                # Run all tests
cargo test --verbose      # Verbose test output
```

#### Code Quality
```bash
cargo fmt                 # Format code
cargo clippy              # Lint with clippy
```

### Code Style & Conventions

- **Naming:** Use Rust conventions (snake_case for functions, PascalCase for classes)
- **Type Hints:** Yes (strongly encouraged)
- **Error Handling:** Yes - handle errors at boundaries; let exceptions propagate when another layer owns recovery
- **Logging:** No
- **Testing:** Yes - write tests alongside code changes

### Testing Strategy

**Framework:** None detected
**Test Files:** 3817 found

Before committing:
1. Run the full test suite: `cargo test`
2. Run clippy: `cargo clippy --all-targets`
3. Format code: `cargo fmt`
4. Check documentation: `cargo doc --no-deps`

### Writing Documentation

When updating docs:
1. Always include explanatory text before code snippets
2. Describe *why* and *what* before showing *how*
3. Keep sections focused on a single concept
4. Use clear, concrete examples

### Contributing Guidelines

This project has a detailed contribution guide at **`CONTRIBUTING.md`**.

**Key Requirements:**
- Review the contribution guide for all requirements
- Follow established patterns in the codebase
- Ensure alignment with project's contribution policies

### Common Patterns

When contributing to this project:
1. Read existing code in the area you're modifying
2. Follow the established patterns and style
3. Write tests for new functionality
4. Use clear, descriptive variable and function names
5. Add docstrings for public APIs
6. Update tests when changing behavior

### What We Value

✅ Well-tested code with clear intent
✅ Consistent code style and naming conventions
✅ Code that is easy for AI agents to understand
✅ Clear, descriptive commit messages
✅ Modular, reusable components
✅ Comprehensive documentation

### What We Avoid

❌ Large functions doing multiple things
❌ Commented-out dead code
❌ Inconsistent naming or patterns
❌ Unclear error messages
❌ Unexplained magic numbers or strings
❌ Skipped tests or test TODOs

### AI Readiness Dimensions (Scoring)

This project is evaluated across 8 dimensions:

1. **Architecture** (20/100) - Code organization and modularity
2. **Testing** (15/100) - Test coverage and quality
3. **Dependencies** (12/100) - Dependency management
4. **Conventions** (6/100) - Consistent patterns
5. **Entry Points** (10/100) - Clear main/start locations
6. **Security** (10/100) - Input validation and error handling
7. **Build** (10/100) - Clear build/setup instructions
8. **Documentation** (8/100) - Code and project documentation

### Next Steps

Before making changes:
1. Read relevant source files to understand the existing code
2. Look at existing tests for similar functionality
3. Follow the patterns you see in the codebase
4. Write tests for your changes
5. Run `cargo test` to verify nothing breaks
6. Run clippy: `cargo clippy --all-targets`
7. Format your code: `cargo fmt`

---

*Generated by Braxis - keeping AI agents in sync with your code*

