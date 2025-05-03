# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands
- **JavaScript/Node.js**: `cd examples/js && node browser.js` or `node node.js`
- **TypeScript**: `cd examples/ts && npm install && npm run build && npm start`
- **Python**: `cd examples/python && python jwt_header.py`
- **Rust**: `cd examples/rust && cargo build && cargo run`
- **Go**: `cd examples/revocation && go run go_revocation.go`
- **Testing**: Language-specific test runners (e.g., `pytest examples/python/test_file.py::test_name -v`)
- **Org-mode**: `make examples` (extract code), `make slides` (PDF), `make book` (book PDF)
- **Validation**: `make book-examples` (extracts and validates book examples)
- **Linting**: TypeScript: `tsc --noEmit --allowJs`, Python: `python -m py_compile examples/**/*.py`

## Code Style Guidelines
- **JWT Security**: Always verify signatures before parsing, validate standard claims (exp, iss, aud, sub)
- **Algorithm Safety**: Reject "none" algorithm, validate types, prevent confusion attacks
- **Base64url**: Handle URL-safe base64 properly with correct padding (`-` → `+`, `_` → `/`)
- **Types**: Use strong typing (TypedDict in Python, interfaces in TypeScript) for JWT structures
- **Formatting**: Follow language conventions (PEP 8 for Python, standard JS/TS for JavaScript/TypeScript)
- **Imports**: Group by stdlib → third-party → local with blank lines between groups
- **Naming**: snake_case for Python, camelCase for JS/TS, PascalCase for classes
- **Error handling**: Use language-idiomatic approaches (try/except in Python, Result in Rust, error types in Go)
- **Functions**: Prefer pure functions for JWT operations, clearly separate parsing from verification

## Security Requirements
- Implement proper token validation before accessing payload
- Use short-lived tokens with appropriate expiration and issuer/audience claims
- Implement token revocation via Redis or database storage
- Defend against common JWT attacks (none alg, confusion, token injection)
- Employ family-based revocation for handling all tokens of specific users
- Use JTI (JWT ID) claims for individual token tracking

## Git Worktree Workflow
- **Naming Convention**: Use `../{repo}-wt-{branch name}` where branch name includes issue number
- **Creating Worktrees**: `git worktree add ../jwt-paradigms-wt-feature42 -b feature-issue42-wt feature-issue42`
- **Detecting Worktrees**: Use `git worktree list` to see all worktrees
- **Cleanup**: `git worktree remove ../jwt-paradigms-wt-feature42`