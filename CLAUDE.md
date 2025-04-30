# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands
- **JavaScript/Node.js**: `cd js && npm install && node browser.js` or `node node.js`
- **TypeScript**: `cd ts && npm install && npm run build && npm start`
- **Python**: `cd python && pip install -r requirements.txt && python jwt_header.py`
- **Rust**: `cd rust && cargo build && cargo run`
- **Single Test**: Language-specific test runners (e.g., `pytest test_file.py::test_name -v` for Python) 
- **Lisp Dialects**: Hy: `cd hy && hy jwt_header.hy`, Racket: `cd racket && racket jwt-header.rkt`
- **Clojure**: `cd clojure && lein deps && lein run`
- **Org-mode**: `make examples` (extract code), `make slides` (PDF), `make book` (book PDF)
- **Validation**: `make book-examples` (extracts and validates book examples)
- **Linting**: TypeScript: `tsc --noEmit --allowJs`, Python: `python -m py_compile`

## Code Style Guidelines
- **JWT Security**: Verify signatures before parsing, validate all claims (exp, iss, aud, sub)
- **Algorithm Safety**: Reject "none" algorithm, validate algorithm types, prevent confusion attacks
- **Base64url**: Handle URL-safe base64 properly (`-` → `+`, `_` → `/`) with correct padding
- **Types**: Use type annotations in TypeScript/Python; interfaces for JWT structures
- **Formatting**: Follow language conventions (PEP 8 for Python, standard JS for JavaScript)
- **Imports**: Group by stdlib → third-party → local with blank lines between groups
- **Naming**: snake_case for Python, camelCase for JS/TS, PascalCase for classes
- **Error handling**: Use idiomatic approaches (try/except in Python, Result in Rust)
- **Functions**: Use pure functions for parsing steps, avoid side effects

## Security Requirements
- Implement proper token validation before accessing payload
- Use short-lived tokens with appropriate issuer and audience claims
- Defend against common JWT attacks (none alg, confusion, token injection)
- Implement token revocation for sensitive operations
- Follow secure key management practices