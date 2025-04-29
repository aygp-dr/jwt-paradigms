# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands
- **JavaScript/Node.js**: `cd js && npm install && node browser.js` or `node node.js`
- **TypeScript**: `cd ts && npm install && npm run build && npm start`
- **Python**: `cd python && pip install -r requirements.txt && python jwt_header.py`
- **Rust**: `cd rust && cargo build && cargo run`
- **Single Test**: Use language-specific test runners (e.g., `pytest test_file.py::test_name -v` for Python)
- **Lisp Dialects**: Hy: `cd hy && hy jwt_header.hy`, Racket: `cd racket && racket jwt-header.rkt`
- **Clojure**: `cd clojure && lein deps && lein run`
- **Org-mode**: `make examples` (extract code), `make slides` (PDF), `make book` (book PDF)
- **Code Validation**: `make book-examples` (extracts and validates examples from book chapters)

## Code Style Guidelines
- **Formatting**: Follow language conventions (PEP 8 for Python, standard JS for JavaScript)
- **Imports**: Group by stdlib → third-party → local with blank lines between groups
- **Types**: Use type annotations in TypeScript/Python; interfaces for JWT structures
- **Naming**: snake_case for Python, camelCase for JS, PascalCase for classes
- **Error handling**: Use idiomatic approaches (try/except in Python, Result in Rust)
- **JWT Security**: Always verify signatures before parsing, validate all claims (exp, iss, aud, sub)
- **Algorithm Safety**: Never accept "none" algorithm, validate algorithm types, prevent algorithm confusion
- **Functions**: Use pure functions for parsing steps, avoid side effects
- **Base64url**: Handle URL-safe base64 properly (`-` → `+`, `_` → `/`) with correct padding

## Testing
- Test with both valid and invalid tokens
- Validate proper handling of malformed tokens and expired claims
- Test all flows: parsing, validation, refresh, and revocation
- Verify defense against common JWT attacks (none alg, alg confusion, token injection)
- Use short-lived tokens in tests with appropriate issuer and audience claims