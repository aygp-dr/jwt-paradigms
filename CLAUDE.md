# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands
- **JavaScript/Node.js**: `cd js && npm install && node browser.js` or `node node.js`
- **TypeScript**: `cd ts && npm install && npm run build && npm start`
- **Python**: `cd python && pip install -r requirements.txt && python jwt_header.py`
- **Rust**: `cd rust && cargo build && cargo run`
- **Lisp Dialects**: Hy: `cd hy && hy jwt_header.hy`, Racket: `cd racket && racket jwt-header.rkt`, Scheme: `cd scheme && guile jwt-header.scm`
- **JVM**: Clojure: `cd clojure && lein deps && lein run`
- **Shell**: `cd shell && chmod +x jwt_header.sh && ./jwt_header.sh`
- **Org-mode**: `make examples` (extract code), `make slides` (generate PDF), `make book` (generate book PDF)

## Code Style Guidelines
- **Formatting**: Follow language conventions (PEP 8 for Python, standard JS for JavaScript)
- **Imports**: Group by stdlib → third-party → local with blank lines between groups
- **Types**: Use type annotations in TypeScript/Python; interfaces for JWT structures
- **Naming**: snake_case for Python, camelCase for JS, PascalCase for classes
- **Error handling**: Use idiomatic approaches (try/except in Python, Result in Rust)
- **JWT Security**: Always verify signatures before parsing, validate all claims (exp, iss, aud)
- **Base64url**: Handle URL-safe base64 properly (`-` → `+`, `_` → `/`) with correct padding
- **Functions**: Use pure functions for parsing steps, avoid side effects
- **Algorithm Safety**: Never accept "none" algorithm, validate algorithm types
- **Documentation**: Add docstrings/JSDoc/comments explaining parsing logic and security considerations

## Testing
- Test with both valid and invalid tokens
- Test parsing, validation, refresh, and revocation flows
- Verify proper error handling for malformed tokens
- Check security measures against common JWT attacks