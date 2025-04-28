# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands
- **JavaScript/Node.js**: `cd js && npm install && node browser-example.js` or `node node-example.js`
- **TypeScript**: `cd ts && npm install && npm run build && npm start`
- **Python**: `cd python && pip install -r requirements.txt && python jwt_header.py`
- **Hy**: `cd hy && pip install hy && hy jwt_header.hy`
- **Clojure**: `cd clojure && lein deps && lein run`
- **Racket**: `cd racket && raco pkg install --auto && racket jwt-header.rkt`
- **Scheme**: `cd scheme && guile jwt-header.scm`
- **Shell**: `cd shell && chmod +x jwt_header.sh && ./jwt_header.sh`
- **Rust**: `cd rust && cargo build && cargo run`

## Code Style Guidelines
- **Formatting**: Follow language-specific conventions (PEP 8 for Python, standard JS for JavaScript)
- **Imports**: Group imports by stdlib → third-party → local with blank lines between groups
- **Types**: Use type annotations in TypeScript and Python; structs in Rust
- **Error handling**: Use idiomatic approaches (try/except in Python, Result in Rust)
- **Base64url handling**: Correctly handle URL-safe base64 (`-` → `+`, `_` → `/`) and padding
- **JWT parsing**: Extract header before payload, handle authentication headers properly
- **Security**: Note that examples focus on parsing, not security; in production code, verify signatures first
- **Functions**: Prefer pure functions for transformation steps in parsing