# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands
- **JavaScript/Node.js**: `cd examples/js && node browser.js` or `node node.js`
- **TypeScript**: `cd examples/ts && npm install && npm run build && npm start`
- **Python**: `cd examples/python && python jwt_header.py`
- **Rust**: `cd examples/rust && cargo build && cargo run`
- **Single Test**: Use language-specific test runners (e.g., `pytest examples/python/test_file.py::test_name -v`)
- **Org-mode**: `make examples` (extract code), `make slides` (PDF), `make book` (book PDF)
- **Validation**: `make book-examples` (extracts and validates book examples)
- **Linting**: TypeScript: `tsc --noEmit --allowJs`, Python: `python -m py_compile`

## Git Worktree Workflow
- **Naming Convention**: Use `../{repo}-wt-{branch name}` where branch name includes issue number
- **Task Tracking**: Check for `.worktree/task.org` in worktree root directory for task information
- **Creating Worktrees**:
  ```bash
  # Create feature branch
  git checkout -b feature-issue42
  
  # Create worktree with proper naming
  git worktree add ../jwt-paradigms-wt-feature42 -b feature-issue42-wt feature-issue42
  
  # Initialize task tracking
  mkdir -p ../jwt-paradigms-wt-feature42/.worktree
  # Create task.org with issue details
  ```
- **Detecting Worktrees**: Use `git worktree list` to see all worktrees
- **Cleanup**: `git worktree remove ../jwt-paradigms-wt-feature42`

## Code Style Guidelines
- **JWT Security**: Verify signatures before parsing, validate all claims (exp, iss, aud, sub)
- **Algorithm Safety**: Reject "none" algorithm, validate algorithm types, prevent confusion attacks
- **Base64url**: Handle URL-safe base64 properly (`-` → `+`, `_` → `/`) with correct padding
- **Types**: Use type annotations in TypeScript/Python; interfaces for JWT structures
- **Formatting**: Follow language conventions (PEP 8 for Python, standard JS for JavaScript)
- **Imports**: Group by stdlib → third-party → local with blank lines between groups
- **Naming**: snake_case for Python, camelCase for JS/TS, PascalCase for classes
- **Error handling**: Use idiomatic approaches (try/except in Python, Result in Rust)
- **Functions**: Prefer pure functions for parsing steps, avoid side effects

## Security Requirements
- Implement proper token validation before accessing payload
- Use short-lived tokens with appropriate issuer and audience claims
- Defend against common JWT attacks (none alg, confusion, token injection)
- Implement token revocation for sensitive operations
- Follow secure key management practices