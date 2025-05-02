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
  cat > ../jwt-paradigms-wt-feature42/.worktree/task.org << 'EOF'
  #+TITLE: Feature Name - Issue 42
  #+AUTHOR: aygp-dr
  #+DATE: $(date +%Y-%m-%d)
  #+PROPERTY: header-args :mkdirp yes
  
  * Task Summary
  Brief description of the task for issue #42.
  
  ** Requirements
  - List key requirements here
  - Add implementation goals
  
  ** Implementation Notes
  - Working in branch: feature-issue42-wt
  - Relevant files and components
  
  ** Validation Steps
  1. Step one
  2. Step two
  3. Step three
  
  ** References
  - Issue #42: https://github.com/aygp-dr/jwt-paradigms/issues/42
  EOF
  ```
- **PR Creation Process**:
  ```bash
  # Make changes, commit, and push
  git add .
  git commit -m "feat: implement feature for #42"
  git push -u origin feature-issue42-wt
  
  # Create pull request
  gh pr create --title "Implement feature for #42" --body "
  ## Summary
  - Brief description of the changes
  - Why they are needed
  - Key implementation details
  
  ## Testing Done
  - How changes were tested
  - Results of testing
  
  Fixes #42
  "
  ```
- **PR Review Process**:
  - Assign at least one reviewer to each PR
  - Use Claude PR Reviewer agent for initial feedback
  - Reviewer examines code quality, test coverage, and documentation
  - Address all feedback before merging
- **Claude Detection**: When working in a worktree, use `/worktree` command to have Claude check task information
- **Detecting Worktrees**: Use `git worktree list` to see all worktrees, path pattern `{repo}-wt-{branch}` indicates a worktree
- **Cleanup**: `git worktree remove ../jwt-paradigms-wt-feature42` after PR is merged

For detailed documentation, see `docs/git_worktree_workflow.md`.

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