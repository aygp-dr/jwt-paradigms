# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands
- **Setup**: `make setup` (create Python 3.11 virtual environment and install dependencies)
- **Lint**: `make lint` (run ruff checks) or `make fix` (auto-fix issues)
- **Format**: `make format` (format Python code with ruff)
- **Generate Images**: `python scripts/generate_headshots.py` or `make images` (extracts personas)
- **Process Single Script**: `python scripts/extract_personas.py --org-file filename.org`
- **Test Specific File**: Run `python -m pytest scripts/tests/test_file.py::test_name -v`
- **Process All**: `make process-all` (extract code, publish to markdown, generate images)
- **Org Lint**: `make org-lint` (check org files for syntax issues)

## Code Style Guidelines
- **Python Version**: Requires Python 3.11+
- **Formatting**: Follow ruff format rules (line length 100, double quotes, space indentation)
- **Imports**: Group by standard library → third-party → local with blank lines between groups
- **Linting**: Follow ruff's selected rules (E, F, I, W, B)
- **Types**: Use type annotations for function parameters and return values
- **Naming**: Use snake_case for variables/functions, PascalCase for classes
- **Error Handling**: Use try/except with specific error types, log errors appropriately
- **Org Mode**: Structure org-mode files with proper headers and PROPERTY blocks with CUSTOM_ID
- **Documentation**: Include docstrings for all functions, classes, and modules
- **File Structure**: Keep personas in .org files with consistent formatting
- **Logging**: Use the logging module with appropriate levels for scripts