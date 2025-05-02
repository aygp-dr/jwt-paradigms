# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands
- **Setup**: `make setup` (create Python 3.11 virtual environment and install dependencies)
- **Lint**: `make lint` (run ruff checks) or `make fix` (auto-fix issues) 
- **Format**: `make format` (format Python code with ruff)
- **Test**: `python -m pytest scripts/tests/test_file.py::test_name -v` (run specific test)
- **Process Script**: `python scripts/extract_personas.py --org-file filename.org` (process single file)
- **Generate Images**: `python scripts/generate_headshots.py` or `make images` (generate headshots)
- **Process All**: `make process-all` (extract code, publish to markdown, generate images)
- **Org Lint**: `make org-lint` (check org files for syntax issues)

## Code Style Guidelines
- **Python**: Version 3.11+, type annotations, snake_case for variables/functions, PascalCase for classes
- **Formatting**: Follow ruff rules - line length 100, double quotes, space indentation
- **Imports**: Group by stdlib → third-party → local with blank lines between groups
- **Linting**: Follow ruff's rules (E, F, I, W, B), auto-format with `make format`
- **Error Handling**: Use try/except with specific error types, log errors appropriately
- **Documentation**: Include docstrings for all functions, classes, and modules
- **Org Files**: Structure with proper headers and PROPERTY blocks with CUSTOM_ID
- **File Structure**: Keep personas in .org files with consistent formatting
- **Logging**: Use Python's logging module with appropriate levels