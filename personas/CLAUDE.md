# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build & Test Commands
- **Setup**: `make setup` (create Python 3.11 virtual environment and install dependencies)
- **Lint/Format**: `make lint` (ruff check) or `make fix` (auto-fix issues) or `make format` (format code)
- **Run Scripts**: `python scripts/script_name.py` or as module `python -m scripts.script_name`
- **Generate Images**: `make images` (standard) or `make alternatives` (alternative versions)
- **Process All**: `make process-all` (extract code, publish to markdown, generate images)
- **Single Test**: Run a specific script directly with args, e.g., `python scripts/extract_persona.py --input file.org --output output.md`

## Code Style Guidelines
- **Python Version**: Requires Python 3.11+
- **Formatting**: Follow ruff rules (line length 100, double quotes, space indentation)
- **Imports**: Group by stdlib → third-party → local with blank lines between
- **Linting**: Follow ruff's selected rules (E: style errors, F: flakes, I: imports, W: warnings, B: bugs)
- **Types**: Use type annotations for function parameters and return values
- **Naming**: snake_case for variables/functions, PascalCase for classes
- **Error Handling**: Use try/except with specific error types, log appropriately
- **Documentation**: Include docstrings for all functions, classes, and modules
- **Org Mode**: Structured org files with proper headers for extracting content
- **Logging**: Use Python's logging module with appropriate log levels