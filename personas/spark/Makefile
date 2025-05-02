.PHONY: all clean tangle run-examples check-examples

# Default target
all: tangle

# List of chapter files (add more as needed)
CHAPTERS = chapters/chapter01.org chapters/chapter02.org chapters/chapter03.org \
           chapters/chapter04.org chapters/chapter05.org chapters/chapter06.org \
           chapters/chapter07.org chapters/chapter08.org chapters/chapter09.org \
           chapters/chapter10.org chapters/chapter11.org chapters/chapter12.org \
           chapters/chapter13.org chapters/chapter14.org chapters/chapter15.org

# Target to tangle code from all chapters
tangle: 
	@echo "Extracting code examples from chapters..."
	@for chapter in $(CHAPTERS); do \
		echo "Tangling $$chapter..."; \
		emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"$$chapter\")"; \
	done
	@echo "Done tangling code examples!"

# Instead of trying to convert example blocks to source blocks,
# let's create proper source code examples from our chapter15_with_src_blocks.org example
tangle-examples: $(EXAMPLE_DIRS)
	@echo "Tangling code from example with proper source blocks..."
	@emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"examples/chapter15_with_src_blocks.org\")"
	@echo "Done tangling example source blocks!"

# Check extracted examples (basic validation)
check-examples:
	@echo "Checking examples for basic syntax validity..."
	@echo "Checking TypeScript examples..."
	@if command -v tsc > /dev/null; then \
		for file in examples/typescript/*.ts; do \
			if [ -f "$$file" ]; then \
				echo "Checking $$file"; \
				tsc --noEmit --allowJs "$$file" || echo "Warning: TypeScript check failed for $$file"; \
			fi; \
		done; \
	else \
		echo "TypeScript compiler not found, skipping TypeScript validation"; \
	fi
	@echo "Checking Python examples..."
	@if command -v python > /dev/null; then \
		for file in examples/python/*.py; do \
			if [ -f "$$file" ]; then \
				echo "Checking $$file"; \
				python -m py_compile "$$file" || echo "Warning: Python syntax check failed for $$file"; \
			fi; \
		done; \
	else \
		echo "Python not found, skipping Python validation"; \
	fi
	@echo "Examples checked!"

# Generate PDF from paradigms_lost.org with properly tangled examples
pdf: tangle
	@echo "Generating book PDF with tangled examples..."
	@emacs --batch --eval "(require 'org)" --eval "(load-theme 'tango t)" \
		--eval "(setq org-latex-pdf-process '(\"pdflatex -interaction nonstopmode -output-directory %o %f\" \"pdflatex -interaction nonstopmode -output-directory %o %f\" \"pdflatex -interaction nonstopmode -output-directory %o %f\"))" \
		--visit="paradigms_lost.org" \
		--funcall org-latex-export-to-pdf
	@echo "Done! Generated paradigms_lost.pdf with tangled examples."

# Clean up generated files
clean:
	@echo "Cleaning up generated examples..."
	@rm -rf examples/*
	@rm -rf .tmp
	@echo "Cleaned up generated examples!"

# Help target
help:
	@echo "Available targets:"
	@echo "  make              - Tangle code examples from chapters"
	@echo "  make tangle       - Tangle code from all chapters"
	@echo "  make tangle-examples - Tangle code from example with proper source blocks"
	@echo "  make check-examples - Perform basic syntax validation of extracted examples"
	@echo "  make pdf          - Generate PDF from paradigms_lost.org with tangled examples"
	@echo "  make clean        - Remove generated example files"
	@echo "  make help         - Show this help message"
