.PHONY: all clean build tangle examples slides pdf view

# Default target - lists available commands
all:
	@echo "Available commands:"
	@echo "  make tangle    - Extract code from examples.org to examples/ directory"
	@echo "  make examples  - Same as tangle"
	@echo "  make slides    - Generate presentation.pdf from presentation.org"
	@echo "  make pdf       - Same as slides"
	@echo "  make view      - View the generated PDF (if available)"
	@echo "  make clean     - Remove generated files"
	@echo "  make build     - Build all artifacts (tangle code and generate PDF)"

# Extract code examples from org files
tangle:
	@echo "Tangling code from examples.org..."
	@emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"examples.org\")"
	@echo "Done!"

examples: tangle

# Generate PDF presentation from presentation.org using Beamer
slides:
	@echo "Generating presentation PDF..."
	@emacs --batch --eval "(require 'org)" --eval "(load-theme 'tango t)" \
		--eval "(setq org-latex-pdf-process '(\"pdflatex -interaction nonstopmode -output-directory %o %f\" \"pdflatex -interaction nonstopmode -output-directory %o %f\" \"pdflatex -interaction nonstopmode -output-directory %o %f\"))" \
		--visit="presentation.org" \
		--funcall org-beamer-export-to-pdf
	@echo "Done! Generated presentation.pdf"

pdf: slides

# View the generated PDF (platform dependent)
view:
	@if [ -f presentation.pdf ]; then \
		if command -v xdg-open > /dev/null; then \
			xdg-open presentation.pdf; \
		elif command -v open > /dev/null; then \
			open presentation.pdf; \
		else \
			echo "PDF viewer not found. Please open presentation.pdf manually."; \
		fi; \
	else \
		echo "presentation.pdf not found. Run 'make slides' first."; \
	fi

# Build everything
build: tangle slides

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@rm -f presentation.pdf
	@rm -f *.tex *.aux *.log *.out *.toc *.nav *.snm *.vrb
	@echo "Done!"