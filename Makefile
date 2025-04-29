.PHONY: all clean build tangle examples slides pdf view book tangle-all

# Default target - lists available commands
all:
	@echo "Available commands:"
	@echo "  make tangle       - Tangle all .org files in the root directory"
	@echo "  make examples     - Extract code from examples.org to examples/ directory"
	@echo "  make slides       - Generate presentation.pdf from presentation.org"
	@echo "  make pdf          - Same as slides"
	@echo "  make view         - View the generated PDF (if available)"
	@echo "  make book         - Generate PDF from personas/gadfly/paradigms_lost.org"
	@echo "  make clean        - Remove generated files"
	@echo "  make build        - Build all artifacts (tangle code and generate PDFs)"

# Extract code examples from examples.org
examples: examples.org
	@echo "Tangling code from examples.org..."
	@emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"$<\")"
	@echo "Done!"

# Tangle all org files in the root directory
tangle-all:
	@echo "Tangling all org files in root directory..."
	@for file in *.org; do \
		echo "Tangling $$file..."; \
		emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"$$file\")"; \
	done
	@echo "Done!"

# Alias for tangle-all
tangle: tangle-all

# Generate PDF presentation from presentation.org using Beamer
slides: presentation.org
	@echo "Generating presentation PDF..."
	@emacs --batch --eval "(require 'org)" --eval "(load-theme 'tango t)" \
		--eval "(setq org-latex-pdf-process '(\"pdflatex -interaction nonstopmode -output-directory %o %f\" \"pdflatex -interaction nonstopmode -output-directory %o %f\" \"pdflatex -interaction nonstopmode -output-directory %o %f\"))" \
		--visit="$<" \
		--funcall org-beamer-export-to-pdf
	@echo "Done! Generated presentation.pdf"

# Generate PDF from paradigms_lost.org
book: personas/gadfly/paradigms_lost.org
	@echo "Generating book PDF..."
	@emacs --batch --eval "(require 'org)" --eval "(load-theme 'tango t)" \
		--eval "(setq org-latex-pdf-process '(\"pdflatex -interaction nonstopmode -output-directory %o %f\" \"pdflatex -interaction nonstopmode -output-directory %o %f\" \"pdflatex -interaction nonstopmode -output-directory %o %f\"))" \
		--visit="$<" \
		--funcall org-latex-export-to-pdf
	@echo "Done! Generated personas/gadfly/paradigms_lost.pdf"

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
	elif [ -f personas/gadfly/paradigms_lost.pdf ]; then \
		if command -v xdg-open > /dev/null; then \
			xdg-open personas/gadfly/paradigms_lost.pdf; \
		elif command -v open > /dev/null; then \
			open personas/gadfly/paradigms_lost.pdf; \
		else \
			echo "PDF viewer not found. Please open personas/gadfly/paradigms_lost.pdf manually."; \
		fi; \
	else \
		echo "No PDF files found. Run 'make slides' or 'make book' first."; \
	fi

# Build everything
build: tangle slides book

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@rm -f presentation.pdf personas/gadfly/paradigms_lost.pdf
	@rm -f *.tex *.aux *.log *.out *.toc *.nav *.snm *.vrb
	@rm -f personas/gadfly/*.tex personas/gadfly/*.aux personas/gadfly/*.log personas/gadfly/*.out personas/gadfly/*.toc
	@echo "Done!"