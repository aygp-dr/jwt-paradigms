.PHONY: all clean build build-all tangle examples slides pdf view present book book-examples tangle-all paradigms_lost.pdf

# Default target - lists available commands
all:
	@echo "Available commands:"
	@echo "  make tangle       - Tangle all .org files in the root directory"
	@echo "  make examples     - Extract code from examples.org to examples/ directory"
	@echo "  make slides       - Generate presentation.pdf from presentation.org"
	@echo "  make pdf          - Same as slides"
	@echo "  make view         - View the generated PDF (if available)"
	@echo "  make present      - Present slides using pdfpc (optimal presentation tool)"
	@echo "  make book         - Generate PDF from personas/gadfly/paradigms_lost.org"
	@echo "  make paradigms_lost.pdf - Direct target to generate the book PDF"
	@echo "  make book-examples - Extract and validate code examples from book chapters"
	@echo "  make clean        - Remove generated files"
	@echo "  make build        - Build all artifacts (tangle code and generate PDFs)"
	@echo "  make build-all    - Build everything including code examples from the book"

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
book: personas/gadfly/paradigms_lost.pdf

# Direct target for the book PDF
personas/gadfly/paradigms_lost.pdf: personas/gadfly/paradigms_lost.org personas/gadfly/chapters/*.org .dir-locals.el
	@echo "Generating paradigms_lost.pdf..."
	@emacs --batch --eval "(require 'org)" --eval "(load-theme 'tango t)" \
		--eval "(require 'htmlize)" \
		--eval "(setq org-latex-pdf-process '(\"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f\" \"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f\" \"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f\"))" \
		--eval "(setq org-latex-listings 'minted)" \
		--eval "(add-to-list 'org-latex-packages-alist '(\"\" \"minted\"))" \
		--visit="personas/gadfly/paradigms_lost.org" \
		--funcall org-latex-export-to-pdf
	@echo "Done! Generated personas/gadfly/paradigms_lost.pdf"

pdf: slides

# View the generated PDF (platform dependent)
view:
	@if [ -f presentation.pdf ]; then \
		if command -v pdfpc > /dev/null; then \
			echo "Opening with pdfpc (recommended for presentations)..."; \
			pdfpc presentation.pdf; \
		elif command -v xdg-open > /dev/null; then \
			echo "Opening with default PDF viewer..."; \
			xdg-open presentation.pdf; \
		elif command -v open > /dev/null; then \
			echo "Opening with default PDF viewer..."; \
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

# Present the slides using pdfpc (recommended for presentations)
present: presentation.pdf
	@if command -v pdfpc > /dev/null; then \
		echo "Starting presentation with pdfpc..."; \
		pdfpc presentation.pdf; \
	else \
		echo "pdfpc not found. For optimal presentation experience, please install:"; \
		echo "  - FreeBSD: pkg install pdfpc"; \
		echo "  - Linux: apt install pdfpc (Debian/Ubuntu) or dnf install pdfpc (Fedora)"; \
		echo "  - macOS: brew install pdfpc"; \
		echo "Falling back to regular PDF viewer..."; \
		make view; \
	fi

# Build everything
build: tangle slides book

# Tangle and validate code examples from the book
book-examples:
	@echo "Processing code examples from book chapters..."
	@cd personas/gadfly && $(MAKE) tangle-examples && $(MAKE) check-examples
	@echo "Code examples processed and checked."

# Build everything including code examples
build-all: tangle slides book book-examples

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@rm -f presentation.pdf personas/gadfly/paradigms_lost.pdf
	@rm -f *.tex *.aux *.log *.out *.toc *.nav *.snm *.vrb
	@rm -f personas/gadfly/*.tex personas/gadfly/*.aux personas/gadfly/*.log personas/gadfly/*.out personas/gadfly/*.toc
	@echo "Cleaning book examples..."
	@cd personas/gadfly && $(MAKE) clean
	@echo "Done!"