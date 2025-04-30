# Variables for common settings and paths
EMACS := emacs --batch
ORG_REQUIRE := --eval "(require 'org)"
THEME_LOAD := --eval "(load-theme 'tango t)"
PDFLATEX_BASIC := "pdflatex -interaction nonstopmode -output-directory %o %f"
PDFLATEX_SHELL_ESCAPE := "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
PRESENTATION_ORG := presentation.org
PRESENTATION_PDF := presentation.pdf
BOOK_DIR := personas/gadfly
BOOK_ORG := $(BOOK_DIR)/paradigms_lost.org
BOOK_PDF := $(BOOK_DIR)/paradigms_lost.pdf

.PHONY: all clean build build-all tangle examples slides pdf view present book book-examples tangle-all

# Default target - lists available commands
all:
	@echo "JWT Parsing Examples - Makefile Commands"
	@echo "======================================"
	@echo
	@echo "Presentation Commands:"
	@echo "  make slides       - Generate presentation.pdf from presentation.org"
	@echo "  make view         - View the generated PDF (if available)"
	@echo "  make present      - Present slides using pdfpc (optimal presentation tool)"
	@echo
	@echo "Code Extraction Commands:"
	@echo "  make tangle       - Tangle all .org files in the root directory"
	@echo "  make examples     - Extract code from examples.org to examples/ directory"
	@echo
	@echo "Book Commands:"
	@echo "  make book         - Generate PDF from $(BOOK_ORG)"
	@echo "  make book-examples - Extract and validate code examples from book chapters"
	@echo
	@echo "Utility Commands:"
	@echo "  make clean        - Remove generated files"
	@echo "  make build        - Build all artifacts (tangle code and generate PDFs)"
	@echo "  make build-all    - Build everything including code examples from the book"

# Extract code examples from examples.org
examples: examples.org
	@echo "Tangling code from examples.org..."
	@$(EMACS) $(ORG_REQUIRE) --eval "(org-babel-tangle-file \"$<\")"
	@echo "Done!"

# Tangle all org files in the root directory
tangle-all:
	@echo "Tangling all org files in root directory..."
	@for file in *.org; do \
		echo "Tangling $$file..."; \
		$(EMACS) $(ORG_REQUIRE) --eval "(org-babel-tangle-file \"$$file\")"; \
	done
	@echo "Done!"

# Alias for tangle-all
tangle: tangle-all

# Generate PDF presentation from presentation.org using Beamer
slides: $(PRESENTATION_ORG)
	@echo "Generating presentation PDF..."
	@$(EMACS) $(ORG_REQUIRE) $(THEME_LOAD) \
		--eval "(setq org-latex-pdf-process '($(PDFLATEX_BASIC) $(PDFLATEX_BASIC) $(PDFLATEX_BASIC)))" \
		--visit="$<" \
		--funcall org-beamer-export-to-pdf
	@echo "Done! Generated $(PRESENTATION_PDF)"

# Generate PDF from paradigms_lost.org
book: $(BOOK_PDF)

# Direct target for the book PDF
$(BOOK_PDF): $(BOOK_ORG) $(BOOK_DIR)/chapters/*.org .dir-locals.el
	@echo "Generating $(BOOK_PDF)..."
	@$(EMACS) $(ORG_REQUIRE) $(THEME_LOAD) \
		--eval "(condition-case nil (require 'htmlize) (error (message \"Note: htmlize package not available. Syntax highlighting may be limited.\")))" \
		--eval "(setq org-latex-pdf-process '($(PDFLATEX_SHELL_ESCAPE) $(PDFLATEX_SHELL_ESCAPE) $(PDFLATEX_SHELL_ESCAPE)))" \
		--eval "(setq org-latex-listings 'minted)" \
		--eval "(add-to-list 'org-latex-packages-alist '(\"\" \"minted\"))" \
		--visit="$(BOOK_ORG)" \
		--funcall org-latex-export-to-pdf
	@echo "Done! Generated $(BOOK_PDF)"

# Alias for slides
pdf: slides

# Define PDF viewing commands for different platforms
PDFPC := pdfpc
LINUX_OPEN := xdg-open
MAC_OPEN := open

# View the generated PDF (platform dependent)
view:
	@if [ -f $(PRESENTATION_PDF) ]; then \
		if command -v $(PDFPC) > /dev/null; then \
			echo "Opening with pdfpc (recommended for presentations)..."; \
			$(PDFPC) $(PRESENTATION_PDF); \
		elif command -v $(LINUX_OPEN) > /dev/null; then \
			echo "Opening with default PDF viewer..."; \
			$(LINUX_OPEN) $(PRESENTATION_PDF); \
		elif command -v $(MAC_OPEN) > /dev/null; then \
			echo "Opening with default PDF viewer..."; \
			$(MAC_OPEN) $(PRESENTATION_PDF); \
		else \
			echo "PDF viewer not found. Please open $(PRESENTATION_PDF) manually."; \
		fi; \
	elif [ -f $(BOOK_PDF) ]; then \
		if command -v $(LINUX_OPEN) > /dev/null; then \
			$(LINUX_OPEN) $(BOOK_PDF); \
		elif command -v $(MAC_OPEN) > /dev/null; then \
			$(MAC_OPEN) $(BOOK_PDF); \
		else \
			echo "PDF viewer not found. Please open $(BOOK_PDF) manually."; \
		fi; \
	else \
		echo "No PDF files found. Run 'make slides' or 'make book' first."; \
	fi

# Present the slides using pdfpc (recommended for presentations)
present: $(PRESENTATION_PDF)
	@if command -v $(PDFPC) > /dev/null; then \
		echo "Starting presentation with pdfpc..."; \
		$(PDFPC) $(PRESENTATION_PDF); \
	else \
		echo "pdfpc not found. For optimal presentation experience, please install:"; \
		echo "  - FreeBSD: pkg install pdfpc"; \
		echo "  - Linux: apt install pdfpc (Debian/Ubuntu) or dnf install pdfpc (Fedora)"; \
		echo "  - macOS: brew install pdfpc"; \
		echo "Falling back to regular PDF viewer..."; \
		$(MAKE) view; \
	fi

# Combined targets
build: tangle slides book
build-all: tangle slides book book-examples

# Tangle and validate code examples from the book
book-examples:
	@echo "Processing code examples from book chapters..."
	@cd $(BOOK_DIR) && $(MAKE) tangle-examples && $(MAKE) check-examples
	@echo "Code examples processed and checked."

# Generated file patterns to clean
LATEX_TEMP_FILES := *.tex *.aux *.log *.out *.toc *.nav *.snm *.vrb
BOOK_LATEX_TEMP_FILES := $(BOOK_DIR)/*.tex $(BOOK_DIR)/*.aux $(BOOK_DIR)/*.log $(BOOK_DIR)/*.out $(BOOK_DIR)/*.toc

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@rm -f $(PRESENTATION_PDF) $(BOOK_PDF)
	@rm -f $(LATEX_TEMP_FILES)
	@rm -f $(BOOK_LATEX_TEMP_FILES)
	@echo "Cleaning book examples..."
	@cd $(BOOK_DIR) && $(MAKE) clean
	@echo "Done!"