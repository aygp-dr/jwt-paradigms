# Reviewer Personas Tooling

.PHONY: all setup images tangle detangle publish clean help .venv alternatives dalle gemini claude-commands process-all org-lint org-ai-images headshot-audit

# Configuration variables
VENV := . .venv/bin/activate &&
UV := $(VENV) uv run
PYTHON := $(VENV) python
EMACS := emacs --batch
ORG_FILES := $(wildcard *.org)
SCRIPTS_DIR := scripts
OUTPUT_DIR := output
CLAUDE_DIR := .claude/commands
IMAGES_DIR := images/tmp
SHELL := /bin/bash

# Default target is help
.DEFAULT_GOAL := help

help: ## Show available commands
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-15s\033[0m %s\n", $1, $2}' $(MAKEFILE_LIST)

headshot-audit: ## Audit headshot images for existence and proper references
	@$(SCRIPTS_DIR)/headshot_audit.sh

all: images ## Setup and generate images

.venv: ## Create Python 3.11 virtual environment
	uv python pin 3.11
	uv venv --python 3.11

README.md:
	pandoc -o README.md -i README.org

setup: .venv README.md ## Install dependencies from pyproject.toml
	. .venv/bin/activate && uv pip install -e .

lint: ## Lint Python code
	$(UV) ruff check $(SCRIPTS_DIR)


fix: ## Fix Python code
	$(UV) ruff check --fix $(SCRIPTS_DIR)

format: ## Format Python code
	$(UV) ruff format $(SCRIPTS_DIR)

org-lint: ## Check org files for syntax issues
	@for file in $(ORG_FILES); do \
		echo "Running org-lint on $$file"; \
		$(EMACS) --batch -l org \
			--eval "(progn \
				(require 'org) \
				(find-file \"$$file\") \
				(org-lint) \
				(with-current-buffer \"*Org Lint*\" \
					(princ (buffer-string))))"; \
	done

$(IMAGES_DIR):
	@mkdir -p $@

$(OUTPUT_DIR):
	@mkdir -p $@

$(CLAUDE_DIR):
	@mkdir -p $@

images: ## Generate persona headshots
	$(PYTHON) $(SCRIPTS_DIR)/extract_personas.py --org-file "*_*_*.org"

images/placeholder.png:
	convert -size 400x400 xc:gray -fill white -gravity center -font Arial -pointsize 40 -annotate 0 "FPO" $@

replace-placeholders: ## Replace placeholder images with Gemini versions
	@$(SCRIPTS_DIR)/replace_placeholders.sh

scale-all-gemini: ## Optional: A target to create scaled versions of all Gemini images
	@echo "Scaling all Gemini images to 400x400..."
	@mkdir -p images/scaled
	@for img in images/gemini/*.png; do \
		filename=$$(basename $$img); \
		convert "$$img" -geometry 400x400 "images/scaled/$$filename"; \
		echo "Scaled $$filename"; \
	done
	@echo "Done scaling. Files are in images/scaled/"

dalle: $(IMAGES_DIR) ## Generate headshots using DALL-E
	@for file in $(ORG_FILES); do \
		echo "Executing DALL-E blocks in $file"; \
		$(EMACS) --batch -l org \
			--eval "(progn \
				(require 'ob-core) \
				(require 'ob-gptel) \
				(org-babel-do-load-languages 'org-babel-load-languages '((gptel . t))) \
				(find-file \"$file\") \
				(org-babel-execute-buffer) \
				(save-buffer))"; \
	done

gemini: images/gemini ## Generate headshots using Gemini
	$(PYTHON) $(SCRIPTS_DIR)/generate_headshots.py

gemini-replace: ## Generate headshots using Gemini and replace only files specified in :file
	@echo "Generating headshots using file paths specified in :file attributes..."
	$(PYTHON) $(SCRIPTS_DIR)/extract_personas.py --verbose
	$(PYTHON) $(SCRIPTS_DIR)/generate_headshots.py --copy-to-target
	$(SCRIPTS_DIR)/replace_placeholders.sh

org-ai-images: $(IMAGES_DIR) ## Generate images using org-ai blocks
	@for file in $(ORG_FILES); do \
		echo "Executing org-ai image blocks in $$file"; \
		$(EMACS) --batch -l org \
			--eval "(progn \
				(require 'org-ai) \
				(org-ai-mode) \
				(find-file \"$$file\") \
				(org-babel-execute-buffer) \
				(save-buffer))"; \
	done

tangle: $(OUTPUT_DIR) ## Extract code blocks to files
	@for file in $(ORG_FILES); do \
		echo "Tangling $file"; \
		$(EMACS) --batch -l org --eval "(org-babel-tangle-file \"$file\")"; \
	done

detangle: ## Update org blocks from external files
	@for file in $(ORG_FILES); do \
		echo "Detangling $file"; \
		$(EMACS) --batch -l org --eval "(progn (find-file \"$file\") (org-babel-detangle) (save-buffer))"; \
	done

publish: $(OUTPUT_DIR) ## Export org files to markdown
	@for file in $(ORG_FILES); do \
		base=$(basename $file .org); \
		echo "Publishing $file to $(OUTPUT_DIR)/$base.md"; \
		$(EMACS) --batch -l org \
			--eval "(progn \
				(find-file \"$file\") \
				(org-md-export-to-markdown) \
				(rename-file \"$base.md\" \"$(OUTPUT_DIR)/$base.md\" t))"; \
	done

claude-commands: ## Generate Claude review command files
	@mkdir -p $(CLAUDE_DIR)
	@$(SCRIPTS_DIR)/generate_commands.sh

process-all: tangle publish images claude-commands ## Process all org files

clean: ## Remove generated files
	rm -rf $(IMAGES_DIR)/*.png $(OUTPUT_DIR)/*
