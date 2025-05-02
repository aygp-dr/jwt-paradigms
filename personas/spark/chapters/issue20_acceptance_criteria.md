# Acceptance Criteria for Issue #20: "Convert examples to proper begin_src blocks for tangle support"

## Objectives
1. All code examples in "Paradigms Lost" chapters should use proper org-mode begin_src blocks with tangle directives
2. Enable automatic extraction of code examples via org-babel tangling
3. Support validation of syntax correctness for all code examples
4. Implement a consistent workflow for editing and maintaining code examples

## Required Changes

- [ ] All code blocks should use the format: `#+BEGIN_SRC language :tangle ../examples/language/filename.ext :mkdirp yes`
- [ ] Each source block should specify the appropriate file extension for the language (see language mapping in update_tangle.py)
- [ ] File paths should follow the convention: `../examples/{language}/{chapter_name}_{descriptive_name}.{extension}`
- [ ] File names should be descriptive of the example's content (e.g., `chapter01_von_neumann.c` not just `example1.c`)
- [ ] All blocks should include the `:mkdirp yes` property to automatically create directories when needed
- [ ] The tangle script should successfully extract all code examples to their target locations
- [ ] Directory structure should be organized by language type (e.g., `/examples/java/`, `/examples/python/`)
- [ ] The CI pipeline should validate that all tangled code examples compile/parse without syntax errors

## Implementation Notes

- Use the existing `update_tangle.py` script for automatic updating of blocks without tangle directives
- The `tangle_chapters.sh` script will extract code from the org files to their targets
- The process should be compatible with FreeBSD environment
- All examples should maintain proper indentation and formatting in the tangled files

## Verification Steps

1. Run `python update_tangle.py personas/gadfly/chapters` to add tangle directives to any missing blocks
2. Run `./tangle_chapters.sh` to extract all code examples
3. Verify correct directory structure was created with `find personas/gadfly/examples -type d | sort`
4. Check all files were extracted with `find personas/gadfly/examples -type f | sort`
5. Run syntax validation for each language type where possible (e.g., `gcc -fsyntax-only` for C files)
6. Generate the book PDF to ensure all code blocks display correctly in the document

## Implementation Plan

1. First update chapters 3-6 which are known to have source blocks without tangle directives
2. Then verify all other chapters have proper tangle directives
3. Run the extraction and validation steps above
4. Make any needed corrections to the block format or file names
5. Verify the final output with book generation and syntax checking