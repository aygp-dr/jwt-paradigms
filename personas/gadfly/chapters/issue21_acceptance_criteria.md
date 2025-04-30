# Issue #21 Acceptance Criteria: Refactor org-mode files and update build process

## Background
This issue involves refactoring our org-mode files and improving the org-mode build process to ensure consistent generation of PDF outputs and code examples.

## Completed Tasks
- ✅ Rename chapter files to use two-digit numbering (chapter01.org, etc.)
- ✅ Set up proper emacs configuration for org-mode and Babel
- ✅ Ensure PDF generation works correctly
- ✅ Configure tangling for code examples

## Remaining Tasks
- [ ] Add all chapter files to git
- [ ] Verify all PDFs are correctly built and pushed to GitHub

## Testing Criteria
To verify this issue is complete, the following tests must pass:

1. **Build Verification**
   - Run `make book` - should generate paradigms_lost.pdf without errors
   - Run `make slides` - should generate presentation.pdf without errors
   - Run `make examples` - should extract all code examples successfully

2. **File Structure**
   - All chapter files should follow the format `chapterNN.org`
   - Chapter files should be properly organized in the `/personas/gadfly/chapters/` directory
   - Required `.dir-locals.el` file should be properly configured for org-mode

3. **PDF Output**
   - `presentation.pdf` should be properly formatted with all sections
   - `paradigms_lost.pdf` should include all chapters with proper formatting
   - All code blocks should display with correct syntax highlighting

4. **Git Management**
   - All chapter files should be committed to git
   - Generated PDFs should be committed and pushed to GitHub

## Definition of Done
This issue will be considered complete when:
1. All remaining tasks are completed and committed
2. All testing criteria pass successfully
3. PR is approved and merged to main branch