# Pull Request Review Guide

This guide outlines the process for reviewing and providing feedback on pull requests in this project.

## Overview

Pull Request (PR) reviews ensure code quality, maintain project standards, and promote knowledge sharing among team members. This project uses a combination of human reviewers and Claude AI to provide thorough reviews.

## Review Process

### 1. Initial Review by Claude PR Reviewer Agent

All PRs should first be reviewed by the Claude PR Reviewer agent, which:

- Checks for basic code quality issues
- Verifies that the PR follows project conventions
- Ensures proper documentation
- Flags potential security issues
- Validates test coverage

To trigger the Claude PR Reviewer:
```bash
# Comment on the PR with
/request-review
```

### 2. Human Review

After the automated review, at least one human reviewer should examine:

1. **Code Quality**
   - Is the code readable and maintainable?
   - Does it follow project style guidelines?
   - Are there opportunities for improvement?

2. **Functionality**
   - Does the code correctly implement the requirements?
   - Are there edge cases not covered?
   - Could this break existing functionality?

3. **Tests**
   - Are there sufficient tests for the new functionality?
   - Do tests cover edge cases?
   - Are tests clear and maintainable?

4. **Documentation**
   - Is the code properly documented?
   - Are any new features documented for users?
   - Does the PR description clearly explain the changes?

5. **Security**
   - Are there any security implications?
   - For JWT-related code, are all validation steps properly implemented?

### 3. Providing Feedback

When providing feedback:

- Be specific and actionable
- Reference relevant lines of code
- Explain the reasoning behind suggestions
- Distinguish between required changes and nice-to-haves
- Use a constructive, collaborative tone

### 4. Addressing Feedback

PR authors should:

- Respond to all review comments
- Implement required changes
- Discuss alternatives if they disagree with suggestions
- Mark resolved comments as they're addressed
- Request follow-up reviews when changes are complete

### 5. Merging

PRs can be merged when:

- All required changes have been addressed
- Tests are passing
- At least one reviewer has approved the PR
- The branch is up to date with the target branch

## Using Claude for PR Reviews

The Claude PR Reviewer agent uses the following process:

1. Fetches the PR details, including files changed and commit history
2. Examines code changes for quality, style, and potential issues
3. Checks for adherence to project conventions
4. Verifies that tests are included and appropriate
5. Provides a detailed review comment with:
   - Summary of changes
   - Identified issues and suggestions
   - Positive aspects of the PR
   - Recommendations for improvement

Claude PR Reviews should be considered as helpful suggestions rather than definitive assessments. Human reviewers should still perform their own analysis.

## Review Checklist

- [ ] Code follows project style guidelines
- [ ] Tests are included and pass
- [ ] Documentation is updated if needed
- [ ] PR description clearly explains the changes
- [ ] Security considerations have been addressed
- [ ] Performance implications have been considered
- [ ] Claude PR Reviewer feedback has been addressed
- [ ] Changes meet the requirements of the issue
- [ ] Branch is up to date with target branch