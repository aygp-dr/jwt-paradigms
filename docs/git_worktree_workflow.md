# Git Worktree Workflow

This document describes the standardized Git worktree workflow for parallel development tasks in this project.

## Overview

Git worktrees allow you to check out multiple branches from the same repository in separate directories. This enables concurrent work on different features or issues without having to stash changes or switch branches in a single working directory.

## Workflow

### 1. Naming Convention

Worktrees follow this naming convention:
```
{repo}-wt-{branch-name}
```

Where:
- `{repo}` is the original repository name
- `-wt-` indicates it's a worktree
- `{branch-name}` includes the issue number and a brief description

Example: `jwt-paradigms-wt-validate49` for issue #49 about validating the worktree workflow.

### 2. Creating a Worktree

```bash
# In the main repository
# 1. Create a feature branch
git checkout -b feature-issue42

# 2. Create worktree with proper naming
git worktree add ../jwt-paradigms-wt-feature42 -b feature-issue42-wt feature-issue42

# 3. Initialize task tracking
mkdir -p ../jwt-paradigms-wt-feature42/.worktree

# 4. Create task.org with issue details
cat > ../jwt-paradigms-wt-feature42/.worktree/task.org << 'EOF'
#+TITLE: Feature Name - Issue 42
#+AUTHOR: aygp-dr
#+DATE: $(date +%Y-%m-%d)
#+PROPERTY: header-args :mkdirp yes

* Task Summary
Brief description of the task for issue #42.

** Requirements
- List key requirements here
- Add implementation goals

** Implementation Notes
- Working in branch: feature-issue42-wt
- Relevant files and components

** Validation Steps
1. Step one
2. Step two
3. Step three

** References
- Issue #42: https://github.com/aygp-dr/jwt-paradigms/issues/42
EOF
```

### 3. Working in a Worktree

```bash
# Navigate to the worktree
cd ../jwt-paradigms-wt-feature42

# Make changes, commit, and push as normal
git add .
git commit -m "feat: implement feature for #42"
git push -u origin feature-issue42-wt
```

### 4. Pull Request Process

```bash
# Create a pull request
gh pr create --title "Implement feature for #42" --body "
## Summary
- Brief description of the changes
- Why they are needed
- Key implementation details

## Testing Done
- How changes were tested
- Results of testing

Fixes #42
"
```

### 5. PR Review Process

1. Assign at least one reviewer to the PR
2. The reviewer examines:
   - Code quality and style
   - Test coverage
   - Documentation
   - Compliance with requirements
3. Use the Claude PR Reviewer agent to get initial feedback
4. Address all feedback before merging

### 6. Cleanup

```bash
# After PR is merged
cd /path/to/main/repo
git checkout main
git pull
git branch -d feature-issue42
git worktree remove ../jwt-paradigms-wt-feature42
```

## Task Tracking

Each worktree contains a `.worktree/task.org` file that tracks:

1. Task title and issue number
2. Requirements
3. Implementation notes
4. Validation steps
5. References

This information helps Claude Code and developers understand the purpose and context of the worktree.

## Detection

Claude Code can automatically detect worktrees by:
1. Checking if the current path matches the pattern `{repo}-wt-{branch}`
2. Looking for the `.worktree/task.org` file
3. Using `git worktree list` to confirm status

## Commands

| Command | Description |
|---------|-------------|
| `git worktree list` | Show all worktrees |
| `git worktree add <path> <branch>` | Create a new worktree |
| `git worktree remove <path>` | Remove a worktree |
| `/worktree` | Claude Code command to check worktree task info |