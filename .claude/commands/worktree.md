# Worktree Task Command

This command instructs Claude Code to check for task information in a git worktree environment.

## Usage

```bash
/worktree
```

## Description

When invoked, this command prompts Claude Code to:

1. Check if the current working directory is in a git worktree
2. Look for a `.worktree/task.org` file
3. Parse and summarize the task information
4. Provide guidance on next steps based on the task details

## Implementation

When this command is executed, Claude Code will:

1. Run `git worktree list` to check if the current directory is a worktree
2. If in a worktree, read the task.org file from the .worktree directory
3. Parse the Org-mode structure to extract:
   - Task title and description
   - Requirements and implementation notes
   - Current status and next steps
4. Summarize the findings and suggest the next actions

## Example Response

```
📋 Worktree Task: Issue #47 - Second Edition of Paradigms Lost

Currently working in: /home/aygp-dr/projects/aygp-dr/jwt-paradigms-wt-pl2ed47

Task Requirements:
- Track second edition of paradigms_lost.org
- Create academic_gadfly_marcus_wellington.org
- Implement support from Zero Chen persona

Next Steps:
1. Review the current paradigms_lost.org structure
2. Create the academic_gadfly_marcus_wellington.org file
3. Integrate Zero Chen persona support

Would you like me to start working on any of these steps?
```

## Notes

- This command is specifically designed for the worktree workflow where task information is stored in `.worktree/task.org`
- The task.org file should follow Org-mode formatting for proper parsing
- The command will provide a concise summary rather than displaying the entire task.org content