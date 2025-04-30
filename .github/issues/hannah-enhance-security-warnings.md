---
title: Enhance security warnings in JWT parsing examples
labels: security, documentation, financial-security
assignees: aygp-dr
---

## Description
In reviewing the presentation materials as a security architect, I noticed that while comments warn against using unverified parsing in production, the actual code examples still demonstrate the unsafe approach first. This could lead to copy-paste security issues.

Recommendations:
- Replace all unsafe parsing examples with secure-by-default implementations
- Add visual indicators (red borders/backgrounds) to clearly mark insecure code
- Include concrete examples of exploitation resulting from unsafe parsing
- Create paired examples showing vulnerable code alongside secure alternatives

The current approach, while educational, risks developers implementing the simplest solution they see first.

## Reviewer
Hannah Chen (Application Security Architect)