#!/usr/bin/env python3
"""
Extract persona information from org-mode files and generate Claude command files.

This script:
1. Reads org-mode persona files
2. Extracts the persona's name, role, background, and communication style
3. Generates a markdown command file for Claude to use that persona

Usage:
  python extract_persona.py --input personas/staff_engineer_jason_wilson.org --output .claude/commands/review-presentation-jason.md
"""

import argparse
import re
import os
from pathlib import Path


def extract_sections(content):
    """Extract main sections from an org-mode file."""
    sections = {}
    
    # Extract the title (first heading)
    title_match = re.search(r'^\* (.*?)$', content, re.MULTILINE)
    if title_match:
        sections['title'] = title_match.group(1).strip()
    
    # Extract key sections
    section_pattern = r'\*\* ([^\n:]+)[\s\S]*?:END:\s*\n([\s\S]*?)(?=\n\*\* |\Z)'
    for match in re.finditer(section_pattern, content, re.MULTILINE):
        section_name = match.group(1).strip()
        section_content = match.group(2).strip()
        sections[section_name.lower()] = section_content
    
    return sections


def generate_command_file(sections, output_path):
    """Generate a Claude command file based on extracted persona information."""
    title = sections.get('title', 'Unknown Persona')
    if '(' in title:
        name, role = title.split('(', 1)
        name = name.strip()
        role = role.replace(')', '').strip()
    else:
        name = title
        role = "Unknown Role"
    
    background = sections.get('background', '')
    
    # Extract key interests
    interests = []
    for section_name in ['interests', 'specific concerns', 'interests in jwt implementations']:
        if section_name in sections:
            interests.append(sections[section_name])
    
    # Get communication style
    communication = sections.get('communication style', '')
    if not communication:
        communication = sections.get('feedback style', '')
    
    # Create command content
    command_name = name.lower().split()[0]
    command_content = f"""---
title: Review JWT Presentation as {name}
description: Review the presentation from the perspective of {name}, {role}
---

I'll now review your JWT presentation from the perspective of {name}, {role}.

{background.split('.')[0]}.

As I review your work, I'll focus on:
"""

    # Add interests if available
    if interests:
        for interest in interests[:1]:  # Use just the first interest section to avoid verbosity
            interest_lines = interest.split('\n')
            for line in interest_lines[:5]:  # Limit to first 5 lines
                clean_line = re.sub(r'^-\s*', '', line.strip())
                if clean_line:
                    command_content += f"\n- {clean_line}"
    
    # Add closing
    command_content += f"""

I'll provide feedback in the communication style of {name}, focusing on the aspects that matter most to me given my background and expertise.

Let me review your presentation now.
"""
    
    # Write to file
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write(command_content)
    
    return command_name


def main():
    parser = argparse.ArgumentParser(description="Generate Claude command files from org-mode persona files")
    parser.add_argument("--input", required=True, help="Input org file")
    parser.add_argument("--output", required=True, help="Output markdown file")
    args = parser.parse_args()
    
    # Read org file
    with open(args.input, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Extract sections
    sections = extract_sections(content)
    
    # Generate command file
    command_name = generate_command_file(sections, args.output)
    
    print(f"Generated command file for {sections.get('title', 'Unknown')} at {args.output}")
    print(f"Use with /review-presentation-{command_name}")


if __name__ == "__main__":
    main()