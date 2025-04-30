#!/usr/bin/env python3
"""
Extract persona information from org files and generate Claude command files.

This script parses org-mode files containing persona definitions
and converts them to markdown files for use with Claude AI commands.
"""

import argparse
import logging
import os
import re
import sys
from pathlib import Path
from typing import Dict

# Configure logging
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger("extract_persona")


def setup_args() -> argparse.Namespace:
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(description="Extract personas from org files")
    parser.add_argument("--input", "-i", type=str, required=True, help="Input org file")
    parser.add_argument("--output", "-o", type=str, required=True, help="Output markdown file")
    parser.add_argument(
        "--format",
        "-f",
        type=str,
        default="claude",
        choices=["claude", "markdown", "json"],
        help="Output format",
    )
    return parser.parse_args()


def extract_persona_from_org(org_file: str) -> Dict[str, str]:
    """
    Parse an org-mode file and extract persona information.
    Returns a dictionary with persona details.
    """
    persona = {
        "name": "",
        "role": "",
        "description": "",
        "behaviors": [],
        "expertise": [],
        "concerns": [],
        "speaking_style": "",
        "image_path": "",
    }

    try:
        # Load the org file content
        with open(org_file, "r", encoding="utf-8") as f:
            content = f.read()

        # Extract title (possibly persona name)
        title_match = re.search(r"#\+TITLE:\s*(.+?)$", content, re.MULTILINE)
        if title_match:
            persona["name"] = title_match.group(1).strip()

        # Extract headings and content
        sections = re.split(r"^\*+\s+", content, flags=re.MULTILINE)[1:]

        for section in sections:
            lines = section.strip().split("\n")
            if not lines:
                continue

            heading = lines[0].strip()
            content = "\n".join(lines[1:]).strip()

            # Skip empty sections
            if not content:
                continue

            # Map headings to persona attributes
            if re.match(r"(?i)name|title|persona", heading):
                persona["name"] = content
            elif re.match(r"(?i)role|position|job", heading):
                persona["role"] = content
            elif re.match(r"(?i)description|summary|overview", heading):
                persona["description"] = content
            elif re.match(r"(?i)behavior|approach|style", heading):
                behaviors = [b.strip() for b in re.split(r"[-*+]\s+", content) if b.strip()]
                persona["behaviors"].extend(behaviors)
            elif re.match(r"(?i)expertise|skills|knowledge", heading):
                expertise = [e.strip() for e in re.split(r"[-*+]\s+", content) if e.strip()]
                persona["expertise"].extend(expertise)
            elif re.match(r"(?i)concerns|focus|priorities", heading):
                concerns = [c.strip() for c in re.split(r"[-*+]\s+", content) if c.strip()]
                persona["concerns"].extend(concerns)
            elif re.match(r"(?i)speak|talk|voice|communication", heading):
                persona["speaking_style"] = content

        # Look for image paths in src blocks
        image_match = re.search(
            r"#\+begin_src.+?:file\s+([^\s]+\.png)", content, re.IGNORECASE | re.DOTALL
        )
        if image_match:
            persona["image_path"] = image_match.group(1)

    except Exception as e:
        logger.error(f"Error parsing org file: {e}")
        return {}

    return persona


def generate_claude_command(
    persona: Dict[str, str], output_file: str, org_file: str = None
) -> bool:
    """
    Generate a Claude command file from persona information.

    Args:
        persona: Dictionary containing persona information
        output_file: Path to the output file
        org_file: Path to the original org file (for resolving image paths)
    """
    try:
        # Create Claude command format
        with open(output_file, "w", encoding="utf-8") as f:
            f.write(f"# Review as {persona['name']}\n\n")

            # Add role description
            if persona["role"]:
                f.write(f"## Role\n{persona['role']}\n\n")

            # Add full description
            if persona["description"]:
                f.write(f"## Description\n{persona['description']}\n\n")

            # Add expertise
            if persona["expertise"]:
                f.write("## Expertise\n")
                for exp in persona["expertise"]:
                    f.write(f"- {exp}\n")
                f.write("\n")

            # Add concerns
            if persona["concerns"]:
                f.write("## Main Concerns\n")
                for concern in persona["concerns"]:
                    f.write(f"- {concern}\n")
                f.write("\n")

            # Add behaviors
            if persona["behaviors"]:
                f.write("## Typical Behaviors\n")
                for behavior in persona["behaviors"]:
                    f.write(f"- {behavior}\n")
                f.write("\n")

            # Add speaking style
            if persona["speaking_style"]:
                f.write(f"## Speaking Style\n{persona['speaking_style']}\n\n")

            # Add review instruction
            f.write("---\n\n")
            f.write(f"Please review the following code or documentation as {persona['name']}. ")
            f.write(
                "Focus on providing constructive feedback that reflects the expertise, concerns, "
                "and "
            )
            f.write("communication style described above. ")
            f.write("Maintain this persona's perspective consistently throughout the review.\n\n")

            # Add image reference if available
            if persona["image_path"]:
                # Make sure the image path is relative to the output file
                image_path = persona["image_path"]
                if not os.path.isabs(image_path):
                    # If it's a relative path from the org file, adjust accordingly
                    org_dir = os.path.dirname(org_file) if os.path.dirname(org_file) else "."
                    abs_image_path = os.path.join(org_dir, image_path)
                    if os.path.exists(abs_image_path):
                        rel_path = os.path.relpath(abs_image_path, os.path.dirname(output_file))
                        f.write(f"![{persona['name']}]({rel_path})\n")
                else:
                    # If it's an absolute path, try to make it relative
                    if os.path.exists(image_path):
                        rel_path = os.path.relpath(image_path, os.path.dirname(output_file))
                        f.write(f"![{persona['name']}]({rel_path})\n")

        logger.info(f"Generated Claude command file: {output_file}")
        return True

    except Exception as e:
        logger.error(f"Error generating Claude command file: {e}")
        return False


def main():
    """Main entry point for the script."""
    args = setup_args()

    # Extract persona from org file
    persona = extract_persona_from_org(args.input)
    if not persona or not persona["name"]:
        logger.error(f"Failed to extract persona from {args.input}")
        return 1

    # Ensure output directory exists
    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    # Generate output file
    if args.format == "claude":
        success = generate_claude_command(persona, args.output, args.input)
    elif args.format == "markdown":
        # TODO: Implement markdown format
        logger.error("Markdown format not yet implemented")
        return 1
    elif args.format == "json":
        # TODO: Implement JSON format
        logger.error("JSON format not yet implemented")
        return 1
    else:
        logger.error(f"Unknown format: {args.format}")
        return 1

    if not success:
        logger.error(f"Failed to generate output file: {args.output}")
        return 1

    return 0


if __name__ == "__main__":
    sys.exit(main())
