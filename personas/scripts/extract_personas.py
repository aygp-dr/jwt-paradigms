#!/usr/bin/env python3
"""
Extract and report personas and their image generation prompts from Org-mode files.
"""

import argparse
import sys
import re
import glob
import os
import logging
from pathlib import Path

# Configure logging
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger("persona_extractor")

def extract_personas_from_file(file_path):
    """Extract persona information directly from an org-mode file using regex."""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Extract persona name from filename (role_firstname_lastname.org)
        file_name = os.path.basename(file_path)
        base_name = os.path.splitext(file_name)[0]  # Remove .org extension
        name_parts = base_name.split('_')
        
        # Extract org-mode title if available
        title_pattern = r'#\+TITLE:\s*(.*?)(?:\n|\Z)'
        title_match = re.search(title_pattern, content)
        
        # Extract first heading as a fallback title
        heading_pattern = r'\*\s+(.*?)(?:\n|\Z)'
        heading_match = re.search(heading_pattern, content)
        
        # Use title, heading, or filename parts to determine the persona name
        if title_match:
            persona_name = title_match.group(1).strip()
        elif heading_match:
            persona_name = heading_match.group(1).strip()
            # Remove the "(Role)" part if present
            persona_name = re.sub(r'\(.*?\)', '', persona_name).strip()
        elif len(name_parts) >= 3:
            # Format: role_firstname_lastname.org
            first_name = name_parts[1].capitalize()
            last_name = name_parts[2].capitalize()
            persona_name = f"{first_name} {last_name}"
        elif len(name_parts) == 2:
            # Format: role_name.org
            persona_name = name_parts[1].capitalize()
        else:
            # Fallback
            persona_name = base_name.replace('_', ' ').title()
            
        # Determine default filename based on name parts
        if len(name_parts) >= 3:
            # Format: images/lastname.png or images/firstname_lastname.png for executives
            last_name = name_parts[-1].lower()
            if any(role in name_parts[0].lower() for role in ["chief", "officer", "ceo", "cto", "cfo", "cio", "ciso"]):
                # Use both first and last name for executives to disambiguate
                default_filename = f"images/{name_parts[1].lower()}_{last_name}.png"
            else:
                default_filename = f"images/{last_name}.png"
        else:
            # Fallback to a general pattern
            default_filename = f"images/{name_parts[-1].lower()}.png"
        
        # Determine role from file or derive from filename
        if heading_match:
            # Extract role from parentheses in heading if present
            role_in_heading = re.search(r'\((.*?)\)', heading_match.group(1))
            if role_in_heading:
                role = role_in_heading.group(1).strip()
            else:
                role = name_parts[0].replace('_', ' ').title()
        else:
            role = name_parts[0].replace('_', ' ').title()
        
        personas = []
        
        # Various patterns to find image prompts
        # 1. Find begin_ai blocks with image parameter
        begin_ai_image_patterns = [
            # Standard org-ai image block with optional file parameter
            r'#\+begin_ai\s+:image(?:\s+:file\s+([^\s\n]+))?\n(.*?)#\+end_ai',
            # SD-image variant
            r'#\+begin_ai\s+:sd-image(?:\s+:file\s+([^\s\n]+))?\n(.*?)#\+end_ai',
            # Any begin_ai block under a "Headshot Generation Prompt" heading
            r'\*\* Headshot Generation Prompt.*?\n(?:.*?)#\+begin_ai(?:\s+:image)?(?:\s+:file\s+([^\s\n]+))?\n(.*?)#\+end_ai'
        ]
        
        # Try all patterns for begin_ai blocks
        for pattern in begin_ai_image_patterns:
            matches = re.findall(pattern, content, re.DOTALL)
            if matches:
                for match in matches:
                    # First capture group is file path (may be empty)
                    # Second capture group is prompt
                    if match[0]:  # If file parameter specified
                        img_filename = match[0]
                    else:
                        # Skip if no file parameter specified
                        logger.info(f"Skipping prompt in {file_path} - no :file parameter specified")
                        continue
                    
                    prompt = match[1].strip()
                    
                    personas.append({
                        "persona": persona_name,
                        "role": role,
                        "filename": img_filename,
                        "file_path": file_path,
                        "prompt": prompt,
                        "source": "begin_ai"
                    })
        
        # No fallback for headshot sections - only use begin_ai blocks
        if not personas:
            logger.info(f"No valid begin_ai blocks with :file parameters found in {file_path}")
        
        # Skip PERSONA_IMAGE property handling - only use begin_ai blocks
        
        # Skip legacy gptel format - only use begin_ai blocks
        
        if not personas:
            logger.warning(f"No prompt found in {file_path}")
        else:
            logger.info(f"Successfully extracted {len(personas)} prompts from {file_path}")
            
        return personas
    
    except Exception as e:
        logger.error(f"Error extracting from {file_path}: {e}")
        return []

def extract_personas(path_pattern="*_*_*.org", org_dir=None):
    """Extract persona information from multiple org files matching a pattern."""
    all_personas = []
    
    # Handle single file mode
    if os.path.isfile(path_pattern):
        logger.info(f"Processing single file: {path_pattern}")
        all_personas.extend(extract_personas_from_file(path_pattern))
    else:
        # Directory mode with pattern
        if org_dir:
            if os.path.isdir(org_dir):
                # If org_dir is specified, use that as the base directory
                full_pattern = os.path.join(org_dir, path_pattern)
            else:
                logger.warning(f"Specified org directory '{org_dir}' is not a directory, using current directory")
                full_pattern = path_pattern
        else:
            # Default to current directory or use provided pattern
            full_pattern = path_pattern
            
        logger.info(f"Searching for org files matching: {full_pattern}")
        matching_files = glob.glob(full_pattern)
        
        if not matching_files:
            logger.warning(f"No files found matching pattern: {full_pattern}")
            return []
            
        logger.info(f"Found {len(matching_files)} matching files")
        
        for file_path in matching_files:
            if file_path.endswith('.org'):
                try:
                    file_personas = extract_personas_from_file(file_path)
                    all_personas.extend(file_personas)
                except Exception as e:
                    logger.error(f"Error processing {file_path}: {e}")
    
    logger.info(f"Total personas extracted: {len(all_personas)}")
    return all_personas

def main():
    parser = argparse.ArgumentParser(description="Extract and report personas from org-mode files")
    parser.add_argument(
        "--org-dir", 
        help="Directory containing org files",
        default=None
    )
    parser.add_argument(
        "--pattern", 
        help="Pattern to match org files (e.g., '*_*_*.org')",
        default="*_*_*.org"
    )
    parser.add_argument(
        "--persona", 
        help="Filter results to match a specific persona name",
        default=None
    )
    parser.add_argument(
        "--output-json",
        help="Output JSON file to write persona data"
    )
    parser.add_argument(
        "--verbose", "-v",
        action="store_true",
        help="Enable verbose logging"
    )
    
    args = parser.parse_args()
    
    # Set verbose logging if requested
    if args.verbose:
        logging.getLogger().setLevel(logging.DEBUG)
    
    try:
        logger.info("Starting persona extraction")
        personas = extract_personas(args.pattern, args.org_dir)
        
        # Filter by persona name if specified
        if args.persona and personas:
            search_term = args.persona.lower()
            personas = [p for p in personas if search_term in p["persona"].lower()]
            logger.info(f"Filtered to {len(personas)} personas matching '{args.persona}'")
        
        print(f"\nFound {len(personas)} personas:\n")
        
        for i, item in enumerate(personas, 1):
            persona_name = item["persona"]
            role = item.get("role", "Unknown Role")
            filename = item["filename"]
            source_file = item["file_path"]
            extraction_source = item.get("source", "unknown")
            
            # Clean the filename path to just the basename
            simple_filename = os.path.basename(filename)
            source_basename = os.path.basename(source_file)
            
            # Get a short prompt preview (first 50 chars)
            prompt_preview = item["prompt"][:50] + "..." if len(item["prompt"]) > 50 else item["prompt"]
            
            print(f"{i}. {persona_name} ({role})")
            print(f"   Source: {source_basename} (extracted via {extraction_source})")
            print(f"   Image: {simple_filename}")
            print(f"   Prompt preview: {prompt_preview}")
            print()
        
        # Optional JSON output
        if args.output_json:
            import json
            with open(args.output_json, 'w', encoding='utf-8') as f:
                json.dump(personas, f, indent=2)
            logger.info(f"Wrote persona data to {args.output_json}")
    
    except Exception as e:
        logger.error(f"Error processing: {e}")
        return 1
    
    return 0

if __name__ == "__main__":
    exit(main())
