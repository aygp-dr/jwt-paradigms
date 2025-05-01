#!/usr/bin/env python3
"""
Extract and report personas and their image generation prompts from Org-mode files.
"""

import argparse
import sys
import re
import glob
import os
from pathlib import Path

def extract_personas_from_file(file_path):
    """Extract persona information directly from an org-mode file using regex."""
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
    
    # Extract persona name from filename (role_firstname_lastname.org)
    file_name = os.path.basename(file_path)
    base_name = os.path.splitext(file_name)[0]  # Remove .org
    name_parts = base_name.split('_')
    
    # Determine persona name - use the reasonable parts we can extract
    if len(name_parts) >= 3:
        # Format: role_firstname_lastname.org
        first_name = name_parts[1].capitalize()
        last_name = name_parts[2].capitalize()
        persona_name = f"{first_name} {last_name}"
        filename = f"images/{last_name.lower()}.png"
    elif len(name_parts) == 2:
        # Format: role_name.org
        persona_name = name_parts[1].capitalize()
        filename = f"images/{persona_name.lower()}.png"
    else:
        # Fallback
        persona_name = base_name.replace('_', ' ').title()
        filename = f"images/{base_name.lower()}.png"
    
    personas = []
    
    # Try multiple image block formats
    
    # 1. Standard org-ai image blocks with file parameter
    org_ai_pattern = r'#\+begin_ai\s+:image(?:\s+:file\s+([^\s\n]+))?\n(.*?)#\+end_ai'
    org_ai_matches = re.findall(org_ai_pattern, content, re.DOTALL)
    
    # 2. Stable Diffusion/other variants format
    sd_pattern = r'#\+begin_ai\s+:sd-image\s+:file\s+([^\s\n]+)(?:[^\n]*)?\n(.*?)#\+end_ai'
    sd_matches = re.findall(sd_pattern, content, re.DOTALL)
    
    # 3. Check for PERSONA_IMAGE property
    image_prop_pattern = r'#\+PROPERTY:\s+PERSONA_IMAGE\s+([^\s\n]+)'
    image_prop_match = re.search(image_prop_pattern, content)
    
    # Process standard org-ai matches
    if org_ai_matches:
        for match in org_ai_matches:
            # If file parameter is specified, use it
            if match[0]:
                img_filename = match[0]
            else:
                # Fall back to derived filename
                img_filename = filename
                
            prompt = match[1]
            personas.append({
                "persona": persona_name,
                "filename": img_filename,
                "file_path": file_path,
                "prompt": prompt.strip()
            })
    
    # Process SD matches        
    elif sd_matches:
        for match in sd_matches:
            img_filename = match[0]
            prompt = match[1]
            personas.append({
                "persona": persona_name,
                "filename": img_filename,
                "file_path": file_path,
                "prompt": prompt.strip()
            })
            
    # Use PERSONA_IMAGE property if available
    elif image_prop_match:
        # Look for a prompt in a section called "Image"
        img_filename = image_prop_match.group(1)
        image_section_pattern = r'\* Image\s*\n(.*?)(?:\n\* |$)'
        image_section_match = re.search(image_section_pattern, content, re.DOTALL)
        
        if image_section_match:
            prompt = image_section_match.group(1).strip()
            # Extract the prompt from the section
            ai_block_pattern = r'#\+begin_ai[^\n]*\n(.*?)#\+end_ai'
            ai_block_match = re.search(ai_block_pattern, prompt, re.DOTALL)
            
            if ai_block_match:
                prompt = ai_block_match.group(1).strip()
            
            personas.append({
                "persona": persona_name,
                "filename": img_filename,
                "file_path": file_path,
                "prompt": prompt
            })
    
    # Fallback to legacy gptel format
    if not personas:
        gptel_block_pattern = r'#\+begin_src gptel :file\s+([^\n]+?)\n(.*?)#\+end_src'
        gptel_matches = re.findall(gptel_block_pattern, content, re.DOTALL)
        
        if gptel_matches:
            for file_ref, prompt in gptel_matches:
                personas.append({
                    "persona": persona_name,
                    "filename": file_ref.strip(),
                    "file_path": file_path,
                    "prompt": prompt.strip()
                })
    
    return personas

def extract_personas(path_pattern):
    """Extract persona information from multiple org files matching a pattern."""
    all_personas = []
    
    # Handle single file or directory pattern
    if os.path.isfile(path_pattern):
        # Single file mode
        all_personas.extend(extract_personas_from_file(path_pattern))
    else:
        # Use glob to find all matching files
        if '*' not in path_pattern:
            # If no wildcard is provided, assume directory and add pattern
            if os.path.isdir(path_pattern):
                path_pattern = os.path.join(path_pattern, '*_*_*.org')
            else:
                # Default to current directory with pattern
                path_pattern = '*_*_*.org'
        
        matching_files = glob.glob(path_pattern)
        
        for file_path in matching_files:
            if file_path.endswith('.org'):
                try:
                    file_personas = extract_personas_from_file(file_path)
                    all_personas.extend(file_personas)
                except Exception as e:
                    print(f"Error processing {file_path}: {e}", file=sys.stderr)
    
    return all_personas

def main():
    parser = argparse.ArgumentParser(description="Extract and report personas from org-mode files")
    parser.add_argument(
        "--org-file", 
        help="Input org-mode file or glob pattern (e.g., '*_*_*.org')",
        default="*_*_*.org"
    )
    parser.add_argument(
        "--output-json",
        help="Output JSON file to write persona data"
    )
    
    args = parser.parse_args()
    
    try:
        personas = extract_personas(args.org_file)
        
        print(f"Found {len(personas)} personas:\n")
        
        for i, item in enumerate(personas, 1):
            persona_name = item["persona"]
            filename = item["filename"]
            source_file = item["file_path"]
            
            # Clean the filename path to just the basename
            simple_filename = os.path.basename(filename)
            source_basename = os.path.basename(source_file)
            
            # Get a short prompt preview (first 50 chars)
            prompt_preview = item["prompt"][:50] + "..." if len(item["prompt"]) > 50 else item["prompt"]
            
            print(f"{i}. {persona_name}")
            print(f"   Source: {source_basename}")
            print(f"   Image: {simple_filename}")
            print(f"   Prompt preview: {prompt_preview}")
            print()
        
        # Optional JSON output
        if args.output_json:
            import json
            with open(args.output_json, 'w', encoding='utf-8') as f:
                json.dump(personas, f, indent=2)
            print(f"Wrote persona data to {args.output_json}")
    
    except Exception as e:
        print(f"Error processing: {e}", file=sys.stderr)
        return 1
    
    return 0

if __name__ == "__main__":
    exit(main())
