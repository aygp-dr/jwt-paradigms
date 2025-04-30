#!/usr/bin/env python3
"""
Extract and report personas and their image generation prompts from an Org-mode file.
"""

import argparse
import orgparse
import sys
import re

def extract_personas(org_file_path):
    """Extract persona information from an org-mode file using orgparse."""
    # Parse the org file
    org = orgparse.load(org_file_path)
    
    personas = []
    
    # Iterate through all nodes (headings)
    for node in org[1:]:  # Skip the root node
        # Skip the Introduction or any other non-persona sections
        if node.heading == "Introduction":
            continue
        
        # Extract the prompt from the body text
        # Look for the gptel block pattern in the body
        gptel_block_pattern = r'#\+begin_src gptel :file (.+?)\n(.*?)#\+end_src'
        matches = re.findall(gptel_block_pattern, node.body, re.DOTALL)
        
        if matches:
            for filename, prompt in matches:
                personas.append({
                    "persona": node.heading,
                    "filename": filename.strip(),
                    "prompt": prompt.strip()
                })
    
    return personas

def main():
    parser = argparse.ArgumentParser(description="Extract and report personas from org-mode file")
    parser.add_argument(
        "--org-file", 
        required=True,
        help="Input org-mode file"
    )
    
    args = parser.parse_args()
    
    try:
        personas = extract_personas(args.org_file)
        
        print(f"Found {len(personas)} personas:\n")
        
        for i, item in enumerate(personas, 1):
            persona_name = item["persona"]
            filename = item["filename"]
            
            # Clean the filename path to just the basename
            simple_filename = filename.split("/")[-1]
            
            # Get a short prompt preview (first 50 chars)
            prompt_preview = item["prompt"][:50] + "..." if len(item["prompt"]) > 50 else item["prompt"]
            
            print(f"{i}. {persona_name}")
            print(f"   File: {simple_filename}")
            print(f"   Prompt preview: {prompt_preview}")
            print()
    
    except Exception as e:
        print(f"Error processing file: {e}", file=sys.stderr)
        return 1
    
    return 0

if __name__ == "__main__":
    exit(main())
