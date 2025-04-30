#!/usr/bin/env python3
"""
Extract personas and their image generation prompts from an Org-mode file.
Can be called from a Makefile with:
  $(PYTHON) $(SCRIPTS_DIR)/extract_personas.py --org-file generate-persona-images.org
"""

import argparse
import orgparse
import os
import json
from pathlib import Path


def extract_personas(org_file_path):
    """
    Extract persona information from an org-mode file using orgparse.
    
    Args:
        org_file_path (str): Path to the org-mode file
    
    Returns:
        list: List of dictionaries with persona, filename, and prompt
    """
    # Parse the org file
    org = orgparse.load(org_file_path)
    
    personas = []
    
    # Iterate through all nodes (headings)
    for node in org[1:]:  # Skip the root node
        # Skip the Introduction or any other non-persona sections
        if node.heading == "Introduction":
            continue
            
        # Look for gptel source blocks
        for src_block in node.get_source_blocks():
            if src_block.get("language") == "gptel":
                # Extract the filename from the parameters
                params = src_block.get("parameters", "")
                file_param = next((p for p in params.split() if p.startswith(":file")), None)
                
                if file_param:
                    filename = file_param.split(" ", 1)[1]
                    prompt = src_block.get("contents", "").strip()
                    
                    personas.append({
                        "persona": node.heading,
                        "filename": filename,
                        "prompt": prompt
                    })
                    break  # Assume one gptel block per persona
    
    return personas


def ensure_directory_exists(filename):
    """Create directory if it doesn't exist for a given filename"""
    directory = os.path.dirname(filename)
    if directory and not os.path.exists(directory):
        os.makedirs(directory, exist_ok=True)


def main():
    parser = argparse.ArgumentParser(description="Extract personas from org-mode file")
    parser.add_argument(
        "--org-file", 
        required=True,
        help="Input org-mode file"
    )
    parser.add_argument(
        "--output-dir",
        default="personas",
        help="Output directory for JSON files (default: personas)"
    )
    parser.add_argument(
        "--format", 
        choices=["text", "json"], 
        default="json",
        help="Output format (default: json)"
    )
    
    args = parser.parse_args()
    
    try:
        personas = extract_personas(args.org_file)
        
        if args.format == "text":
            print(f"Found {len(personas)} personas with prompts:\n")
            for item in personas:
                print(f"Persona: {item['persona']}")
                print(f"Filename: {item['filename']}")
                print(f"Prompt: {item['prompt']}")
                print("-" * 80)
        else:  # json format
            output_dir = args.output_dir
            os.makedirs(output_dir, exist_ok=True)
            
            # Save a combined file with all personas
            combined_file = Path(output_dir) / "all_personas.json"
            with open(combined_file, "w") as f:
                json.dump(personas, f, indent=2)
            
            # Also save individual files
            for item in personas:
                # Create a safe filename from the persona name
                safe_name = item['persona'].lower().replace(" ", "_").replace('"', '').replace("'", "").replace("(", "").replace(")", "")
                output_file = Path(output_dir) / f"{safe_name}.json"
                
                # Ensure the directory exists
                ensure_directory_exists(str(output_file))
                
                with open(output_file, "w") as f:
                    json.dump(item, f, indent=2)
            
            print(f"Saved {len(personas)} persona JSON files to {output_dir}/")
            print(f"Combined data saved to {combined_file}")
            
            # Generate a report of the extracted data
            print("\nExtracted Personas:")
            for i, item in enumerate(personas, 1):
                print(f"{i}. {item['persona']} → {item['filename']}")
    
    except Exception as e:
        print(f"Error processing file: {e}")
        return 1
    
    return 0


if __name__ == "__main__":
    exit(main())
