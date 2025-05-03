#!/usr/bin/env python3
"""
Script for generating persona headshots using Google's Gemini API.
This script extracts prompts directly from persona org files instead of using hardcoded values.
"""

import logging
import os
import sys
import time
import argparse
import glob
import re
from pathlib import Path

try:
    # For image processing
    from io import BytesIO

    # For Gemini API
    from google import genai
    from google.genai import types
    from PIL import Image
except ImportError:
    print("Missing required packages. Please install with:")
    print("pip install pillow google-genai")
    sys.exit(1)

# Configure logging
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger("gemini_headshots_generator")


def check_api_key():
    """Check if the Gemini API key is set in the environment."""
    api_key = os.environ.get("GEMINI_API_KEY")
    if not api_key:
        logger.error("GEMINI_API_KEY environment variable not set")
        logger.error("Please set this variable in your .env file or directly in your environment")
        return False
    return True


def generate_image(prompt, output_path, force=False):
    """Generate an image using Google's Gemini API."""
    # Skip generation if file exists and force is False
    output_path = Path(output_path)
    if output_path.exists() and not force:
        filesize = output_path.stat().st_size
        # If the file is very small (likely a placeholder), regenerate it
        if filesize < 5000:  # Less than 5KB is probably a placeholder
            logger.info(f"Existing image at {output_path} appears to be a placeholder ({filesize} bytes). Will regenerate.")
        else:
            logger.info(f"Image already exists at {output_path} ({filesize} bytes). Skipping (use --force to override)")
            return True
        
    try:
        # Get API key from environment
        api_key = os.environ.get("GEMINI_API_KEY")

        # Initialize the client with API key
        client = genai.Client(api_key=api_key)

        logger.info(f"Generating image: {output_path}")
        logger.info(f"Using prompt: {prompt[:50]}...")

        # Generate the image using Imagen model
        response = client.models.generate_images(
            model="imagen-3.0-generate-002",
            prompt=prompt,
            config=types.GenerateImagesConfig(
                number_of_images=1,
            ),
        )

        # Save the image
        image = Image.open(BytesIO(response.generated_images[0].image.image_bytes))
        output_path.parent.mkdir(parents=True, exist_ok=True)  # Ensure output directory exists
        
        # Resize to 400x400 while preserving aspect ratio
        image = image.resize((400, 400), Image.LANCZOS)
        image.save(output_path)

        logger.info(f"Image saved to {output_path}")
        return True

    except Exception as e:
        logger.error(f"Error generating image: {e}")
        return False


def extract_persona_from_file(file_path):
    """Extract persona information directly from an org-mode file using regex."""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Extract persona name from filename (role_firstname_lastname.org)
        file_name = os.path.basename(file_path)
        base_name = os.path.splitext(file_name)[0]  # Remove .org
        name_parts = base_name.split('_')
        
        # Determine persona name from filename components
        if len(name_parts) >= 3:
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
        
        # Extract role from file or derive from filename
        role_pattern = r'\* (.*?)\s*\n'
        role_match = re.search(role_pattern, content)
        role = role_match.group(1) if role_match else name_parts[0].replace('_', ' ').title()
        
        # Look for begin_ai with :file parameter
        file_pattern = r'#\+begin_ai\s+:image\s+:file\s+([^\s\n]+)'
        file_match = re.search(file_pattern, content)
        
        if file_match:
            image_filename = file_match.group(1)
        else:
            # If no file specified in begin_ai, don't process this persona
            logger.warning(f"No :file parameter specified in begin_ai block for {file_path}")
            return None
        
        # Extract prompt from begin_ai block
        prompt_pattern = r'#\+begin_ai\s+:image(?:\s+:file\s+[^\s\n]+)?\n(.*?)#\+end_ai'
        prompt_match = re.search(prompt_pattern, content, re.DOTALL)
        
        if prompt_match:
            prompt = prompt_match.group(1).strip()
            
            return {
                "name": persona_name,
                "role": role,
                "filename": image_filename,
                "file_path": file_path,
                "prompt": prompt
            }
        
        logger.warning(f"No prompt found in begin_ai block in {file_path}")
        return None
        
    except Exception as e:
        logger.error(f"Error extracting persona from {file_path}: {e}")
        return None


def extract_personas(org_dir=None, pattern=None):
    """Extract personas from org files."""
    personas = []
    
    if org_dir is None:
        org_dir = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    
    if pattern is None:
        pattern = "*_*_*.org"
    
    # Use glob to find all org files
    org_pattern = os.path.join(org_dir, pattern)
    org_files = glob.glob(org_pattern)
    
    logger.info(f"Found {len(org_files)} org files matching pattern {org_pattern}")
    
    for file_path in org_files:
        # Skip files that don't look like persona files
        if "README" in file_path or "CLAUDE" in file_path:
            continue
            
        persona = extract_persona_from_file(file_path)
        if persona:
            personas.append(persona)
            logger.info(f"Extracted persona: {persona['name']} from {os.path.basename(file_path)}")
    
    return personas


def main():
    """Main entry point for the script."""
    # Set up argument parser
    parser = argparse.ArgumentParser(description="Generate headshots using Google's Gemini API")
    parser.add_argument(
        "--force", 
        action="store_true", 
        help="Force regeneration of images even if they already exist"
    )
    parser.add_argument(
        "--persona",
        type=str,
        help="Generate headshot only for the specified persona (by name or filename without extension)"
    )
    parser.add_argument(
        "--org-dir",
        type=str,
        help="Directory containing org files (default: parent directory of this script)"
    )
    parser.add_argument(
        "--pattern",
        type=str,
        default="*_*_*.org",
        help="Pattern to match org files (default: *_*_*.org)"
    )
    parser.add_argument(
        "--output-dir",
        type=str,
        default="images/gemini",
        help="Directory where generated images will be saved"
    )
    parser.add_argument(
        "--target-dir",
        type=str,
        default="images",
        help="Directory where final images should be placed (after processing)"
    )
    parser.add_argument(
        "--copy-to-target",
        action="store_true",
        help="Copy generated images to the target directory automatically"
    )
    args = parser.parse_args()
    
    logger.info("Starting Gemini headshots generation")
    logger.info(f"Force mode: {'ON' if args.force else 'OFF'}")

    # Check for API key
    if not check_api_key():
        return 1

    # Ensure output directory exists
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    # Extract personas from org files
    personas = extract_personas(args.org_dir, args.pattern)
    
    if not personas:
        logger.error("No personas found in org files")
        return 1
    
    logger.info(f"Found {len(personas)} personas with prompts")

    # Filter personas if a specific one was requested
    if args.persona:
        target = args.persona.lower()
        # Try to match by name or filename
        personas_to_process = [p for p in personas if 
                             target in p["name"].lower() or 
                             target in os.path.basename(p["file_path"]).lower()]
        if not personas_to_process:
            logger.error(f"No persona found matching '{args.persona}'")
            return 1
        logger.info(f"Processing only persona(s) matching: {args.persona}")
    else:
        personas_to_process = personas

    # Process each persona
    success_count = 0
    for persona in personas_to_process:
        # Extract filename from path
        filename = os.path.basename(persona["filename"])
        output_path = output_dir / filename

        logger.info(f"Processing {persona['name']} ({persona['role']})")

        success = generate_image(persona["prompt"], output_path, force=args.force)

        if success:
            success_count += 1
            logger.info(f"Successfully processed headshot for {persona['name']}")
            
            # Optionally copy to target directory
            if args.copy_to_target:
                target_path = Path(args.target_dir) / filename
                try:
                    import shutil
                    target_path.parent.mkdir(parents=True, exist_ok=True)  # Ensure target directory exists
                    shutil.copy2(output_path, target_path)
                    logger.info(f"Copied to {target_path}")
                except Exception as e:
                    logger.error(f"Error copying to target directory: {e}")
        else:
            logger.error(f"Failed to process headshot for {persona['name']}")

        # Add a delay to avoid rate limits
        time.sleep(2)

    logger.info(f"Completed processing of {success_count}/{len(personas_to_process)} headshots")

    return 0


if __name__ == "__main__":
    sys.exit(main())