#!/usr/bin/env python3
"""
Unified script for processing personas:
1. Extract personas and prompts from org files
2. Generate headshots using Gemini API (if API key is available)
3. Copy generated images to replace placeholders

Usage: uv run scripts/process_personas.py [--force]
"""

import argparse
import os
import re
import glob
import sys
import time
import shutil
from pathlib import Path
import subprocess
import logging

# Configure logging
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger("persona_processor")

# Function from extract_personas.py
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
    
    # 4. Look for "Headshot Generation Prompt" section
    headshot_section_pattern = r'\*\* Headshot Generation Prompt\s*\n(?:.*?:PROPERTIES:.*?:END:\s*\n)?(.*?)(?:\n\*\* |\Z)'
    headshot_section_match = re.search(headshot_section_pattern, content, re.DOTALL)
    
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
    
    # Check for Headshot Generation Prompt section (for newer persona format)
    elif headshot_section_match:
        # Use the filename format for consistency
        # For naming consistency with existing images
        file_name = os.path.basename(file_path)
        base_name = os.path.splitext(file_name)[0]  # Remove .org
        
        # Special cases for specific personas
        if "chief_information_security_officer" in base_name:
            img_filename = "images/rebecca_martinez.png"
        elif "product_manager" in base_name:
            img_filename = "images/sarah_johnson.png"
        elif "vp_sales" in base_name:
            img_filename = "images/jennifer_williams.png"
        else:
            # Use the existing filename pattern
            img_filename = filename
        
        prompt = headshot_section_match.group(1).strip()
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

def extract_personas(path_pattern="*_*_*.org"):
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
                    logger.error(f"Error processing {file_path}: {e}")
    
    return all_personas

def check_api_key():
    """Check if the Gemini API key is set in the environment."""
    api_key = os.environ.get("GEMINI_API_KEY")
    if not api_key:
        logger.warning("GEMINI_API_KEY environment variable not set")
        logger.warning("Image generation will be skipped")
        return False
    return True

def generate_image_with_gemini(prompt, output_path, force=False):
    """Generate an image using Google's Gemini API."""
    try:
        # Import needed libraries
        from io import BytesIO
        from google import genai
        from google.genai import types
        from PIL import Image
        
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
            
        # Get API key from environment
        api_key = os.environ.get("GEMINI_API_KEY")
        if not api_key:
            logger.error("GEMINI_API_KEY environment variable not set")
            return False

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

    except ImportError:
        logger.error("Required packages not installed. Install with:")
        logger.error("pip install pillow google-genai")
        return False
    except Exception as e:
        logger.error(f"Error generating image: {e}")
        return False

def create_placeholders_for_missing_images(personas):
    """Create placeholder images for personas that don't have images yet."""
    created_count = 0
    placeholder_path = Path("images/placeholder.png")
    
    # Ensure placeholder exists
    if not placeholder_path.exists():
        logger.warning("Placeholder image not found at images/placeholder.png")
        return 0
    
    for persona in personas:
        image_path = Path(persona["filename"])
        if not image_path.exists():
            logger.info(f"Creating placeholder for {image_path}")
            
            # Ensure directory exists
            image_path.parent.mkdir(parents=True, exist_ok=True)
            
            # Copy placeholder
            shutil.copy2(placeholder_path, image_path)
            created_count += 1
    
    return created_count

def replace_placeholders():
    """Run the replace_placeholders.sh script to replace placeholder images."""
    script_path = Path("scripts/replace_placeholders.sh")
    if not script_path.exists():
        logger.error(f"Replacement script not found at {script_path}")
        return False
    
    try:
        logger.info("Running placeholder replacement script...")
        subprocess.run(["bash", str(script_path)], check=True)
        return True
    except subprocess.CalledProcessError as e:
        logger.error(f"Error running replacement script: {e}")
        return False

def generate_images_for_personas(personas, force=False):
    """Generate images for all personas using Gemini API."""
    if not check_api_key():
        return 0
    
    # Create output directory
    output_dir = Path("images/gemini")
    output_dir.mkdir(parents=True, exist_ok=True)
    
    success_count = 0
    for persona in personas:
        # Use the filename from the persona data
        filename = Path(persona["filename"])
        
        # Create the output path in the gemini directory
        output_path = output_dir / filename.name
        
        logger.info(f"Processing {persona['persona']}")
        
        success = generate_image_with_gemini(persona["prompt"], output_path, force=force)
        
        if success:
            success_count += 1
            logger.info(f"Successfully processed headshot for {persona['persona']}")
        else:
            logger.error(f"Failed to process headshot for {persona['persona']}")
        
        # Add a delay to avoid rate limits
        time.sleep(2)
    
    return success_count

def main():
    """Main entry point for the script."""
    parser = argparse.ArgumentParser(description="Process personas: extract prompts, generate images, and replace placeholders")
    parser.add_argument(
        "--force", 
        action="store_true", 
        help="Force regeneration of images even if they already exist"
    )
    parser.add_argument(
        "--pattern",
        type=str,
        default="*_*_*.org",
        help="Pattern to match org files (default: *_*_*.org)"
    )
    args = parser.parse_args()
    
    try:
        logger.info("Step 1: Extracting personas from org files")
        personas = extract_personas(args.pattern)
        
        if not personas:
            logger.error("No personas found. Check your org files for proper formatting.")
            return 1
        
        logger.info(f"Found {len(personas)} personas")
        
        logger.info("\nStep 2: Creating placeholders for missing images")
        created = create_placeholders_for_missing_images(personas)
        logger.info(f"Created {created} placeholder images")
        
        logger.info("\nStep 3: Generating images with Gemini API")
        generated = generate_images_for_personas(personas, force=args.force)
        logger.info(f"Generated {generated} images")
        
        logger.info("\nStep 4: Replacing placeholders with generated images")
        replace_placeholders()
        
        logger.info("\nPersona processing complete!")
        return 0
        
    except Exception as e:
        logger.error(f"Error during processing: {e}")
        return 1

if __name__ == "__main__":
    sys.exit(main())