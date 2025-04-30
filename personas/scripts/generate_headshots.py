#!/usr/bin/env python3
"""
Script for generating persona headshots using Google's Gemini API.
This script uses persona definitions from the persona_prompts module.
"""

import logging
import os
import sys
import time
import argparse
from pathlib import Path

try:
    # For image processing
    from io import BytesIO

    # For Gemini API
    from google import genai
    from google.genai import types

    # Import persona data
    from persona_prompts import PERSONAS
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
        help="Generate headshot only for the specified persona (by ID or filename without extension)"
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

    # Filter personas if a specific one was requested
    personas_to_process = PERSONAS
    if args.persona:
        target = args.persona
        # Try to match by ID or filename without extension
        personas_to_process = [p for p in PERSONAS if p["id"] == target or p["filename"].split('.')[0] == target]
        if not personas_to_process:
            logger.error(f"No persona found with ID or filename '{target}'")
            return 1
        logger.info(f"Processing only persona: {target}")

    # Process each persona
    success_count = 0
    for persona in personas_to_process:
        output_path = output_dir / persona["filename"]

        logger.info(f"Processing {persona['name']} ({persona['role']})")

        success = generate_image(persona["prompt"], output_path, force=args.force)

        if success:
            success_count += 1
            logger.info(f"Successfully processed headshot for {persona['name']}")
            
            # Optionally copy to target directory
            if args.copy_to_target:
                target_path = Path(args.target_dir) / persona["filename"]
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