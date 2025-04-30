#!/usr/bin/env python3
"""
Test script for generating headshots using Google's Gemini API.
This script uses persona descriptions from the persona_prompts module.
"""

import logging
import os
import sys
import time
from pathlib import Path

try:
    # For image processing
    from io import BytesIO

    # For Gemini API
    from google import genai
    from google.genai import types

    # Import persona prompts
    from persona_prompts import PERSONA_PROMPTS
    from PIL import Image
except ImportError:
    print("Missing required packages. Please install with:")
    print("pip install pillow google-genai")
    sys.exit(1)

# Configure logging
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger("gemini_headshots_test")

# Define personas using prompts from the imported module
TEST_PERSONAS = [
    {
        "name": "Professor Wellington",
        "role": "Academic Gadfly",
        "filename": "spark_wellington.png",
        "prompt": PERSONA_PROMPTS["wellington"],
    },
    {
        "name": "Raj Patel",
        "role": "Polyglot Developer",
        "filename": "raj_patel.png",
        "prompt": PERSONA_PROMPTS["raj"],
    },
    {
        "name": "Dr. Maya Ramirez",
        "role": "Python Security Expert",
        "filename": "maya_ramirez.png",
        "prompt": PERSONA_PROMPTS["maya"],
    },
    {
        "name": "Hannah Chen",
        "role": "Security Architect",
        "filename": "hannah_chen.png",
        "prompt": PERSONA_PROMPTS["hannah"],
    },
    {
        "name": "Alex Chen",
        "role": "Paradigm Purist",
        "filename": "zero_chen.png",
        "prompt": PERSONA_PROMPTS["zero"],
    },
    {
        "name": "Sofia Martinez",
        "role": "Beginner-Friendly Developer",
        "filename": "sofia_martinez.png",
        "prompt": PERSONA_PROMPTS["sofia"],
    },
    {
        "name": "Dr. Neha Kapoor",
        "role": "Performance Engineer",
        "filename": "neha_kapoor.png",
        "prompt": PERSONA_PROMPTS["neha"],
    },
    {
        "name": "Marco Hernandez",
        "role": "Accessibility Advocate",
        "filename": "marco_hernandez.png",
        "prompt": PERSONA_PROMPTS["marco"],
    },
    {
        "name": "Dr. Eleanor Reynolds",
        "role": "VP of Engineering",
        "filename": "eleanor_reynolds.png",
        "prompt": PERSONA_PROMPTS["eleanor"],
    },
]


def check_api_key():
    """Check if the Gemini API key is set in the environment."""
    api_key = os.environ.get("GEMINI_API_KEY")
    if not api_key:
        logger.error("GEMINI_API_KEY environment variable not set")
        logger.error("Please set this variable in your .env file or directly in your environment")
        return False
    return True


def generate_image(prompt, output_path):
    """Generate an image using Google's Gemini API."""
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
        image.save(output_path)

        logger.info(f"Image saved to {output_path}")
        return True

    except Exception as e:
        logger.error(f"Error generating image: {e}")
        return False


def main():
    """Main entry point for the script."""
    logger.info("Starting Gemini headshots test")

    # Check for API key
    if not check_api_key():
        return 1

    # Ensure output directory exists
    output_dir = Path("images/gemini")
    output_dir.mkdir(parents=True, exist_ok=True)

    # Process each persona
    success_count = 0
    for persona in TEST_PERSONAS:
        output_path = output_dir / persona["filename"]

        logger.info(f"Processing {persona['name']} ({persona['role']})")

        success = generate_image(persona["prompt"], output_path)

        if success:
            success_count += 1
            logger.info(f"Successfully generated headshot for {persona['name']}")
        else:
            logger.error(f"Failed to generate headshot for {persona['name']}")

        # Add a delay to avoid rate limits
        time.sleep(2)

    logger.info(f"Completed generation of {success_count}/{len(TEST_PERSONAS)} headshots")

    return 0


if __name__ == "__main__":
    sys.exit(main())
