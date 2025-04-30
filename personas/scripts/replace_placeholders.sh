#!/usr/bin/env bash

# Script to replace placeholder images with generated versions
# Looks for small PNG files in images/ directory and replaces them
# with equivalent files from images/gemini/ if they exist

IMAGES_DIR="/home/aygp-dr/projects/aygp-dr/jwt-paradigms/personas/images"
GEMINI_DIR="${IMAGES_DIR}/gemini"
BACKUP_DIR="${IMAGES_DIR}/backup"

# Create backup directory if it doesn't exist
mkdir -p "$BACKUP_DIR"

echo "Checking for placeholder images (< 5KB) in $IMAGES_DIR..."

# Loop through all PNG files in the images directory
for image in "$IMAGES_DIR"/*.png; do
    # Get the base filename
    filename=$(basename "$image")
    
    # Get file size in KB
    filesize=$(du -k "$image" | cut -f1)
    
    # Check if file is smaller than 5KB (placeholder)
    if [ "$filesize" -lt 5 ]; then
        echo "Found small image: $filename ($filesize KB)"
        
        # Check if equivalent exists in Gemini directory
        if [ -f "$GEMINI_DIR/$filename" ]; then
            echo "  ✓ Found Gemini replacement"
            
            # Create backup
            cp "$image" "$BACKUP_DIR/$filename"
            echo "  ✓ Created backup in $BACKUP_DIR/$filename"
            
            # Replace with Gemini version
            cp "$GEMINI_DIR/$filename" "$image"
            echo "  ✓ Replaced with Gemini version"
        else
            echo "  ✗ No Gemini replacement found for $filename"
        fi
    else
        echo "Skipping $filename ($filesize KB) - not a placeholder"
    fi
done

echo "Image replacement process complete!"
