#!/usr/bin/env bash

# Script to replace placeholder images with generated versions
# Looks for small PNG files in images/ directory and replaces them
# with equivalent files from images/gemini/ if they exist

IMAGES_DIR="/home/aygp-dr/projects/aygp-dr/jwt-paradigms/personas/images"
GEMINI_DIR="${IMAGES_DIR}/gemini"
BACKUP_DIR="${IMAGES_DIR}/backup"

# Create backup directory if it doesn't exist
mkdir -p "$BACKUP_DIR"

# Function to check and fix image dimensions
check_image_dimensions() {
    echo "Checking for non-standard sized images..."
    
    for img in "$IMAGES_DIR"/*.png; do
        # Skip placeholder.png
        if [[ "$(basename "$img")" == "placeholder.png" ]]; then
            continue
        fi
        
        # Get image dimensions
        dimensions=$(identify -format "%wx%h" "$img")
        
        # If not 400x400, resize it
        if [ "$dimensions" != "400x400" ]; then
            echo "Resizing $img from $dimensions to 400x400"
            
            # Backup the original if it doesn't exist
            filename=$(basename "$img")
            if [ ! -f "$BACKUP_DIR/$filename" ]; then
                cp "$img" "$BACKUP_DIR/$filename"
                echo "  ✓ Backed up original to $BACKUP_DIR/$filename"
            fi
            
            # Resize the image
            convert "$img" -resize 400x400 "$img"
            echo "  ✓ Resized to 400x400"
        fi
    done
    
    echo "All images checked for correct dimensions."
}

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
            
            # Replace with Gemini version, resizing to 400x400
            convert "$GEMINI_DIR/$filename" -resize 400x400 "$image"
            echo "  ✓ Replaced with Gemini version"
        else
            echo "  ✗ No Gemini replacement found for $filename"
        fi
    else
        echo "Skipping $filename ($filesize KB) - not a placeholder"
    fi
done

# Run dimension check to make sure all images are 400x400
check_image_dimensions

echo "Image replacement process complete!"
