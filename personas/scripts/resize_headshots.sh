#!/usr/bin/env bash
# Script to resize headshots to 400x400
# Run from the project root directory

# Set the script to exit on error
set -e

# Define colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color
BLUE='\033[0;34m'

echo -e "${BLUE}======= RESIZING HEADSHOTS =======${NC}"

# Check if convert (ImageMagick) is installed
if ! command -v convert &> /dev/null; then
    echo -e "${RED}Error: ImageMagick is not installed. Please install it first.${NC}"
    exit 1
fi

# Create scaled directory if it doesn't exist
mkdir -p images/scaled

# Process all PNG files over 500KB
for IMAGE_PATH in $(find images -maxdepth 1 -name "*.png" -size +500k); do
    FILENAME=$(basename "$IMAGE_PATH")
    SCALED_PATH="images/scaled/$FILENAME"
    
    echo -e "${GREEN}Resizing: $IMAGE_PATH${NC}"
    
    # Get original dimensions
    DIMENSIONS=$(identify -format "%wx%h" "$IMAGE_PATH")
    echo "Original size: $DIMENSIONS"
    
    # Resize to 400x400
    convert "$IMAGE_PATH" -resize 400x400 "$SCALED_PATH"
    
    # Get new dimensions and file size
    NEW_DIMENSIONS=$(identify -format "%wx%h" "$SCALED_PATH")
    ORIG_SIZE=$(du -h "$IMAGE_PATH" | cut -f1)
    NEW_SIZE=$(du -h "$SCALED_PATH" | cut -f1)
    
    echo "New size: $NEW_DIMENSIONS ($NEW_SIZE vs. original $ORIG_SIZE)"
    
    # Replace original with scaled version
    mv "$SCALED_PATH" "$IMAGE_PATH"
    echo -e "${GREEN}Replaced original with scaled version${NC}"
    echo
done

echo -e "${BLUE}======= RESIZING COMPLETE =======${NC}"