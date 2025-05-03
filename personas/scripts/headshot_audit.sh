#!/usr/bin/env bash
# Headshot Audit Script
# Checks if all images referenced in README.org exist and are correctly referenced in org files

# Set the script to exit on error
set -e

# Define colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color
BLUE='\033[0;34m'

echo -e "${BLUE}======= HEADSHOT AUDIT =======${NC}"
echo -e "${BLUE}Checking all images referenced in README.org${NC}"
echo

# Counter for missing files and missing references
missing_files=0
missing_refs=0

# Extract image paths from README.org and audit them
for IMAGE_PATH in $(grep "images/" README.org | cut -d: -f2 | tr -d ']'); do
  echo -e "${GREEN}## ${IMAGE_PATH}${NC}"
  
  # Check if the file exists
  if [ -f "$IMAGE_PATH" ]; then
    ls -la "$IMAGE_PATH"
  else
    echo -e "${RED}ERROR: File does not exist!${NC}"
    missing_files=$((missing_files + 1))
  fi
  
  # Check if the image is referenced in any org file
  references=$(grep -Hn "$IMAGE_PATH" *_*_*.org)
  if [ -n "$references" ]; then
    echo "$references"
  else
    echo -e "${YELLOW}WARNING: No reference found in any org file!${NC}"
    missing_refs=$((missing_refs + 1))
  fi
  
  echo
done

# Print summary
echo -e "${BLUE}======= AUDIT SUMMARY =======${NC}"
if [ $missing_files -eq 0 ] && [ $missing_refs -eq 0 ]; then
  echo -e "${GREEN}All images exist and are referenced correctly.${NC}"
else
  if [ $missing_files -gt 0 ]; then
    echo -e "${RED}$missing_files image(s) are missing from the filesystem.${NC}"
  fi
  if [ $missing_refs -gt 0 ]; then
    echo -e "${YELLOW}$missing_refs image(s) are not referenced in any org file.${NC}"
  fi
fi