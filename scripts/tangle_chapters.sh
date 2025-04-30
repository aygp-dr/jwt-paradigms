#!/bin/bash
# Script to tangle all chapter files

# Use the directory where the script is located as the base
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
cd "$REPO_ROOT"

echo "Tangling all chapter files..."
for file in personas/gadfly/chapters/*.org; do
  echo "Tangling $file..."
  emacs --batch --eval "(require 'org)" --eval "(org-babel-tangle-file \"$file\")"
done

echo "Done tangling!"

echo "Checking tangled directories and files:"
find personas/gadfly/examples -type d | sort
echo "Files created:"
find personas/gadfly/examples -type f | sort