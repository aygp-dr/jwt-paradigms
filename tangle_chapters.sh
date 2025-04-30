#!/bin/bash
# Script to tangle all chapter files

cd /home/jwalsh/projects/aygp-dr/jwt-parsing-examples

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