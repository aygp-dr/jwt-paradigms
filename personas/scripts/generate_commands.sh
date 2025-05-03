#!/bin/bash
# Generate Claude command files for all personas

set -e

echo "Generating Claude command files for all personas..."
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PERSONAS_DIR="$(dirname "$SCRIPT_DIR")"
PROJECT_ROOT="$(dirname "$PERSONAS_DIR")"
CLAUDE_DIR="$PROJECT_ROOT/.claude/commands"
ORG_FILES="$PERSONAS_DIR"/*_*_*.org

# Create Claude commands directory if it doesn't exist
mkdir -p "$CLAUDE_DIR"

# Clear existing command files
rm -f "$CLAUDE_DIR"/review-presentation-*.md

# Generate command files for each persona
for file in $ORG_FILES; do
    filename=$(basename "$file")
    base_name="${filename%.org}"
    
    # Extract persona name for command
    persona_parts=(${base_name//_/ })
    if [ ${#persona_parts[@]} -ge 3 ]; then
        # Format: role_firstname_lastname.org
        command_name="${persona_parts[1]}"
    else 
        # Fallback to base name
        command_name="$base_name"
    fi
    
    echo "Generating command file for $filename"
    python "$SCRIPT_DIR/extract_persona.py" --input "$file" --output "$CLAUDE_DIR/review-presentation-$command_name.md"
done

echo "Generated $(ls "$CLAUDE_DIR"/review-presentation-*.md | wc -l) command files"
echo "Files available in: $CLAUDE_DIR"