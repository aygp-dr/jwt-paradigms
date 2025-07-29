# Persona Headshot Generation Workflow

This document describes the workflow for generating consistent persona headshots using Google's Gemini API.

## Setup

1. **Environment Configuration**
   ```bash
   # Copy the example environment file
   cp ../.env.example ../.env
   
   # Add your Gemini API key
   # Get key from: https://makersuite.google.com/app/apikey
   GEMINI_API_KEY=your-api-key-here
   ```

2. **Python Environment**
   ```bash
   # Setup is handled by the Makefile
   make setup
   ```

## Key Design Decisions

### Avoiding CGI/Stylized Images
- Use "Professional headshot" or "Realistic professional corporate headshot" in prompts
- Explicitly state "No artistic filters or stylization"
- Emphasize "natural lighting" and "LinkedIn profile photo" style
- Avoid terms like "androgynous" which may trigger stylized rendering

### Ensuring Visual Consistency
- Include specific details: hair color, eye color, glasses style, clothing
- Add unique identifiers (freckles, scars, jewelry) to maintain identity
- Use consistent lighting descriptions across regenerations
- End prompts with "Consistent features:" list

### Example Prompt Structure
```
Professional headshot of [Name], a [age]-year-old [person/man/woman] with [specific physical features]. 
[Detailed appearance description including hair, eyes, facial features, expression]. 
They're wearing [specific clothing description]. 
[Background description]. 
[Expression and personality traits]. 
The lighting is [lighting description]. 
Consistent features: [list key identifiers].
```

## Generation Commands

### Generate Single Persona
```bash
# Generate specific persona with force regeneration
uv run python scripts/generate_headshots.py --persona "casey_thompson" --force --copy-to-target

# Generate only if missing (respects cache)
uv run python scripts/generate_headshots.py --persona "casey_thompson" --copy-to-target
```

### Generate Multiple Personas
```bash
# Generate all L5/L6/L7 engineers
uv run python scripts/generate_headshots.py --pattern "l*_engineer_*.org" --copy-to-target

# Generate all personas
make headshots
```

### Cache Management
```bash
# Clear cache for specific persona
rm images/gemini/alex_morgan.png

# Clear all engineer caches
rm -f images/gemini/casey_thompson.png images/gemini/blake_harrison.png images/gemini/alex_morgan.png

# Force regeneration (ignores cache)
uv run python scripts/generate_headshots.py --force
```

## Directory Structure
```
personas/
├── images/                  # Final headshot images
│   ├── casey_thompson.png
│   ├── blake_harrison.png
│   └── alex_morgan.png
├── images/gemini/          # Cached generated images
├── scripts/
│   └── generate_headshots.py  # Generation script (uses Gemini 2.0)
└── *.org                   # Persona files with #+begin_ai blocks
```

## Troubleshooting

### Image looks too stylized/CGI
1. Remove words like "androgynous", "striking", "commanding"
2. Add "realistic", "natural lighting", "LinkedIn-style"
3. Emphasize "no filters or stylization"

### Inconsistent appearance across regenerations
1. Add more specific physical details
2. Include unique identifiers (scars, freckles, specific jewelry)
3. Use "Consistent features:" list at end of prompt

### Script not detecting missing images
1. Check both `images/` and `images/gemini/` directories
2. Use `--force` flag to bypass cache
3. Clear gemini cache directory if needed

## Current Engineer Personas
- **Casey Thompson (L5)**: Female, blonde ponytail, burgundy hoodie, enthusiastic
- **Blake Harrison (L6)**: Male, dark hair, glasses, green shirt with gray cardigan
- **Alex Morgan (L7)**: Female, auburn hair in ponytail, navy blazer, wire-rimmed glasses