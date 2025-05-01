#\!/usr/bin/env python3
"""
Fix org files that need image generation blocks.
"""

import os
import sys
import re

# Persona definitions
PERSONAS = {
    "accessibility_advocate_marco_hernandez.org": {
        "filename": "images/marco_hernandez.png",
        "prompt": """Professional headshot of Marco Hernandez, a 41-year-old Latino man with a warm, empathetic expression. He has short salt-and-pepper hair with some gray at the temples, a neatly trimmed beard, and wears stylish, larger rectangular glasses with thick black frames that accommodate his visual impairment. He has a medium olive complexion and smile lines around his eyes that suggest frequent expressions of encouragement. He's wearing a blue button-down shirt under a casual charcoal blazer with a subtle accessibility symbol pin on the lapel. The background is a light, clean gradient. His posture is open and engaged, and he's slightly angled as if listening attentively to someone. The lighting is warm and even, creating a welcoming feel. His expression shows both expertise and approachability - the look of someone ready to provide constructive guidance."""
    },
    "beginner_friendly_sofia_martinez.org": {
        "filename": "images/sofia_martinez.png",
        "prompt": """Professional headshot of Sofia Martinez, a 28-year-old Latina woman with medium-length straight dark brown hair that falls just above her shoulders. She has warm brown eyes, a friendly smile, and a welcoming expression. She's wearing a navy blue professional top with minimal, tasteful jewelry (small stud earrings). The background is a neutral light gray. Her appearance conveys the enthusiasm and approachability of a former teacher now working as a junior developer. Portrait lighting is soft and flattering, highlighting her warm complexion and friendly demeanor. She has a slightly rounded face shape with defined eyebrows and natural makeup."""
    },
    "executive_sponsor_eleanor_reynolds.org": {
        "filename": "images/eleanor_reynolds.png",
        "prompt": """Professional executive headshot of Dr. Eleanor Reynolds, a 48-year-old woman with an authoritative yet approachable presence. She has shoulder-length silver-streaked brown hair styled in a sophisticated bob. She has a fair complexion with natural smile lines that suggest experience, and wears minimal but professional makeup including a subtle mauve lipstick. Her expression is confident and discerning - the look of someone who makes multi-million dollar decisions daily. She's wearing a tailored charcoal suit jacket over a burgundy blouse with a simple gold necklace. The background is a soft gradient of deep blue. Her posture is straight and commanding, and her gaze is direct but not intimidating. The lighting is professional with subtle edge lighting that highlights her strong bone structure and creates depth. Her expression balances executive authority with thoughtful intelligence."""
    },
    "paradigm_purist_alex_chen.org": {
        "filename": "images/zero_chen.png",
        "prompt": """Professional but dramatic portrait of Alex "Zero" Chen, a cyberpunk-styled graduate student in their late 20s with an ambiguous gender presentation. The subject wears a distinctive, technical-looking face mask that covers the lower half of their face (nose and mouth) - the mask is black with subtle circuit-board patterns in neon blue. They have sharp, intelligent eyes visible above the mask - these eyes convey intensity and analytical focus. Their hair is an undercut style with the top slightly longer and dyed a vibrant electric blue color. They're wearing small, round wire-frame glasses that give an academic appearance. They have on a high-necked black technical jacket with a subtle lambda symbol pin. The background is dark with faint code elements barely visible. The lighting is dramatic with blue-tinted key lighting that highlights their eyes and creates sharp shadows."""
    },
    "performance_engineer_neha_kapoor.org": {
        "filename": "images/neha_kapoor.png",
        "prompt": """Professional headshot of Dr. Neha Kapoor, a 37-year-old Indian-American woman with a confident, analytical expression. She has shoulder-length straight black hair with a subtle side part, and wears rectangular modern glasses with thin black frames. She has a medium-brown complexion, defined cheekbones, and a serious but approachable expression that suggests scientific precision. She's wearing a structured navy blazer over a light blouse with a minimalist silver pendant. The background is a gradient gray. Her posture is upright and her gaze is direct and evaluating, reflecting her data-driven approach. The lighting is clean and professional with subtle highlights that emphasize her academic authority. Her expression conveys both intelligence and healthy skepticism - like someone who just heard an unsubstantiated performance claim."""
    },
}

def fix_org_file(file_path):
    """Add image generation block to org file."""
    if not os.path.exists(file_path):
        print(f"Error: File not found: {file_path}")
        return False
        
    file_name = os.path.basename(file_path)
    if file_name not in PERSONAS:
        print(f"Error: No image data for: {file_name}")
        return False
        
    # Read file
    with open(file_path, 'r', encoding='utf-8') as f:
        content = f.read()
        
    # Check if file already has image block
    if '#+begin_ai :image' in content:
        print(f"File already has image block: {file_name}")
        return True
        
    # Insert block after main title
    main_title_pattern = r'(\* .*?\n\s+:PROPERTIES:.*?:END:\n)'
    prompt_info = PERSONAS[file_name]
    
    if re.search(main_title_pattern, content, re.DOTALL):
        new_content = re.sub(
            main_title_pattern,
            f"\\1** Image Generation\n   :PROPERTIES:\n   :CUSTOM_ID: image-generation\n   :END:\n\n#+begin_ai :image :file {prompt_info['filename']}\n{prompt_info['prompt']}\n#+end_ai\n\n",
            content,
            count=1,
            flags=re.DOTALL
        )
        
        # Write modified content
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(new_content)
            
        print(f"Added image block to: {file_name}")
        return True
    else:
        print(f"Error: Could not find main title in: {file_name}")
        return False

def main():
    """Process files."""
    if len(sys.argv) > 1:
        # Process specific files
        for file_path in sys.argv[1:]:
            fix_org_file(file_path)
    else:
        # Process all files in PERSONAS
        for file_name in PERSONAS:
            fix_org_file(file_name)
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
