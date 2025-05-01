#\!/usr/bin/env python3
"""
Update persona org files to use standardized image blocks.
"""

import re
import os
import sys
import glob

# Image prompts from persona_prompts.py
IMAGE_PROMPTS = {
    "systems_security_researcher_vikram_shah.org": {
        "filename": "images/vikram_shah.png",
        "prompt": """Professional academic portrait of Dr. Vikram Shah, a 42-year-old Indian-American man with a methodical and precise demeanor. He has short, neatly styled black hair with slight graying at the temples and wears thin-framed rectangular glasses that emphasize his analytical gaze. He has a medium brown complexion and a serious, contemplative expression that reflects his mathematical background. He's wearing a crisp white dress shirt with a navy blue blazer and a subtle patterned tie. The background suggests an academic environment with blurred bookshelves. His posture is straight and formal, and his eyes convey deep concentration - like someone working through a complex proof. The lighting is structured and precise, creating clear definition that emphasizes his scholarly features. His expression shows both intellectual rigor and academic authority."""
    },
    "code_quality_advocate_james_wilson.org": {
        "filename": "images/james_wilson.png",
        "prompt": """Professional headshot of James Wilson, a 39-year-old Caucasian man with a methodical, patient expression. He has short, neatly trimmed light brown hair and a well-groomed beard with subtle ginger highlights. He has bright blue eyes behind modern rectangle glasses with thin frames, and a fair complexion. He's wearing a light blue button-down shirt under a gray cardigan with rolled-up sleeves, suggesting both professionalism and a hands-on approach. The background is a subtle gradient of soft gray. His expression is thoughtful and attentive - like someone carefully considering code structure. He has a slight, warm smile that conveys his teacher's mindset. The lighting is even and clean, emphasizing clarity and organization. His expression balances technical precision with approachable mentorship."""
    },
    "ai_ethics_researcher_amara_chen.org": {
        "filename": "images/amara_chen.png",
        "prompt": """Professional academic portrait of Dr. Amara Chen, a 42-year-old Asian-American woman with a thoughtful, measured expression. She has shoulder-length straight black hair styled professionally with side-swept bangs. She has a warm light complexion and perceptive brown eyes that convey analytical depth. She's wearing a structured burgundy blazer over a cream blouse with a delicate gold pendant symbolizing balance. The background suggests a modern university office with blurred bookshelves and natural light. Her posture is engaged and attentive, with a slight tilt of her head suggesting careful consideration. She has a subtle, thoughtful smile that conveys both intelligence and empathy. The lighting is warm yet professional, creating dimension that highlights her contemplative nature. Her expression balances academic authority with ethical concern."""
    },
    "ux_researcher_olivia_rodriguez.org": {
        "filename": "images/olivia_rodriguez.png",
        "prompt": """Mid-30s Latina woman with shoulder-length dark brown hair with subtle highlights, warm brown eyes, and a friendly, approachable expression. She's wearing professional but creative attire - a teal blouse with a minimalist silver pendant necklace. Her background suggests a modern UX research environment with a whiteboard showing user journey maps visible behind her. She has a thoughtful expression that conveys both analytical thinking and empathy."""
    },
    "collaborative_software_researcher_diego_martinez.org": {
        "filename": "images/diego_martinez.png",
        "prompt": """Hispanic male in his early 40s with short black hair with some gray at the temples, trimmed beard, rectangular glasses, thoughtful expression. Professional appearance with a navy blue button-up shirt. Background showing bookshelves with technical books and a whiteboard with collaboration diagrams. Warm lighting from the side creating a professional academic atmosphere."""
    },
    "accessibility_advocate_marco_hernandez.org": {
        "filename": "images/marco_hernandez.png",
        "prompt": """Professional headshot of Marco Hernandez, a 41-year-old Latino man with a warm, empathetic expression. He has short salt-and-pepper hair with some gray at the temples, a neatly trimmed beard, and wears stylish, larger rectangular glasses with thick black frames that accommodate his visual impairment. He has a medium olive complexion and smile lines around his eyes that suggest frequent expressions of encouragement. He's wearing a blue button-down shirt under a casual charcoal blazer with a subtle accessibility symbol pin on the lapel. The background is a light, clean gradient. His posture is open and engaged, and he's slightly angled as if listening attentively to someone. The lighting is warm and even, creating a welcoming feel. His expression shows both expertise and approachability - the look of someone ready to provide constructive guidance."""
    },
    "paradigm_purist_alex_chen.org": {
        "filename": "images/zero_chen.png",
        "prompt": """Professional but dramatic portrait of Alex "Zero" Chen, a cyberpunk-styled graduate student in their late 20s with an ambiguous gender presentation. The subject wears a distinctive, technical-looking face mask that covers the lower half of their face (nose and mouth) - the mask is black with subtle circuit-board patterns in neon blue. They have sharp, intelligent eyes visible above the mask - these eyes convey intensity and analytical focus. Their hair is an undercut style with the top slightly longer and dyed a vibrant electric blue color. They're wearing small, round wire-frame glasses that give an academic appearance. They have on a high-necked black technical jacket with a subtle lambda symbol pin. The background is dark with faint code elements barely visible. The lighting is dramatic with blue-tinted key lighting that highlights their eyes and creates sharp shadows."""
    },
    "performance_engineer_neha_kapoor.org": {
        "filename": "images/neha_kapoor.png", 
        "prompt": """Professional headshot of Dr. Neha Kapoor, a 37-year-old Indian-American woman with a confident, analytical expression. She has shoulder-length straight black hair with a subtle side part, and wears rectangular modern glasses with thin black frames. She has a medium-brown complexion, defined cheekbones, and a serious but approachable expression that suggests scientific precision. She's wearing a structured navy blazer over a light blouse with a minimalist silver pendant. The background is a gradient gray. Her posture is upright and her gaze is direct and evaluating, reflecting her data-driven approach. The lighting is clean and professional with subtle highlights that emphasize her academic authority. Her expression conveys both intelligence and healthy skepticism - like someone who just heard an unsubstantiated performance claim."""
    },
    "beginner_friendly_sofia_martinez.org": {
        "filename": "images/sofia_martinez.png",
        "prompt": """Professional headshot of Sofia Martinez, a 28-year-old Latina woman with medium-length straight dark brown hair that falls just above her shoulders. She has warm brown eyes, a friendly smile, and a welcoming expression. She's wearing a navy blue professional top with minimal, tasteful jewelry (small stud earrings). The background is a neutral light gray. Her appearance conveys the enthusiasm and approachability of a former teacher now working as a junior developer. Portrait lighting is soft and flattering, highlighting her warm complexion and friendly demeanor. She has a slightly rounded face shape with defined eyebrows and natural makeup."""
    },
    "executive_sponsor_eleanor_reynolds.org": {
        "filename": "images/eleanor_reynolds.png",
        "prompt": """Professional executive headshot of Dr. Eleanor Reynolds, a 48-year-old woman with an authoritative yet approachable presence. She has shoulder-length silver-streaked brown hair styled in a sophisticated bob. She has a fair complexion with natural smile lines that suggest experience, and wears minimal but professional makeup including a subtle mauve lipstick. Her expression is confident and discerning - the look of someone who makes multi-million dollar decisions daily. She's wearing a tailored charcoal suit jacket over a burgundy blouse with a simple gold necklace. The background is a soft gradient of deep blue. Her posture is straight and commanding, and her gaze is direct but not intimidating. The lighting is professional with subtle edge lighting that highlights her strong bone structure and creates depth. Her expression balances executive authority with thoughtful intelligence."""
    }
}

def update_file(file_path):
    """Update a persona file to include standardized image block."""
    if not os.path.exists(file_path):
        print(f"File not found: {file_path}")
        return False
        
    file_name = os.path.basename(file_path)
    if file_name not in IMAGE_PROMPTS:
        print(f"No image prompt found for: {file_name}")
        return False
        
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
            
        # Already has org-ai image block?
        if '#+begin_ai :image :file' in content:
            print(f"File already has org-ai image block: {file_name}")
            return True
            
        # Check for old headshot format
        headshot_match = re.search(r'\*\* Headshot(?:.*?)Generation Prompt\n.*?:PROPERTIES:.*?:END:\n(.*?)(?=\n\n\*\*)', content, re.DOTALL)
        prompt_info = IMAGE_PROMPTS[file_name]
        
        if headshot_match:
            # Replace existing headshot section
            new_content = re.sub(
                r'\*\* Headshot(?:.*?)Generation Prompt\n.*?:PROPERTIES:.*?:END:\n.*?(?=\n\n\*\*)', 
                f"""** Image Generation
   :PROPERTIES:
   :CUSTOM_ID: image-generation
   :END:

#+begin_ai :image :file {prompt_info['filename']}
{prompt_info['prompt']}
#+end_ai

""", 
                content, 
                flags=re.DOTALL
            )
        else:
            # Add after main heading
            new_content = re.sub(
                r'(\* .*?\n.*?:END:\n)',
                f"""\\1** Image Generation
   :PROPERTIES:
   :CUSTOM_ID: image-generation
   :END:

#+begin_ai :image :file {prompt_info['filename']}
{prompt_info['prompt']}
#+end_ai

""",
                content,
                count=1
            )
            
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(new_content)
            
        print(f"Updated: {file_name}")
        return True
        
    except Exception as e:
        print(f"Error updating {file_name}: {e}")
        return False

def main():
    """Main entry point."""
    # Update specific files
    if len(sys.argv) > 1:
        for file_arg in sys.argv[1:]:
            update_file(file_arg)
    else:
        # Update all files in IMAGE_PROMPTS
        for file_name in IMAGE_PROMPTS:
            update_file(file_name)
    
    return 0

if __name__ == "__main__":
    sys.exit(main())
