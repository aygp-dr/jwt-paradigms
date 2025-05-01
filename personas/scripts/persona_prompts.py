#!/usr/bin/env python3
"""
This module contains prompts and metadata for all personas.
All persona information is centralized here for easier management.
"""

# Define comprehensive persona information with both prompts and metadata

PERSONAS = [
    # Original personas
    {
        "id": "wellington",
        "name": "Professor Marcus Wellington",
        "role": "Academic Gadfly",
        "filename": "spark_wellington.png",
        "file_path": "academic_gadfly_marcus_wellington.org",
        "review_command": "/review-presentation-wellington",
        "focus": "Theoretical foundations, historical context, philosophical underpinnings",
        "prompt": """Professional headshot of Professor Marcus Wellington, a 57-year-old tenured
Computer Science professor with a completely bald head and a well-trimmed salt-and-pepper beard.
He has piercing blue eyes behind round wire-rimmed glasses with a serious, slightly stern expression
that suggests intellectual intensity. He's wearing a dark turtleneck with a tweed jacket.
The background is a neutral dark gray. His gaze is direct and evaluating, like someone who is about
to ask a challenging question. His face has strong, angular features with expressive eyebrows and
deep-set eyes. Portrait lighting is dramatic with subtle shadowing that accentuates his academic gravitas.
His expression conveys both brilliance and skepticism."""
    },
    {
        "id": "raj",
        "name": "Raj Patel",
        "role": "Polyglot Developer",
        "filename": "raj_patel.png",
        "file_path": "polyglot_developer_raj_patel.org",
        "review_command": "/review-presentation-raj",
        "focus": "Cross-language patterns, idiomatic approaches, implementation tradeoffs",
        "prompt": """Professional headshot of Raj Patel, a 31-year-old Indian-American man with short,
slightly wavy black hair styled neatly. He has medium brown skin, thoughtful dark eyes behind modern
round glasses with thin metal frames, and a well-groomed short beard. He's wearing a light blue button-up
shirt with a clean, professional appearance. The background is neutral dark gray. His expression is
slightly smiling, conveying intelligence and curiosity. His face has a defined jawline and his posture
is upright and engaged. Portrait lighting is crisp and professional, creating subtle shadows that highlight
his facial structure."""
    },
    {
        "id": "maya",
        "name": "Dr. Maya Ramirez",
        "role": "Python Security Expert",
        "filename": "maya_ramirez.png",
        "file_path": "python_expert_maya_ramirez.org",
        "review_command": "/review-presentation-maya",
        "focus": "Security implications, best practices, efficient implementations",
        "prompt": """Professional headshot of Dr. Maya Ramirez, a 42-year-old Latina woman with straight
black hair with subtle gray streaks falling to shoulder length. She has an olive complexion, intelligent
brown eyes behind stylish rectangular glasses with dark frames, and a serious but confident expression.
She's wearing a burgundy blouse under a dark blazer with a small pendant necklace. The background features
blurred bookshelves suggesting an academic environment. Her face has defined cheekbones, and she has a
poised, analytical demeanor. Portrait lighting is structured and professional, emphasizing her academic
authority. Her makeup is minimal and professional."""
    },
    {
        "id": "hannah",
        "name": "Hannah Chen",
        "role": "Security Architect",
        "filename": "hannah_chen.png",
        "file_path": "security_expert_hannah_chen.org",
        "review_command": "/review-presentation-hannah",
        "focus": "Real-world security concerns, practical mitigations, balancing security with usability",
        "prompt": """Professional headshot of Hannah Chen, a 36-year-old East Asian woman with straight
black hair cut in a sleek, shoulder-length style with side-swept bangs. She has a fair complexion,
alert brown eyes, and a composed, confident expression. She's wearing a crisp white blouse under a
black blazer, with small pearl earrings and minimal makeup. The background is a neutral dark gray.
Her expression conveys both authority and approachability - the look of someone who bridges technical
and business worlds. Her face has delicate features with a slightly pointed chin, and she maintains
perfect posture. Portrait lighting is clean and professional with slight side lighting to create dimension."""
    },
    {
        "id": "zero",
        "name": "Alex \"Zero\" Chen",
        "role": "Paradigm Purist",
        "filename": "zero_chen.png",
        "file_path": "paradigm_purist_alex_chen.org",
        "review_command": "/review-presentation-zero",
        "focus": "Idiomatic functional implementations, pattern matching, code elegance",
        "prompt": """Professional but dramatic portrait of Alex "Zero" Chen, a cyberpunk-styled graduate
student in their late 20s with an ambiguous gender presentation. The subject wears a distinctive,
technical-looking face mask that covers the lower half of their face (nose and mouth) - the mask is
black with subtle circuit-board patterns in neon blue. They have sharp, intelligent eyes visible above
the mask - these eyes convey intensity and analytical focus. Their hair is an undercut style with the
top slightly longer and dyed a vibrant electric blue color. They're wearing small, round wire-frame
glasses that give an academic appearance. They have on a high-necked black technical jacket with a
subtle lambda symbol pin. The background is dark with faint code elements barely visible. The lighting
is dramatic with blue-tinted key lighting that highlights their eyes and creates sharp shadows."""
    },
    {
        "id": "sofia",
        "name": "Sofia Martinez",
        "role": "Beginner-Friendly Developer",
        "filename": "sofia_martinez.png",
        "file_path": "beginner_friendly_sofia_martinez.org",
        "review_command": "/review-presentation-sofia",
        "focus": "Clear explanations, approachability, newcomer perspective",
        "prompt": """Professional headshot of Sofia Martinez, a 28-year-old Latina woman with medium-length
straight dark brown hair that falls just above her shoulders. She has warm brown eyes, a friendly smile,
and a welcoming expression. She's wearing a navy blue professional top with minimal, tasteful jewelry
(small stud earrings). The background is a neutral light gray. Her appearance conveys the enthusiasm
and approachability of a former teacher now working as a junior developer. Portrait lighting is soft
and flattering, highlighting her warm complexion and friendly demeanor. She has a slightly rounded
face shape with defined eyebrows and natural makeup."""
    },
    {
        "id": "neha",
        "name": "Dr. Neha Kapoor",
        "role": "Performance Engineer",
        "filename": "neha_kapoor.png",
        "file_path": "performance_engineer_neha_kapoor.org",
        "review_command": "/review-presentation-neha",
        "focus": "Performance metrics, benchmarking methodology, optimization tradeoffs",
        "prompt": """Professional headshot of Dr. Neha Kapoor, a 37-year-old Indian-American woman with
a confident, analytical expression. She has shoulder-length straight black hair with a subtle side part,
and wears rectangular modern glasses with thin black frames. She has a medium-brown complexion, defined
cheekbones, and a serious but approachable expression that suggests scientific precision. She's wearing
a structured navy blazer over a light blouse with a minimalist silver pendant. The background is a
gradient gray. Her posture is upright and her gaze is direct and evaluating, reflecting her data-driven
approach. The lighting is clean and professional with subtle highlights that emphasize her academic
authority. Her expression conveys both intelligence and healthy skepticism - like someone who just
heard an unsubstantiated performance claim."""
    },
    {
        "id": "marco",
        "name": "Marco Hernandez",
        "role": "Accessibility Advocate",
        "filename": "marco_hernandez.png",
        "file_path": "accessibility_advocate_marco_hernandez.org",
        "review_command": "/review-presentation-marco",
        "focus": "Inclusive presentations, assistive technology compatibility, cognitive accessibility",
        "prompt": """Professional headshot of Marco Hernandez, a 41-year-old Latino man with a warm,
empathetic expression. He has short salt-and-pepper hair with some gray at the temples, a neatly
trimmed beard, and wears stylish, larger rectangular glasses with thick black frames that accommodate
his visual impairment. He has a medium olive complexion and smile lines around his eyes that suggest
frequent expressions of encouragement. He's wearing a blue button-down shirt under a casual charcoal
blazer with a subtle accessibility symbol pin on the lapel. The background is a light, clean gradient.
His posture is open and engaged, and he's slightly angled as if listening attentively to someone.
The lighting is warm and even, creating a welcoming feel. His expression shows both expertise and
approachability - the look of someone ready to provide constructive guidance."""
    },
    {
        "id": "eleanor",
        "name": "Dr. Eleanor Reynolds",
        "role": "VP of Engineering",
        "filename": "eleanor_reynolds.png",
        "file_path": "executive_sponsor_eleanor_reynolds.org",
        "review_command": "/review-presentation-eleanor",
        "focus": "Strategic implications, organizational impact, business alignment",
        "prompt": """Professional executive headshot of Dr. Eleanor Reynolds, a 48-year-old woman with
an authoritative yet approachable presence. She has shoulder-length silver-streaked brown hair styled
in a sophisticated bob. She has a fair complexion with natural smile lines that suggest experience,
and wears minimal but professional makeup including a subtle mauve lipstick. Her expression is confident
and discerning - the look of someone who makes multi-million dollar decisions daily. She's wearing a
tailored charcoal suit jacket over a burgundy blouse with a simple gold necklace. The background is a
soft gradient of deep blue. Her posture is straight and commanding, and her gaze is direct but not
intimidating. The lighting is professional with subtle edge lighting that highlights her strong bone
structure and creates depth. Her expression balances executive authority with thoughtful intelligence."""
    },

    # New personas
    {
        "id": "vikram",
        "name": "Dr. Vikram Shah",
        "role": "Systems Security Researcher",
        "filename": "vikram_shah.png",
        "file_path": "systems_security_researcher_vikram_shah.org",
        "review_command": "/review-presentation-vikram",
        "focus": "Formal verification, cryptographic proofs, mathematical security models",
        "prompt": """Professional academic portrait of Dr. Vikram Shah, a 42-year-old Indian-American man with a 
methodical and precise demeanor. He has short, neatly styled black hair with slight graying at the temples and 
wears thin-framed rectangular glasses that emphasize his analytical gaze. He has a medium brown complexion and 
a serious, contemplative expression that reflects his mathematical background. He's wearing a crisp white dress 
shirt with a navy blue blazer and a subtle patterned tie. The background suggests an academic environment with 
blurred bookshelves. His posture is straight and formal, and his eyes convey deep concentration - like someone 
working through a complex proof. The lighting is structured and precise, creating clear definition that emphasizes 
his scholarly features. His expression shows both intellectual rigor and academic authority."""
    },
    {
        "id": "james",
        "name": "James Wilson",
        "role": "Code Quality Advocate",
        "filename": "james_wilson.png",
        "file_path": "code_quality_advocate_james_wilson.org",
        "review_command": "/review-presentation-james",
        "focus": "Testing strategies, maintainability, error handling, clean code principles",
        "prompt": """Professional headshot of James Wilson, a 39-year-old Caucasian man with a methodical, patient 
expression. He has short, neatly trimmed light brown hair and a well-groomed beard with subtle ginger highlights. 
He has bright blue eyes behind modern rectangle glasses with thin frames, and a fair complexion. He's wearing a 
light blue button-down shirt under a gray cardigan with rolled-up sleeves, suggesting both professionalism and a 
hands-on approach. The background is a subtle gradient of soft gray. His expression is thoughtful and attentive - 
like someone carefully considering code structure. He has a slight, warm smile that conveys his teacher's mindset. 
The lighting is even and clean, emphasizing clarity and organization. His expression balances technical precision 
with approachable mentorship."""
    },
    {
        "id": "amara",
        "name": "Dr. Amara Chen",
        "role": "AI Ethics Researcher",
        "filename": "amara_chen.png",
        "file_path": "ai_ethics_researcher_amara_chen.org",
        "review_command": "/review-presentation-amara",
        "focus": "Ethical implications of identity systems, consent models, power dynamics",
        "prompt": """Professional academic portrait of Dr. Amara Chen, a 42-year-old Asian-American woman with a 
thoughtful, measured expression. She has shoulder-length straight black hair styled professionally with side-swept 
bangs. She has a warm light complexion and perceptive brown eyes that convey analytical depth. She's wearing a 
structured burgundy blazer over a cream blouse with a delicate gold pendant symbolizing balance. The background 
suggests a modern university office with blurred bookshelves and natural light. Her posture is engaged and attentive, 
with a slight tilt of her head suggesting careful consideration. She has a subtle, thoughtful smile that conveys both 
intelligence and empathy. The lighting is warm yet professional, creating dimension that highlights her contemplative 
nature. Her expression balances academic authority with ethical concern."""
    },
    {
        "id": "olivia",
        "name": "Olivia Rodriguez",
        "role": "UX Researcher",
        "filename": "olivia_rodriguez.png",
        "file_path": "ux_researcher_olivia_rodriguez.org",
        "review_command": "/review-presentation-olivia",
        "focus": "Usability of authentication flows, error handling, accessibility",
        "prompt": """Mid-30s Latina woman with shoulder-length dark brown hair with subtle highlights, warm brown eyes, 
and a friendly, approachable expression. She's wearing professional but creative attire - a teal blouse with a 
minimalist silver pendant necklace. Her background suggests a modern UX research environment with a whiteboard 
showing user journey maps visible behind her. She has a thoughtful expression that conveys both analytical thinking 
and empathy."""
    },
    {
        "id": "diego",
        "name": "Diego Martinez",
        "role": "Collaborative Software Researcher",
        "filename": "diego_martinez.png",
        "file_path": "collaborative_software_researcher_diego_martinez.org",
        "review_command": "/review-presentation-diego",
        "focus": "Integration with workflow tools, cross-system authentication, team dynamics",
        "prompt": """Hispanic male in his early 40s with short black hair with some gray at the temples, trimmed beard, 
rectangular glasses, thoughtful expression. Professional appearance with a navy blue button-up shirt. Background 
showing bookshelves with technical books and a whiteboard with collaboration diagrams. Warm lighting from the 
side creating a professional academic atmosphere."""
    },
    
    # Business role personas
    {
        "id": "michael_chen",
        "name": "Michael Chen",
        "role": "Chief Technology Officer",
        "filename": "michael_chen.png",
        "file_path": "chief_technology_officer_michael_chen.org",
        "review_command": "/review-presentation-michael",
        "focus": "Strategic alignment, technical vision, architectural decisions",
        "prompt": """Professional executive headshot of Michael Chen, a 45-year-old East Asian man with a confident, visionary expression. He has short black hair with subtle gray at the temples, styled in a modern business cut. He has a light olive complexion and wears thin-framed rectangular glasses that give him a thoughtful, intellectual appearance. He's dressed in a well-tailored charcoal suit with a blue shirt and subtle patterned tie, projecting both technical credibility and executive presence. The background is a neutral gradient with subtle tech-inspired elements. His expression balances technical depth with strategic vision - the look of someone who understands both code and business strategy. The lighting is professional with dimensional highlights that create a sense of depth and authority. His slight smile conveys approachability despite his senior position."""
    },
    {
        "id": "isabella",
        "name": "Isabella Garcia",
        "role": "Chief Financial Officer",
        "filename": "isabella_garcia.png",
        "file_path": "chief_financial_officer_isabella_garcia.org",
        "review_command": "/review-presentation-isabella",
        "focus": "Financial implications, ROI, resource allocation, risk management",
        "prompt": """Professional executive headshot of Isabella Garcia, a 48-year-old Latina woman with a poised, confident expression. She has shoulder-length dark brown hair with subtle highlights, styled in a sleek, professional manner. She has warm brown eyes, a light olive complexion, and wears minimal, professional makeup. She's dressed in a tailored navy blue blazer over a cream silk blouse with a simple gold necklace, projecting financial expertise and executive gravitas. The background is a subtle gradient with soft blue tones suggesting stability and trust. Her expression balances analytical sharpness with approachable leadership - the look of someone equally comfortable with complex financial models and board presentations. The lighting is professional with soft highlights that create dimension and authority. Her composed smile conveys confidence and competence."""
    },
    {
        "id": "james_wilson_ceo",
        "name": "James Wilson",
        "role": "Chief Executive Officer",
        "filename": "james_wilson_ceo.png",
        "file_path": "chief_executive_officer_james_wilson.org",
        "review_command": "/review-presentation-james-ceo",
        "focus": "Business strategy, market positioning, organizational alignment",
        "prompt": """Professional executive headshot of James Wilson, a 52-year-old Black man with a commanding yet approachable presence. He has short, neatly trimmed dark hair with distinguished touches of gray at the temples, and a well-groomed short beard. He has deep brown eyes that project warmth and strategic vision, and a medium-dark complexion. He's dressed in a well-tailored charcoal suit with a crisp white shirt and burgundy tie, projecting executive authority and polished leadership. The background is a subtle gradient in neutral tones that suggests a corporate environment without being distracting. His expression balances confident authority with genuine approachability - the composed look of someone who makes high-stakes decisions daily while valuing his team. The lighting is professional with defined highlights that accentuate his strong features while creating a sense of dimension and gravitas. His slight smile conveys both confidence and accessibility."""
    },
    {
        "id": "sarah_martinez",
        "name": "Sarah Martinez",
        "role": "Legal Counsel",
        "filename": "sarah_martinez.png",
        "file_path": "legal_counsel_sarah_martinez.org",
        "review_command": "/review-presentation-sarah",
        "focus": "Legal implications, compliance, risk mitigation, contractual obligations",
        "prompt": """Professional executive headshot of Sarah Martinez, a 43-year-old Latina woman with a confident, attentive expression. She has shoulder-length dark brown hair styled in a sleek bob with side-swept bangs. She has warm brown eyes that convey both intelligence and careful scrutiny, and a light olive complexion. She's dressed in a tailored navy suit jacket over a cream blouse with a subtle gold pendant necklace, projecting legal authority and professional polish. The background is a subtle gradient with muted tones suggesting a corporate legal environment. Her expression balances analytical precision with approachable confidence - the look of someone who pays attention to every detail without losing sight of the big picture. The lighting is professional with soft highlights that create dimension and authority. Her composed, slight smile conveys both competence and steady assurance."""
    },
    {
        "id": "priya_patel",
        "name": "Priya Patel",
        "role": "Site Reliability Engineer",
        "filename": "priya_patel.png",
        "file_path": "site_reliability_engineer_priya_patel.org",
        "review_command": "/review-presentation-priya",
        "focus": "Operational reliability, monitoring, incident response, scalability",
        "prompt": """Professional headshot of Priya Patel, a 34-year-old South Asian woman with a focused, confident expression. She has long dark brown hair pulled back in a practical low ponytail with some loose strands framing her face. She has warm brown eyes that convey both technical precision and problem-solving drive, and a medium brown complexion. She's dressed in a casual but professional navy button-down with the sleeves rolled up, suggesting a hands-on approach to engineering work. The background is a subtle gradient with tech-inspired elements suggesting a modern operations center. Her expression balances technical expertise with calm under pressure - the look of someone who thrives when systems need immediate attention. The lighting is clean and even, emphasizing clarity and focus. Her slight smile shows confidence in her technical abilities while remaining approachable."""
    },
    {
        "id": "aiden_wong",
        "name": "Aiden Wong",
        "role": "User Experience Designer",
        "filename": "aiden_wong.png",
        "file_path": "user_experience_designer_aiden_wong.org",
        "review_command": "/review-presentation-aiden",
        "focus": "User flows, visual design, interaction patterns, usability testing",
        "prompt": """Professional headshot of Aiden Wong, a 32-year-old East Asian man with a creative, perceptive expression. He has medium-length black hair styled with a subtle undercut and modern side part, and wears distinctive round glasses with thin matte black frames that emphasize his design sensibility. He has a light complexion and an engaging expression that suggests both creativity and analytical thinking. He's dressed in a navy blue button-up shirt under a light gray casual blazer with a subtle pattern pocket square, projecting creative professionalism. The background is a clean white gradient suggesting a minimalist design studio. His posture is slightly angled, giving him an engaged, dynamic presence. The lighting is soft but directional, creating gentle shadows that add dimension to his features. His warm smile conveys both empathy and enthusiasm - the look of someone passionate about user-centered design."""
    }
]

# Generate a dictionary for compatibility with older code
PERSONA_PROMPTS = {p["id"]: p["prompt"] for p in PERSONAS}