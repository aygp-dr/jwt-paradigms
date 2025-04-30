#!/usr/bin/env python3
"""
This module contains prompts for generating persona headshots.
These are moved to a separate file to improve readability and maintainability.
"""

# Define persona prompts

WELLINGTON_PROMPT = """Professional headshot of Professor Marcus Wellington, a 57-year-old tenured
Computer Science professor with a completely bald head and a well-trimmed salt-and-pepper beard.
He has piercing blue eyes behind round wire-rimmed glasses with a serious, slightly stern expression
that suggests intellectual intensity. He's wearing a dark turtleneck with a tweed jacket.
The background is a neutral dark gray. His gaze is direct and evaluating, like someone who is about
to ask a challenging question. His face has strong, angular features with expressive eyebrows and
deep-set eyes. Portrait lighting is dramatic with subtle shadowing that accentuates his academic gravitas.
His expression conveys both brilliance and skepticism."""

RAJ_PROMPT = """Professional headshot of Raj Patel, a 31-year-old Indian-American man with short,
slightly wavy black hair styled neatly. He has medium brown skin, thoughtful dark eyes behind modern
round glasses with thin metal frames, and a well-groomed short beard. He's wearing a light blue button-up
shirt with a clean, professional appearance. The background is neutral dark gray. His expression is
slightly smiling, conveying intelligence and curiosity. His face has a defined jawline and his posture
is upright and engaged. Portrait lighting is crisp and professional, creating subtle shadows that highlight
his facial structure."""

MAYA_PROMPT = """Professional headshot of Dr. Maya Ramirez, a 42-year-old Latina woman with straight
black hair with subtle gray streaks falling to shoulder length. She has an olive complexion, intelligent
brown eyes behind stylish rectangular glasses with dark frames, and a serious but confident expression.
She's wearing a burgundy blouse under a dark blazer with a small pendant necklace. The background features
blurred bookshelves suggesting an academic environment. Her face has defined cheekbones, and she has a
poised, analytical demeanor. Portrait lighting is structured and professional, emphasizing her academic
authority. Her makeup is minimal and professional."""

HANNAH_PROMPT = """Professional headshot of Hannah Chen, a 36-year-old East Asian woman with straight
black hair cut in a sleek, shoulder-length style with side-swept bangs. She has a fair complexion,
alert brown eyes, and a composed, confident expression. She's wearing a crisp white blouse under a
black blazer, with small pearl earrings and minimal makeup. The background is a neutral dark gray.
Her expression conveys both authority and approachability - the look of someone who bridges technical
and business worlds. Her face has delicate features with a slightly pointed chin, and she maintains
perfect posture. Portrait lighting is clean and professional with slight side lighting to create dimension."""

ZERO_PROMPT = """Professional but dramatic portrait of Alex "Zero" Chen, a cyberpunk-styled graduate
student in their late 20s with an ambiguous gender presentation. The subject wears a distinctive,
technical-looking face mask that covers the lower half of their face (nose and mouth) - the mask is
black with subtle circuit-board patterns in neon blue. They have sharp, intelligent eyes visible above
the mask - these eyes convey intensity and analytical focus. Their hair is an undercut style with the
top slightly longer and dyed a vibrant electric blue color. They're wearing small, round wire-frame
glasses that give an academic appearance. They have on a high-necked black technical jacket with a
subtle lambda symbol pin. The background is dark with faint code elements barely visible. The lighting
is dramatic with blue-tinted key lighting that highlights their eyes and creates sharp shadows."""

SOFIA_PROMPT = """Professional headshot of Sofia Martinez, a 28-year-old Latina woman with medium-length
straight dark brown hair that falls just above her shoulders. She has warm brown eyes, a friendly smile,
and a welcoming expression. She's wearing a navy blue professional top with minimal, tasteful jewelry
(small stud earrings). The background is a neutral light gray. Her appearance conveys the enthusiasm
and approachability of a former teacher now working as a junior developer. Portrait lighting is soft
and flattering, highlighting her warm complexion and friendly demeanor. She has a slightly rounded
face shape with defined eyebrows and natural makeup."""

NEHA_PROMPT = """Professional headshot of Dr. Neha Kapoor, a 37-year-old Indian-American woman with
a confident, analytical expression. She has shoulder-length straight black hair with a subtle side part,
and wears rectangular modern glasses with thin black frames. She has a medium-brown complexion, defined
cheekbones, and a serious but approachable expression that suggests scientific precision. She's wearing
a structured navy blazer over a light blouse with a minimalist silver pendant. The background is a
gradient gray. Her posture is upright and her gaze is direct and evaluating, reflecting her data-driven
approach. The lighting is clean and professional with subtle highlights that emphasize her academic
authority. Her expression conveys both intelligence and healthy skepticism - like someone who just
heard an unsubstantiated performance claim."""

MARCO_PROMPT = """Professional headshot of Marco Hernandez, a 41-year-old Latino man with a warm,
empathetic expression. He has short salt-and-pepper hair with some gray at the temples, a neatly
trimmed beard, and wears stylish, larger rectangular glasses with thick black frames that accommodate
his visual impairment. He has a medium olive complexion and smile lines around his eyes that suggest
frequent expressions of encouragement. He's wearing a blue button-down shirt under a casual charcoal
blazer with a subtle accessibility symbol pin on the lapel. The background is a light, clean gradient.
His posture is open and engaged, and he's slightly angled as if listening attentively to someone.
The lighting is warm and even, creating a welcoming feel. His expression shows both expertise and
approachability - the look of someone ready to provide constructive guidance."""

ELEANOR_PROMPT = """Professional executive headshot of Dr. Eleanor Reynolds, a 48-year-old woman with
an authoritative yet approachable presence. She has shoulder-length silver-streaked brown hair styled
in a sophisticated bob. She has a fair complexion with natural smile lines that suggest experience,
and wears minimal but professional makeup including a subtle mauve lipstick. Her expression is confident
and discerning - the look of someone who makes multi-million dollar decisions daily. She's wearing a
tailored charcoal suit jacket over a burgundy blouse with a simple gold necklace. The background is a
soft gradient of deep blue. Her posture is straight and commanding, and her gaze is direct but not
intimidating. The lighting is professional with subtle edge lighting that highlights her strong bone
structure and creates depth. Her expression balances executive authority with thoughtful intelligence."""

# Collection of all prompts for easy access
PERSONA_PROMPTS = {
    "wellington": WELLINGTON_PROMPT,
    "raj": RAJ_PROMPT,
    "maya": MAYA_PROMPT,
    "hannah": HANNAH_PROMPT,
    "zero": ZERO_PROMPT,
    "sofia": SOFIA_PROMPT,
    "neha": NEHA_PROMPT,
    "marco": MARCO_PROMPT,
    "eleanor": ELEANOR_PROMPT,
}
