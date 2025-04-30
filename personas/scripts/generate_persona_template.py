#!/usr/bin/env python3
"""
Generate template files for new personas.
This script creates structured org-mode templates that can be filled in to create new personas.
"""

import argparse
import logging
import os
import random
import sys
from pathlib import Path

# Configure logging
logging.basicConfig(
    level=logging.INFO, format="%(asctime)s - %(name)s - %(levelname)s - %(message)s"
)
logger = logging.getLogger("persona_template")

# Define persona archetype templates
ARCHETYPES = {
    "developer": {
        "roles": [
            "Frontend Developer",
            "Backend Developer",
            "Full-Stack Developer",
            "Mobile Developer",
            "Game Developer",
            "DevOps Engineer",
            "Platform Engineer",
            "Infrastructure Engineer",
            "Site Reliability Engineer",
            "Machine Learning Engineer",
            "Data Engineer",
            "Embedded Systems Engineer",
        ],
        "expertise": [
            "Proficiency in {languages}",
            "Experience with {frameworks} frameworks",
            "Knowledge of software design patterns",
            "Skilled in debugging complex problems",
            "Understanding of clean code principles",
            "Familiar with version control systems",
            "Experience with CI/CD pipelines",
            "Knowledge of testing methodologies",
        ],
        "concerns": [
            "Code maintainability and readability",
            "Technical debt and refactoring opportunities",
            "Performance and scalability issues",
            "Proper error handling and edge cases",
            "Documentation quality and completeness",
            "Implementation complexity vs. simplicity",
            "Following best practices for {domain}",
        ],
        "languages": [
            "JavaScript/TypeScript",
            "Python",
            "Java",
            "C#",
            "Go",
            "Rust",
            "C/C++",
            "PHP",
            "Ruby",
            "Swift",
            "Kotlin",
        ],
        "frameworks": [
            "React",
            "Angular",
            "Vue.js",
            "Next.js",
            "Django",
            "Flask",
            "Spring Boot",
            "ASP.NET Core",
            "Express.js",
            "Laravel",
            "Ruby on Rails",
            "Svelte",
            "TensorFlow",
            "PyTorch",
        ],
        "domains": [
            "web development",
            "mobile development",
            "cloud services",
            "embedded systems",
            "machine learning",
            "game development",
            "blockchain",
            "IoT",
            "enterprise software",
        ],
    },
    "security": {
        "roles": [
            "Security Engineer",
            "Security Architect",
            "Penetration Tester",
            "Security Researcher",
            "Application Security Specialist",
            "Cryptography Expert",
            "Security Compliance Analyst",
            "Threat Intelligence Analyst",
            "Security Operations Analyst",
        ],
        "expertise": [
            "Deep knowledge of security principles and best practices",
            "Experience with threat modeling and risk assessment",
            "Familiarity with common vulnerabilities and attack vectors",
            "Understanding of cryptographic concepts and implementations",
            "Knowledge of security standards (OWASP, NIST, etc.)",
            "Experience with security tools and frameworks",
            "Network security and infrastructure protection",
            "Secure coding practices and code review",
        ],
        "concerns": [
            "Authentication and authorization mechanisms",
            "Input validation and sanitization",
            "Secure data handling and privacy",
            "Protection against common attack vectors",
            "Security logging and monitoring",
            "Compliance with security standards",
            "Third-party dependency risks",
            "Implementation of security controls",
        ],
    },
    "quality": {
        "roles": [
            "Quality Assurance Engineer",
            "Test Automation Engineer",
            "QA Architect",
            "Performance Testing Specialist",
            "Accessibility Testing Specialist",
            "Test Manager",
        ],
        "expertise": [
            "Experience with testing methodologies",
            "Knowledge of test automation frameworks",
            "Understanding of quality metrics and reporting",
            "Performance testing and optimization",
            "Cross-browser and cross-platform testing",
            "API testing and validation",
            "User acceptance testing",
            "Regression testing strategies",
        ],
        "concerns": [
            "Test coverage and completeness",
            "Edge cases and error handling",
            "Regression and stability issues",
            "Performance under load and stress",
            "Usability and user experience issues",
            "Consistency across platforms",
            "Documentation and test instructions",
            "Reliability and flakiness of tests",
        ],
    },
    "design": {
        "roles": [
            "UX Designer",
            "UI Designer",
            "Interaction Designer",
            "Product Designer",
            "Design Systems Specialist",
            "Information Architect",
            "Accessibility Specialist",
        ],
        "expertise": [
            "User-centered design principles",
            "Interaction design patterns",
            "Visual design and composition",
            "Design systems and component libraries",
            "Wireframing and prototyping",
            "User research and testing methodologies",
            "Accessibility guidelines and standards",
            "Design tools and software",
        ],
        "concerns": [
            "User experience and intuitive interactions",
            "Visual consistency and brand alignment",
            "Accessibility and inclusive design",
            "Responsive and adaptive layouts",
            "Information architecture and navigation",
            "Performance impact of design decisions",
            "Design-development handoff quality",
            "Design scalability and maintenance",
        ],
    },
    "product": {
        "roles": [
            "Product Manager",
            "Product Owner",
            "Business Analyst",
            "Project Manager",
            "Program Manager",
            "Technical Product Manager",
            "Growth Product Manager",
            "User Researcher",
        ],
        "expertise": [
            "Product development lifecycle",
            "User story creation and refinement",
            "Feature prioritization and roadmap planning",
            "Market and competitive analysis",
            "Stakeholder management and communication",
            "Data-driven decision making",
            "User research and requirements gathering",
            "Agile and other development methodologies",
        ],
        "concerns": [
            "Feature alignment with user needs",
            "Business value and ROI",
            "Project scope and feature creep",
            "Timeline and resource constraints",
            "Cross-functional team coordination",
            "Technical feasibility vs. business needs",
            "Product metrics and success criteria",
            "User adoption and satisfaction",
        ],
    },
    "leadership": {
        "roles": [
            "Engineering Manager",
            "Technical Director",
            "CTO",
            "VP of Engineering",
            "Director of Development",
            "Software Development Manager",
            "Technical Lead",
        ],
        "expertise": [
            "Technical leadership and mentorship",
            "Strategic planning and roadmap development",
            "Team building and talent development",
            "Project and resource management",
            "Cross-functional collaboration",
            "Technology selection and architecture",
            "Performance management and growth",
            "Budget planning and resource allocation",
        ],
        "concerns": [
            "Architectural decisions and technical direction",
            "Team productivity and efficiency",
            "Code quality and technical standards",
            "Technical debt management",
            "Risk assessment and mitigation",
            "Resource allocation and prioritization",
            "Talent retention and development",
            "Alignment with business objectives",
        ],
    },
}

# Personality trait options
PERSONALITY_TRAITS = {
    "communication_style": [
        "direct and straightforward",
        "diplomatic and tactful",
        "analytical and precise",
        "patient and educational",
        "enthusiastic and energetic",
        "concise and to-the-point",
        "thoughtful and measured",
        "friendly and approachable",
    ],
    "approach": [
        "detail-oriented and thorough",
        "big-picture and strategic",
        "practical and pragmatic",
        "innovative and creative",
        "cautious and conservative",
        "bold and confident",
        "collaborative and team-focused",
        "independent and self-driven",
    ],
    "temperament": [
        "calm and composed",
        "passionate and expressive",
        "serious and focused",
        "playful and light-hearted",
        "methodical and structured",
        "flexible and adaptable",
        "decisive and quick",
        "reflective and deliberate",
    ],
}

# Define appearance characteristics for diversity
APPEARANCE = {
    "age_ranges": [
        "late 20s",
        "early 30s",
        "mid 30s",
        "late 30s",
        "early 40s",
        "mid 40s",
        "late 40s",
        "early 50s",
        "mid 50s",
        "late 50s",
        "early 60s",
    ],
    "ethnicities": [
        "East Asian",
        "South Asian",
        "Southeast Asian",
        "Black",
        "African American",
        "Afro-Caribbean",
        "Latino",
        "Latina",
        "Hispanic",
        "Middle Eastern",
        "White",
        "Caucasian",
        "Indigenous",
        "Native American",
        "Pacific Islander",
        "Multiracial",
    ],
    "hair_styles": [
        "short and neatly styled",
        "medium-length and professional",
        "long and flowing",
        "wavy",
        "curly",
        "straight",
        "pulled back in a bun",
        "braided",
        "natural",
        "bald",
        "shaved",
        "cropped",
        "undercut",
        "salt-and-pepper",
        "graying at the temples",
        "completely gray",
    ],
    "clothing": [
        "professional suit",
        "business casual attire",
        "casual but neat clothing",
        "smart casual outfit",
        "tech-casual look",
        "creative professional style",
        "button-up shirt",
        "blouse",
        "polo shirt",
        "blazer",
        "cardigan",
        "sweater",
        "jacket",
    ],
    "accessories": [
        "glasses",
        "stylish eyewear",
        "minimal jewelry",
        "statement necklace",
        "professional watch",
        "subtle earrings",
        "no visible accessories",
        "patterned tie",
        "colorful scarf",
    ],
}


def setup_args():
    """Parse command line arguments."""
    parser = argparse.ArgumentParser(description="Generate persona template files")
    parser.add_argument(
        "--output-dir", type=str, default="personas", help="Directory to save persona templates"
    )
    parser.add_argument(
        "--format",
        type=str,
        choices=["org", "md"],
        default="org",
        help="Output file format (org-mode or markdown)",
    )
    parser.add_argument("--count", type=int, default=1, help="Number of templates to generate")
    parser.add_argument(
        "--archetype",
        type=str,
        choices=list(ARCHETYPES.keys()),
        help="Specific archetype to use (random if not specified)",
    )
    parser.add_argument(
        "--dry-run", action="store_true", help="Don't write files, just show what would be done"
    )
    return parser.parse_args()


def generate_random_persona():
    """Generate a random persona profile."""
    # Select random archetype
    archetype_key = random.choice(list(ARCHETYPES.keys()))
    archetype = ARCHETYPES[archetype_key]

    # Select random role
    role = random.choice(archetype["roles"])

    # Generate a name based on random demographic characteristics
    # Random selection for diversity
    gender = random.choice(["male", "female", "non-binary"])
    first_names_male = [
        "James",
        "Michael",
        "David",
        "John",
        "Robert",
        "William",
        "Richard",
        "Joseph",
        "Thomas",
        "Christopher",
        "Daniel",
        "Matthew",
        "Anthony",
        "Mark",
        "Andrew",
        "Raj",
        "Wei",
        "Carlos",
        "Ahmed",
        "Jamal",
        "Hiroshi",
        "Miguel",
        "Kwame",
        "Dmitri",
        "Mohammed",
        "Juan",
        "Ali",
        "Chen",
        "Mateo",
        "Alejandro",
        "Omar",
        "Samuel",
        "Jordan",
    ]

    first_names_female = [
        "Mary",
        "Patricia",
        "Jennifer",
        "Linda",
        "Elizabeth",
        "Barbara",
        "Susan",
        "Jessica",
        "Sarah",
        "Karen",
        "Lisa",
        "Nancy",
        "Betty",
        "Sandra",
        "Margaret",
        "Priya",
        "Mei",
        "Maria",
        "Fatima",
        "Aisha",
        "Yuki",
        "Sofia",
        "Ngozi",
        "Olga",
        "Amina",
        "Gabriela",
        "Zainab",
        "Lin",
        "Carmen",
        "Isabella",
        "Layla",
        "Samantha",
        "Taylor",
    ]

    first_names_nb = [
        "Alex",
        "Jordan",
        "Taylor",
        "Morgan",
        "Casey",
        "Riley",
        "Avery",
        "Quinn",
        "Skylar",
        "Dakota",
        "Reese",
        "Emerson",
        "Phoenix",
        "River",
        "Sage",
        "Kai",
        "Ash",
        "Blair",
        "Robin",
        "Jamie",
        "Jessie",
        "Ari",
        "Shay",
        "Hayden",
        "Parker",
        "Remy",
    ]

    last_names = [
        "Smith",
        "Johnson",
        "Williams",
        "Brown",
        "Jones",
        "Miller",
        "Davis",
        "Garcia",
        "Rodriguez",
        "Wilson",
        "Martinez",
        "Anderson",
        "Taylor",
        "Thomas",
        "Hernandez",
        "Moore",
        "Martin",
        "Jackson",
        "Thompson",
        "White",
        "Lopez",
        "Lee",
        "Gonzalez",
        "Harris",
        "Clark",
        "Lewis",
        "Robinson",
        "Walker",
        "Perez",
        "Hall",
        "Young",
        "Allen",
        "Sanchez",
        "Wright",
        "King",
        "Scott",
        "Green",
        "Baker",
        "Adams",
        "Nelson",
        "Hill",
        "Ramirez",
        "Campbell",
        "Mitchell",
        "Roberts",
        "Carter",
        "Phillips",
        "Evans",
        "Turner",
        "Torres",
        "Parker",
        "Collins",
        "Edwards",
        "Stewart",
        "Flores",
        "Morris",
        "Nguyen",
        "Murphy",
        "Rivera",
        "Cook",
        "Rogers",
        "Morgan",
        "Peterson",
        "Cooper",
        "Reed",
        "Bailey",
        "Bell",
        "Gomez",
        "Kelly",
        "Howard",
        "Ward",
        "Cox",
        "Diaz",
        "Richardson",
        "Wood",
        "Watson",
        "Brooks",
        "Bennett",
        "Gray",
        "James",
        "Reyes",
        "Cruz",
        "Hughes",
        "Price",
        "Myers",
        "Long",
        "Foster",
        "Sanders",
        "Ross",
        "Morales",
        "Powell",
        "Sullivan",
        "Russell",
        "Ortiz",
        "Jenkins",
        "Gutierrez",
        "Perry",
        "Butler",
        "Barnes",
        "Fisher",
        "Wang",
        "Chen",
        "Patel",
        "Singh",
        "Kim",
        "Park",
        "Ali",
        "Shah",
        "Khan",
        "Cohen",
    ]

    if gender == "male":
        first_name = random.choice(first_names_male)
    elif gender == "female":
        first_name = random.choice(first_names_female)
    else:
        first_name = random.choice(first_names_nb)

    last_name = random.choice(last_names)
    name = f"{first_name} {last_name}"

    # Random appearance characteristics
    age_range = random.choice(APPEARANCE["age_ranges"])
    ethnicity = random.choice(APPEARANCE["ethnicities"])
    hair_style = random.choice(APPEARANCE["hair_styles"])
    clothing = random.choice(APPEARANCE["clothing"])
    accessories = random.choice(APPEARANCE["accessories"])

    # Random personality traits
    communication = random.choice(PERSONALITY_TRAITS["communication_style"])
    approach = random.choice(PERSONALITY_TRAITS["approach"])
    temperament = random.choice(PERSONALITY_TRAITS["temperament"])

    # Select expertise and concerns
    num_expertise = random.randint(4, 6)
    num_concerns = random.randint(3, 5)

    # Fill in templates with specifics if needed
    expertise_list = random.sample(
        archetype["expertise"], min(num_expertise, len(archetype["expertise"]))
    )
    concerns_list = random.sample(
        archetype["concerns"], min(num_concerns, len(archetype["concerns"]))
    )

    # Add specifics for certain archetypes
    if archetype_key == "developer":
        languages = random.sample(archetype["languages"], random.randint(2, 4))
        frameworks = random.sample(archetype["frameworks"], random.randint(2, 3))
        domain = random.choice(archetype["domains"])

        # Replace placeholders in expertise and concerns
        expertise_list = [
            item.format(
                languages=", ".join(languages), frameworks=", ".join(frameworks), domain=domain
            )
            if "{" in item
            else item
            for item in expertise_list
        ]

        concerns_list = [
            item.format(domain=domain) if "{" in item else item for item in concerns_list
        ]

    persona = {
        "name": name,
        "role": role,
        "gender": gender,
        "age_range": age_range,
        "ethnicity": ethnicity,
        "hair_style": hair_style,
        "clothing": clothing,
        "accessories": accessories,
        "communication": communication,
        "approach": approach,
        "temperament": temperament,
        "expertise": expertise_list,
        "concerns": concerns_list,
        "archetype": archetype_key,
    }

    return persona


def generate_org_template(persona):
    """Generate an org-mode template file content."""
    name = persona["name"]
    role = persona["role"]

    org_content = [
        f"#+TITLE: {name}",
        "#+AUTHOR: JWT Parsing Examples",
        f"#+DATE: {os.popen('date +%Y-%m-%d').read().strip()}",
        "#+PROPERTY: header-args:python :results output :exports both",
        "",
        f"* {name} ({role})",
        "",
        "** Role",
        "",
        f"{role} with expertise in {persona['expertise'][0][:30].lower()}...",
        "",
        "** Description",
        "",
        f"{name} is a {persona['age_range']} {persona['ethnicity']} "
        f"{persona['gender'] if persona['gender'] != 'non-binary' else 'person'} with "
        f"{persona['hair_style']} hair. They typically wear {persona['clothing']} with "
        f"{persona['accessories']}.",
        "",
        f"They are {persona['communication']}, {persona['approach']}, and generally "
        f"{persona['temperament']} in their work. {name} brings a unique perspective to code "
        f"review based on their background as a {role}.",
        "",
        "** Expertise",
    ]

    # Add expertise items
    for item in persona["expertise"]:
        org_content.append(f"- {item}")

    org_content.extend(
        [
            "",
            "** Main Concerns",
        ]
    )

    # Add concerns
    for item in persona["concerns"]:
        org_content.append(f"- {item}")

    org_content.extend(
        [
            "",
            "** Speaking Style",
            "",
            f"{name} speaks in a {persona['communication']} manner. Their feedback tends to be "
            f"{persona['approach']} and {persona['temperament']}.",
            "",
            "** Review Approach",
            "",
            f"When reviewing code, {name} primarily focuses on:",
            "",
            "1. [First major concern aligned with their expertise]",
            "2. [Second major concern aligned with their expertise]",
            "3. [Third major concern aligned with their expertise]",
            "",
            "They are most impressed by code that demonstrates:",
            "",
            "- [Quality 1 related to their expertise]",
            "- [Quality 2 related to their expertise]",
            "- [Quality 3 related to their expertise]",
            "",
        ]
    )

    return "\n".join(org_content)


def generate_markdown_template(persona):
    """Generate a markdown template file content."""
    name = persona["name"]
    role = persona["role"]

    md_content = [
        f"# {name}",
        "",
        "## Role",
        "",
        f"{role} with expertise in {persona['expertise'][0][:30].lower()}...",
        "",
        "## Description",
        "",
        f"{name} is a {persona['age_range']} {persona['ethnicity']} "
        f"{persona['gender'] if persona['gender'] != 'non-binary' else 'person'} with "
        f"{persona['hair_style']} hair. They typically wear {persona['clothing']} with "
        f"{persona['accessories']}.",
        "",
        f"They are {persona['communication']}, {persona['approach']}, and generally "
        f"{persona['temperament']} in their work. {name} brings a unique perspective to code "
        f"review based on their background as a {role}.",
        "",
        "## Expertise",
    ]

    # Add expertise items
    for item in persona["expertise"]:
        md_content.append(f"- {item}")

    md_content.extend(
        [
            "",
            "## Main Concerns",
        ]
    )

    # Add concerns
    for item in persona["concerns"]:
        md_content.append(f"- {item}")

    md_content.extend(
        [
            "",
            "## Speaking Style",
            "",
            f"{name} speaks in a {persona['communication']} manner. Their feedback tends to be "
            f"{persona['approach']} and {persona['temperament']}.",
            "",
            "## Review Approach",
            "",
            f"When reviewing code, {name} primarily focuses on:",
            "",
            "1. [First major concern aligned with their expertise]",
            "2. [Second major concern aligned with their expertise]",
            "3. [Third major concern aligned with their expertise]",
            "",
            "They are most impressed by code that demonstrates:",
            "",
            "- [Quality 1 related to their expertise]",
            "- [Quality 2 related to their expertise]",
            "- [Quality 3 related to their expertise]",
            "",
        ]
    )

    return "\n".join(md_content)


def main():
    """Main entry point for the script."""
    args = setup_args()

    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)

    logger.info(f"Generating {args.count} persona template(s) in {args.format} format")

    for i in range(args.count):
        # Generate random persona or use specified archetype
        if args.archetype:
            # Override random selection with the specified archetype
            persona = generate_random_persona()
            while persona["archetype"] != args.archetype:
                persona = generate_random_persona()
        else:
            persona = generate_random_persona()

        # Create filename from name
        safe_name = persona["name"].lower().replace(" ", "_").replace('"', "").replace("'", "")

        if args.format == "org":
            content = generate_org_template(persona)
            filename = f"{safe_name}.org"
        else:
            content = generate_markdown_template(persona)
            filename = f"{safe_name}.md"

        filepath = output_dir / filename

        # Check if file already exists (avoid overwriting)
        if filepath.exists():
            logger.warning(f"File {filepath} already exists, adding unique suffix")
            filepath = output_dir / f"{safe_name}_{i}.{args.format}"

        if not args.dry_run:
            with open(filepath, "w", encoding="utf-8") as f:
                f.write(content)
            logger.info(f"Created {filepath}")
        else:
            logger.info(f"Would create {filepath}")

    logger.info("Template generation complete")
    return 0


if __name__ == "__main__":
    sys.exit(main())
