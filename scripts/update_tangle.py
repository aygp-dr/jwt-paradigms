#!/usr/bin/env python3

import os
import re
import sys

def update_src_blocks(directory):
    """Update source blocks in Org files to include tangle and mkdirp directives."""
    src_block_regex = re.compile(r'#\+BEGIN_SRC\s+([a-zA-Z0-9_+\-]+)(\s*)?$')
    
    for root, dirs, files in os.walk(directory):
        for file in files:
            if file.endswith('.org'):
                filepath = os.path.join(root, file)
                updated_content = []
                updated = False
                
                with open(filepath, 'r', encoding='utf-8') as f:
                    content = f.readlines()
                
                for line in content:
                    match = src_block_regex.match(line.strip())
                    if match:
                        language = match.group(1)
                        # Skip if it already has a tangle directive
                        if ':tangle' not in line:
                            # Construct language-appropriate filename
                            extension = get_extension_for_language(language)
                            if extension:
                                # Extract chapter name from filename (e.g., chapter01 from chapter01.org)
                                chapter_name = os.path.splitext(os.path.basename(filepath))[0]
                                # Create consistent path: ../examples/language/chapter_name_example_num.ext
                                tangle_path = f"../examples/{language.lower()}/{chapter_name}_{language.lower()}_example.{extension}"
                                # Add tangle and mkdirp options
                                updated_line = f"#+BEGIN_SRC {language} :tangle {tangle_path} :mkdirp yes\n"
                                updated_content.append(updated_line)
                                updated = True
                                continue
                    
                    updated_content.append(line)
                
                if updated:
                    print(f"Updating {filepath}")
                    with open(filepath, 'w', encoding='utf-8') as f:
                        f.writelines(updated_content)

def get_extension_for_language(language):
    """Return appropriate file extension for a given language."""
    extensions = {
        'c': 'c',
        'pascal': 'pas',
        'java': 'java',
        'haskell': 'hs',
        'clojure': 'clj',
        'smalltalk': 'st',
        'ruby': 'rb',
        'prolog': 'pl',
        'jsx': 'jsx',
        'js': 'js',
        'javascript': 'js',
        'typescript': 'ts',
        'python': 'py',
        'scala': 'scala',
        'fsharp': 'fs',
        'rust': 'rs',
        'sql': 'sql',
        'makefile': 'mk',
        'hcl': 'tf',
        'minizinc': 'mzn',
        'text': 'txt',
        'lisp': 'lisp',
        'ocaml': 'ml',
        'idris': 'idr',
        'proto': 'proto',
    }
    
    return extensions.get(language.lower(), 'txt')

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python update_tangle.py <directory>")
        sys.exit(1)
    
    directory = sys.argv[1]
    if not os.path.isdir(directory):
        print(f"Error: {directory} is not a valid directory")
        sys.exit(1)
    
    update_src_blocks(directory)
    print("Source blocks updated successfully!")