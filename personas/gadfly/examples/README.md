# Code Examples from "Paradigms Lost"

This directory contains code examples extracted from the "Paradigms Lost" book. The examples demonstrate various programming paradigms, languages, and concepts discussed in the book.

## Directory Structure

Examples are organized by language/paradigm:

- `tla/` - TLA+ specifications for formal verification (Chapter 13)
- `idris/` - Idris examples demonstrating dependent types (Chapter 13)
- `scala/` - Scala examples showing multi-paradigm features (Chapter 15)
- `fsharp/` - F# examples of functional-first with OO capabilities (Chapter 15)
- `typescript/` - TypeScript examples of gradual typing (Chapter 15)
- `python/` - Python examples including property-based testing (Chapter 13)
- `racket/` - Racket examples of language creation (Chapter 12)
- `smalltalk/` - Smalltalk examples demonstrating OO concepts (Chapter 14)

## Running the Examples

### Prerequisites

To run all examples, you'll need the following tools installed:

- **Scala**: Install via [scala-lang.org](https://www.scala-lang.org/download/)
- **F#**: Install via [fsharp.org](https://fsharp.org/use/)
- **TypeScript**: Install via npm: `npm install -g typescript`
- **Python**: Install from [python.org](https://www.python.org/downloads/)
- **Idris**: Install from [idris-lang.org](https://www.idris-lang.org/pages/download.html)
- **Racket**: Install from [racket-lang.org](https://racket-lang.org/download/)
- **TLA+**: Install TLA+ Toolbox from [lamport.azurewebsites.net/tla/toolbox.html](https://lamport.azurewebsites.net/tla/toolbox.html)

### Running Specific Examples

#### Scala
```bash
scala scala/example_file.scala
```

#### F#
```bash
dotnet fsi fsharp/example_file.fs
```

#### TypeScript
```bash
# Compile
tsc typescript/example_file.ts
# Run
node typescript/example_file.js
```

#### Python
```bash
python python/example_file.py
```

#### Racket
```bash
racket racket/example_file.rkt
```

#### Idris
```bash
idris -o example idris/example_file.idr
./example
```

#### TLA+
Open the .tla file in the TLA+ Toolbox to check and run the specification.

## Regenerating Examples

Examples are tangled from the original book chapters. To regenerate them:

```bash
cd ..
make prepare-src-blocks  # Convert example blocks to proper source blocks
make tangle              # Extract code from chapters
make check-examples      # Validate syntax of generated examples
```

## Notes on Examples

- These examples are designed primarily for educational purposes
- Some examples may be fragments rather than complete programs
- Examples focus on demonstrating specific concepts rather than optimal implementations
- Some examples might require additional dependencies or setup to run