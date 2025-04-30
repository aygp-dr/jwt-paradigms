# Utility Scripts

This directory contains utility scripts for working with the JWT parsing examples repository.

## Scripts

### `tangle_chapters.sh`

This script tangles all chapter files in the `personas/gadfly/chapters/` directory, extracting code blocks into their respective language directories.

Usage:
```bash
./scripts/tangle_chapters.sh
```

### `update_tangle.py`

This Python script updates source blocks in Org files to include tangle and mkdirp directives. It adds the appropriate file extensions and paths for extracting code examples.

Usage:
```bash
python ./scripts/update_tangle.py <directory>
```

Example:
```bash
python ./scripts/update_tangle.py personas/gadfly/chapters/
```
