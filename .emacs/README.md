# Emacs Configuration

This directory contains Emacs-specific configuration files for the JWT parsing examples repository.

## Files

### `dall-e-config.el`

Configuration for DALL-E image generation in Org mode blocks. This file contains:

- API key configuration for OpenAI
- DALL-E model parameters (model version, image size, style, quality)
- Image generation settings

The configuration is used by the `.dir-locals.el` file in the root directory when processing Org mode files that contain DALL-E image generation code blocks.