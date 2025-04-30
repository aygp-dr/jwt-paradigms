;; DALL-E image generation configuration for JWT parsing examples
;; To use: (load-file "dall-e-config.el") in your Emacs

;; Add dall-e-shell to org-babel languages
(with-eval-after-load 'org
  (add-to-list 'org-babel-load-languages '(dall-e-shell . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; Load and configure ob-dall-e-shell
(when (require 'ob-dall-e-shell nil t)
  ;; Configure dall-e-shell with API key from environment
  (setq dall-e-shell-openai-key 
        (or (getenv "OPENAI_API_KEY")
            (progn 
              (message "Warning: OPENAI_API_KEY environment variable not set! Image generation will fail.")
              nil)))
  
  ;; Configure DALL-E parameters
  (setq dall-e-shell-model "dall-e-3")
  (setq dall-e-shell-size "1024x1024")
  (setq dall-e-shell-style "photographic")
  (setq dall-e-shell-quality "standard")
  
  ;; Set default directory for image outputs as relative path
  (setq dall-e-shell-default-output-dir "./personas/images/png/")
  
  (message "DALL-E configuration loaded successfully"))

;; Provide instructions
(message "To generate persona images, open generate-persona-images.org and execute code blocks with C-c C-c")