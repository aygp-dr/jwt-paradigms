;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((org-mode
  (org-confirm-babel-evaluate . nil)
  (org-src-fontify-natively . t)
  (org-babel-load-languages . ((emacs-lisp . t)
                               (shell . t)
                               (python . t)
                               (js . t)
                               (typescript . t)
                               (clojure . t)
                               (scheme . t)
                               (ruby . t)
                               (lisp . t)
                               (hy . t)
                               (rust . t)
                               (C . t)
                               (java . t)
                               (dall-e-shell . t)))
  (eval . (progn
            (require 'ox-latex)
            (require 'ox-html)
            (require 'htmlize)
            (setq org-html-htmlize-output-type 'css)
            (setq org-latex-listings 'minted)
            (add-to-list 'org-latex-packages-alist '("" "minted"))
            (setq org-latex-minted-options
                  '(("frame" "lines")
                    ("fontsize" "\\footnotesize")
                    ("linenos" "")))
            (setq org-latex-pdf-process
                  '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
                    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
            
            ;; Setup for DALL-E image generation in Org mode
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
              (setq dall-e-shell-default-output-dir "./images/png/"))))))