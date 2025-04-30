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
              ;; Load DALL-E configuration if available
              (load-file (expand-file-name ".emacs/dall-e-config.el" (projectile-project-root)))
              
              ;; Set default directory for image outputs as relative path
              (setq dall-e-shell-default-output-dir "./images/png/"))))))