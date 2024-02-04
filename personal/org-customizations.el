(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))

;; M-x pixel-scroll-precision-mode
;; How to set it in org mode by default?

(defun org-syntax-table-modify ()
  "Modify `org-mode-syntax-table' for the current org buffer."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(add-hook 'org-mode-hook #'org-syntax-table-modify)

(setq org-image-actual-width nil)

(setq org-list-allow-alphabetical t)

;; Export to LateX PDF using minted package
(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
