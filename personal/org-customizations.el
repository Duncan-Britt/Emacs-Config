;; ORG AGENDA
(setq org-agenda-files (list "~/Dropbox/agenda/agenda.org"))
(setq org-archive-location "~/Dropbox/agenda/agenda_archive.org::%s_archive")


(setq org-plantuml-jar-path (expand-file-name "~/plantuml-1.2024.4.jar"))
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;; org src blocks
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (emacs-lisp . t)
   (python . t)
   (ruby . t)
   (js . t)
   (C . t)   
   (octave . t)
   (latex . t)
   (lisp . t)
   (dot . t)
   (matlab . t)
   (sql . t)
   (plantuml . t)
   (shell . t)
   ))

;; Needed to run mysql in org babel
(add-to-list 'exec-path "/usr/local/mysql-8.3.0-macos14-x86_64/bin")

(setq org-babel-python-command "python3")

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (org-bullets-mode 1)
                             (org-indent-mode 1))))
;; Enable visual line mode for org buffers.
(add-hook 'org-mode-hook 'visual-line-mode)

;; Org appear stuff
(setq org-hide-emphasis-markers t) ; Hide /emphasis/ markers in org mode
(setq org-appear-autolinks t)
(setq org-pretty-entities t)
(setq org-pretty-entities-include-sub-superscripts nil) ; <-- This doesn't play nicely with under_scores
(setq org-appear-autoentities t)
(setq org-appear-autosubmarkers t)
(add-hook 'org-mode-hook 'org-appear-mode)

;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; take notes when clocking out
(setq org-log-note-clock-out t)

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

;; Disable line numbers in org mode
(add-hook 'org-mode-hook
          (lambda ()
            (message "ran fn")
            (display-line-numbers-mode 0)))

(use-package olivetti
  :custom (olivetti-body-width 100)
  :hook (org-mode . olivetti-mode))

;; Enable the markdown export backend for Org mode
(setq org-export-backends '(ascii html icalendar latex md))
(use-package ox-gfm
  :ensure t
  :after org)
