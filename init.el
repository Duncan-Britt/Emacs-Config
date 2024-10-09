;;; init.el --- Prelude's configuration entry point.
;;
;; Copyright (c) 2011-2023 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.1.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This file simply sets up the default load path and requires
;; the various modules defined within Emacs Prelude.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
                                        ;(package-initialize)

(defvar prelude-user
  (getenv
   (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(message "[Prelude] Prelude is powering up... Be patient, Master %s!" prelude-user)

(when (version< emacs-version "25.1")
  (error "[Prelude] Prelude requires GNU Emacs 25.1 or newer, but you're running %s" emacs-version))

;; Always load newest byte code
(setq load-prefer-newer t)

;; Define Prelude's directory structure
(defvar prelude-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Prelude distribution.")
(defvar prelude-core-dir (expand-file-name "core" prelude-dir)
  "The home of Prelude's core functionality.")
(defvar prelude-modules-dir (expand-file-name  "modules" prelude-dir)
  "This directory houses all of the built-in Prelude modules.")
(defvar prelude-personal-dir (expand-file-name "personal" prelude-dir)
  "This directory is for your personal configuration.

Users of Emacs Prelude are encouraged to keep their personal configuration
changes in this directory.  All Emacs Lisp files there are loaded automatically
by Prelude.")
(defvar prelude-personal-preload-dir (expand-file-name "preload" prelude-personal-dir)
  "This directory is for your personal configuration,
that you want loaded before Prelude.")
(defvar prelude-vendor-dir (expand-file-name "vendor" prelude-dir)
  "This directory houses packages that are not yet available in ELPA (or MELPA).")
(defvar prelude-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "This folder stores all the automatically generated save/history-files.")
(defvar prelude-modules-file (expand-file-name "prelude-modules.el" prelude-personal-dir)
  "This file contains a list of modules that will be loaded by Prelude.")

(unless (file-exists-p prelude-savefile-dir)
  (make-directory prelude-savefile-dir))

(defun prelude-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (prelude-add-subfolders-to-load-path name)))))

;; add Prelude's directories to Emacs's `load-path'
(add-to-list 'load-path prelude-core-dir)
(add-to-list 'load-path prelude-modules-dir)
(add-to-list 'load-path prelude-vendor-dir)
(prelude-add-subfolders-to-load-path prelude-vendor-dir)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; preload the personal settings from `prelude-personal-preload-dir'
(when (file-exists-p prelude-personal-preload-dir)
  (message "[Prelude] Loading personal configuration files in %s..." prelude-personal-preload-dir)
  (mapc 'load (directory-files prelude-personal-preload-dir 't "^[^#\.].*el$")))

(message "[Prelude] Loading Prelude's core modules...")

;; load the core stuff
(require 'prelude-packages)
(require 'prelude-custom)  ;; Needs to be loaded before core, editor and ui
(require 'prelude-ui)
(require 'prelude-core)
(require 'prelude-mode)
(require 'prelude-editor)
(require 'prelude-global-keybindings)

;; macOS specific settings
(when (eq system-type 'darwin)
  (require 'prelude-macos))

;; Linux specific settings
(when (eq system-type 'gnu/linux)
  (require 'prelude-linux))

;; WSL specific setting
(when (and (eq system-type 'gnu/linux) (getenv "WSLENV"))
  (require 'prelude-wsl))

;; Windows specific settings
(when (eq system-type 'windows-nt)
  (require 'prelude-windows))

(message "[Prelude] Loading Prelude's additional modules...")

;; the modules
(if (file-exists-p prelude-modules-file)
    (load prelude-modules-file)
  (message "[Prelude] Missing personal modules file %s" prelude-modules-file)
  (message "[Prelude] Falling back to the bundled example file sample/prelude-modules.el")
  (message "[Prelude] You should copy this file to your personal configuration folder and tweak it to your liking")
  (load (expand-file-name "sample/prelude-modules.el" prelude-dir)))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" prelude-personal-dir))

;; load the personal settings (this includes `custom-file')
(when (file-exists-p prelude-personal-dir)
  (message "[Prelude] Loading personal configuration files in %s..." prelude-personal-dir)
  (mapc 'load (delete
               prelude-modules-file
               (directory-files prelude-personal-dir 't "^[^#\.].*\\.el$"))))

(message "[Prelude] Prelude is ready to do thy bidding, Master %s!" prelude-user)

;; Patch security vulnerability in Emacs versions older than 25.3
(when (version< emacs-version "25.3")
  (with-eval-after-load "enriched"
    (defun enriched-decode-display-prop (start end &optional param)
      (list start end))))

(prelude-eval-after-init
 ;; greet the use with some useful tip
 (run-at-time 5 nil 'prelude-tip-of-the-day))

;; ===============================================================
;; brew install emacs-plus@29 --with-native-comp --with-xwidgets --with-imagemagick --with-modern-black-variant-icon
;; brew install emacs-plus@29 --with-xwidgets --with-imagemagick --with-modern-black-variant-icon
;; My customizations

;; Initial Frame Size
(setq initial-frame-alist
      (append initial-frame-alist
              '((left   . 20)
                (top    . 0)
                (width  . 240)
                (height . 60))))

;; ============================
;; APPEARANCE
;; ============================

;; disable the feature where prelude disables syntax highlighting after certain line length.
(setq prelude-whitespace nil)

;; Truncate lines by default
(setq-default truncate-lines t)

(use-package ligature
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'org-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                      ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                      "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                      "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                      "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                      "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                      "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                      "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                      ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                      "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                      "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                      "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                      "\\\\" "://"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

(use-package fontaine
  :ensure t
  :config
  (setq fontaine-presets
        '((regular
           :default-family "Iosevka Comfy"
           :fixed-pitch-family "Iosevka Comfy"
           :fixed-pitch-height 1.0
           :variable-pitch-family "Iosevka Comfy Duo"
           :variable-pitch-height 1.0)
          (prose
           :default-family "Iosevka Comfy"
           :fixed-pitch-family "Iosevka Comfy"
           :fixed-pitch-height 1.0
           :variable-pitch-family "ETBembo"
           :variable-pitch-height 1.4)))
  (fontaine-mode 1)
  (fontaine-set-preset 'regular))

(use-package ef-themes
  ;; Make customisations that affect Emacs faces BEFORE loading a theme
  ;; (any change needs a theme re-load to take effect).
  :after fontaine
  :load-path "~/code/vendored-emacs-packages/ef-themes/"
  :config
  (setq ef-themes-headings
        '((0 . (variable-pitch light 1.5))
          (1 . (variable-pitch light 1.4))
          (2 . (variable-pitch regular 1.3))
          (3 . (variable-pitch regular 1.2))
          (4 . (variable-pitch regular 1.1))
          (5 . (variable-pitch 1.1)) ; absence of weight means `bold'
          (6 . (variable-pitch 1.1))
          (7 . (variable-pitch 1.1))
          (t . (variable-pitch 1.1))))
  ;; They are nil by default...
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  ;; Start up theme:
  (mapc #'disable-theme custom-enabled-themes) ; Disable all other themes to avoid awkward blending:
  ;; (load-theme 'ef-dream :no-confirm)
  (ef-themes-select 'ef-dream)
  ;; use this to load the theme which also calls `ef-themes-post-load-hook':
  ;; (ef-themes-select 'ef-winter)

  ;; The themes we provide are recorded in the `ef-themes-dark-themes' and `ef-themes-light-themes'.

  ;; We also provide these commands, but do not assign them to any key:
  ;;
  ;; - `ef-themes-toggle'
  ;; - `ef-themes-select'
  ;; - `ef-themes-select-dark'
  ;; - `ef-themes-select-light'
  ;; - `ef-themes-load-random'
  ;; - `ef-themes-preview-colors'
  ;; - `ef-themes-preview-colors-current'
  )

(use-package theme-switcher
  :load-path "~/code/my-emacs-packages/theme-switcher/"
  :after (org ef-themes)
  :init
  (setq *theme-switcher-themes-dark*
        '("ef-trio-dark"
          "ef-rosa"
          "ef-winter"
          "ef-autumn"
          "ef-cherie"
          "ef-tritanopia-dark"
          "ef-elea-dark"
          "ef-dream"
          "ef-melissa-dark"
          "ef-owl"))
  (setq *theme-switcher-themes-light*
        '("ef-day"
          "ef-light"
          "ef-kassio"
          "ef-frost"
          "ef-arbutus"
          "ef-melissa-light"
          "ef-maris-light"
          "ef-elea-light"
          "ef-summer"
          "ef-cyprus"
          "ef-reverie"))
  :bind
  ("C-t" . theme-switcher-choose-theme)
  (:map org-mode-map
        ("C-c C-x C-v" . ts-toggle-inline-images)))

(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode))

(use-package spaceline
  :ensure t
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme)
 
  (defun my-spaceline-theme ()
    "My custom Spaceline theme."
    (setq-default mode-line-format
                  '("%e"
                    mode-line-front-space
                    ;; evil-mode-line-tag
                    mode-line-mule-info
                    mode-line-client
                    mode-line-modified
                    mode-line-remote
                    mode-line-frame-identification
                    mode-line-buffer-identification
                    "   "                    
                    "   "
                    ;; mode-line-position
                    (vc-mode vc-mode) ;; Remove this line to exclude Git branch
                    "  "
                    ;; mode-line-modes                    
                    mode-line-misc-info
                    mode-line-end-spaces)))

  ;; Apply your custom theme
  (my-spaceline-theme)
  ;; display time in mode line
  (setq display-time-day-and-date t
        display-time-24hr-format nil)
  (display-time)

  (display-battery-mode 1))

(blink-cursor-mode)

;; =============================
;; ORG MODE
;; =============================
(defun my-org-syntax-table-modify ()
  "Modify `org-mode-syntax-table' for the current org buffer."
  (modify-syntax-entry ?< "." org-mode-syntax-table)
  (modify-syntax-entry ?> "." org-mode-syntax-table))

(use-package org
  :config
  (setq org-agenda-files (list (expand-file-name "~/Dropbox/agenda/agenda.org")))
  (setq org-archive-location "~/Dropbox/agenda/agenda_archive.org::%s_archive") ;; <-- unused? Org Archiver has it's own location.
  (setq org-plantuml-jar-path (expand-file-name "~/plantuml-1.2024.4.jar")) ;; <-- doesn't exist on my new mac
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (org-babel-do-load-languages ;; Org source block execution
   'org-babel-load-languages
   '((emacs-lisp . t)
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
  (add-to-list 'exec-path "/usr/local/mysql-8.3.0-macos14-x86_64/bin") ;; <-- doesn't exist on new mac
  (setq org-babel-python-command "python3")
  (setq org-log-note-clock-out t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.2))
  (setq org-image-actual-width nil)
  (setq org-list-allow-alphabetical t)
  (setq org-latex-listings 'minted ;; Export to LateX PDF using minted package
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  (setq org-export-backends '(ascii html icalendar latex md))
  (require 'ox-gfm nil t) ;; <-- For github flavored markdown export
  :hook ((org-mode . variable-pitch-mode)
         (org-mode . visual-line-mode)
         (org-mode . pixel-scroll-precision-mode)
         (org-mode . my-org-syntax-table-modify)
         (org-mode . (lambda () (display-line-numbers-mode 0)))))

(use-package mw-thesaurus
  :ensure t
  :after org
  :bind
  (:map org-mode-map
        ("C-c t" . mw-thesaurus-lookup-dwim)))

(use-package ox-gfm
  :ensure t
  :after org)

(use-package olivetti
  :load-path "~/code/vendored-emacs-packages/olivetti"
  :custom (olivetti-body-width 140)
  :hook (org-mode . olivetti-mode))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda ()
                             (org-bullets-mode 1)
                             (org-indent-mode 1))))

(use-package org-appear
  :after hyperbole
  :hook (org-mode . org-appear-mode)
  :custom
  (org-hide-emphasis-markers t) ; Hide /emphasis/ markers in org mode
  (org-appear-autolinks t) ; <-- This doesn't work when hyperbole package is loaded.
  (org-pretty-entities t)
  (org-pretty-entities-include-sub-superscripts nil) ; <-- This doesn't play nicely with under_scores
  (org-appear-autoentities t)
  (org-appear-autosubmarkers t))

(use-package org-cmenu
  :load-path "~/code/vendored-emacs-packages/org-cmenu/"
  :after org
  :config (require 'org-cmenu-setup)
  :bind
  (:map org-mode-map
        ("M-n" . org-cmenu)))

(use-package archiver
  :load-path "~/code/my-emacs-packages/archiver/"
  :after org
  :init
  (setq *archiver-agenda-archive-location*
        (expand-file-name "~/Dropbox/agenda/agenda_archive.org"))
  :bind
  (:map org-mode-map
        ("C-c C-x C-a" . archiver-archive-heading)))

;; automatically toggle latex previews in org mode.
(use-package org-fragtog
  :ensure t
  :hook (org-mode . org-fragtog-mode))

;; ===========================
;; Text Editing and Movement
;; ===========================

(setq next-line-add-newlines t) ;; c-n adds newlines

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; MAKE C-s search case-insensitive:
;; (setq case-fold-search t)

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/code/yasnippets"))
  (yas-global-mode 1))

;; ===============
;; C/C++ stuff
;; ===============
(defun read-from-minibuffer-with-validation (prompt valid-p)
  (require 'cl-lib)
  (let ((valid? nil)
        (input nil))
    (cl-loop until valid?
          do (progn
               (setq input (read-from-minibuffer prompt))
               (setq valid? (funcall valid-p input))))
    (message input)
    input))

(defvar *vterm-run-command* nil "Variable to store the vterm command.")
(defvar *switch-to-vterm?* nil "After C-c ; do you want to switch to vterm?")

(defun set-vterm-command ()
  (interactive)
  (require 'cl-lib)
  (setq *vterm-run-command*
        (read-from-minibuffer "Enter the vterm command: "
                              (if *vterm-run-command*
                                  *vterm-run-command*
                                "")))
  (setq *switch-to-vterm?*
        (cl-case (intern (read-from-minibuffer-with-validation "Switch to vterm after? (y/n): "
                                                               (lambda (input)
                                                                 (if (or (string-equal (downcase input) "y")
                                                                         (string-equal (downcase input) "n"))
                                                                     t
                                                                   nil))))
          (y t)
          (n nil))))

(defun invoke-vterm-command ()
  (interactive)
  (require 'vterm)
  (require 'cl-lib)
  (let ((buf (current-buffer)))
    (unless (get-buffer vterm-buffer-name)
      (vterm))
    (unless *vterm-run-command*

      (setq *vterm-run-command*
            (read-from-minibuffer "Enter the vterm command: "
                                  (if *vterm-run-command*
                                      *vterm-run-command*
                                    "")))
      (setq *switch-to-vterm?*
            (cl-case (intern (read-from-minibuffer-with-validation "Switch to vterm after? (y/n)"
                                                                   (lambda (input)
                                                                     (if (or (string-equal (downcase input) "y")
                                                                             (string-equal (downcase input) "n"))
                                                                         t
                                                                       nil))))
              (y t)
              (n nil))))
    (display-buffer vterm-buffer-name t)
    (switch-to-buffer-other-window vterm-buffer-name)
    (vterm--goto-line -1)
    (vterm-send-string *vterm-run-command*)
    (vterm-send-return)
    (when (not *switch-to-vterm?*) (switch-to-buffer-other-window buf))))

(add-hook 'octave-mode-hook
          (lambda ()
            (define-key octave-mode-map (kbd "C-c C-c") 'octave-send-defun)))

(add-hook 'c-mode-hook
          (lambda ()
            (define-key c-mode-map (kbd "C-c c") 'recompile)
            (define-key c-mode-map (kbd "C-c x") 'invoke-vterm-command)
            (define-key c-mode-map (kbd "C-c '") 'set-vterm-command)))

(add-hook 'c++-mode-hook
          (lambda ()
            (define-key c++-mode-map (kbd "C-c c") 'recompile)
            (define-key c++-mode-map (kbd "C-c x") 'invoke-vterm-command)
            (define-key c++-mode-map (kbd "C-c '") 'set-vterm-command)))

;; ==================
;; COMMON LISP
;; ==================

(add-to-list 'load-path "~/.emacs.d/personal/common-lisp.el")
(add-to-list 'load-path "~/.emacs.d/personal/lass.el")

;; ==================
;; COMMON LISP: SLIME
;; ==================
(setq inferior-lisp-program (executable-find "sbcl"))
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; replace "sbcl" with the path to your implementation
;; (setq inferior-lisp-program "sbcl --dynamics-space-size 2048")
(setq slime-lisp-implementations '((sbcl ("sbcl" "--dynamic-space-size" "4000"))
                                   (ccl64 ("/usr/local/bin/ccl64"))))
(setq slime-default-lisp 'sbcl)


;; Emacs Easy Draw
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/personal/el-easydraw/"))
;; (autoload 'edraw-mode "edraw-mode")
;; (add-to-list 'auto-mode-alist '("\\.edraw\\.svg$" . edraw-mode))
;; (autoload 'edraw "edraw-mode" nil t)

;; (with-eval-after-load 'org
;;   (require 'edraw-org)
;;   (edraw-org-setup-default))
;; When using the org-export-in-background option (when using the
;; asynchronous export function), the following settings are
;; required. This is because Emacs started in a separate process does
;; not load org.el but only ox.el.
;; (with-eval-after-load "ox"
;;   (require 'edraw-org)
;;   (edraw-org-setup-exporter))

;; =====================
;; MISCELLANEOUS
;; =====================

(add-to-list 'load-path (expand-file-name "~/.emacs.d/personal/update-table/"))
(load (expand-file-name "~/.emacs.d/personal/update-table/update-table.el"))

(use-package safe
  :load-path "~/.safe/")

;; ;; ORG-AI
;; (use-package org-ai
;;   :after safe
;;   :ensure t
;;   :commands (org-ai-mode org-ai-global-mode)
;;   :init
;;   (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
;;   (org-ai-global-mode) ; installs global keybindings on C-c M-a
;;   :config
;;   (setq org-ai-openai-api-token *api-token*)
;;   (setq org-ai-default-chat-model "gpt-4o")  
;;   )

;; Spell Check Dictionary
(setq ispell-personal-dictionary "~/.emacs.d/personal/my-personal-ispell-dictionary")

;; Elixir
(use-package inf-elixir
  :bind (("C-c c i" . 'inf-elixir)
         ("C-c c p" . 'inf-elixir-project)
         ("C-c c l" . 'inf-elixir-send-line)
         ("C-c c r" . 'inf-elixir-send-region)
         ("C-c c b" . 'inf-elixir-send-buffer)
         ("C-c c R" . 'inf-elixir-reload-module))
  :ensure t)

;;;; REPL management
;; (defun elixir-inf-switch ()
;;   "switch to inf elixir window"
;;   (interactive)
;;   (let ((bufs (mapcar #'buffer-name (buffer-list))))
;;     (elixir-inf-helper bufs)))

;; (defun elixir-inf-helper (lis)
;;   "find terminal and wtich to term buffer"
;;   (cond
;;    ((eq '() lis)
;;     (inf-elixir-set-repl))
;;    ((string= (car lis) "Inf-Elixir")
;;     (switch-to-buffer-other-window (car lis)))
;;    (t
;;     (elixir-inf-helper (cdr lis)))))

;; (define-key inf-elixir-mode-map (kbd "C-c C-z") 'previous-multiframe-window)
;; (define-key elixir-mode-map (kbd "C-<return>") 'inf-elixir-send-line)
;; (define-key elixir-mode-map (kbd "C-c C-c") 'inf-elixir-send-buffer)
;; (define-key elixir-mode-map (kbd "C-c C-z") 'elixir-inf-switch)

;; Calendar customization
(defun calendar-insert-date ()
  "Capture the date at point, exit the Calendar, insert the date."
  (interactive)
  (seq-let (month day year) (save-match-data (calendar-cursor-to-date))
    (calendar-exit)
    (insert (format "<%d-%02d-%02d>" year month day))))

(define-key calendar-mode-map (kbd "RET") 'calendar-insert-date)

(use-package rotor
  :after safe
  :load-path "~/code/my-emacs-packages/rotor/")

;; ;; Scratch buffer customization
;; (setq initial-major-mode 'org-mode)
;; (setq initial-scratch-message "")

;; Open Agenda on startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (find-file "~/Dropbox/agenda/agenda.org") ;; <-- org file
            (org-agenda-list)))                       ;; <-- calendar

;; Cassandra database
(use-package cql-mode
  :ensure t
  :mode ("\\.cql\\'" . cql-mode)
  :config
  (setq cql-indent-level 2)) ;; Optionally set custom indentation

(use-package sql-indent
  :ensure t
  :hook ((sql-mode . sqlind-minor-mode)
         (cql-mode . sqlind-minor-mode))
  :config
  ;; You can further customize indentation or align rules here if needed
  )

(use-package dired-preview
  :ensure t)

;; Useful for debugging elisp:
;; Example: (pair-tree '(1 2 3))
(use-package pair-tree
  :ensure t)

(add-hook 'emacs-lisp-mode 'prettify-symbols-mode)

;; Blog publishing
(add-to-list 'load-path (expand-file-name "~/.emacs.d/personal/blog-publishing.el"))

(use-package hyperbole ;; This conflicts with org appear a bit, for links.
  :ensure t
  :config
  (hyperbole-mode 0))  ;; 1 -> Enable Hyperbole mode after installation, 0 -> don't

(use-package command-log-mode
  :ensure t)

(defun entire-buffer-replace (from to)
  "Do search and replace on entire buffer without moving point.
Display the number of replacements made."
  (interactive "MReplace: \nMWith: ")
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search nil)
          (count 0))
      (while (search-forward from nil t)
        (replace-match to t t)
        (setq count (1+ count)))
      (message "Replaced %d occurrences of '%s'." count from))))

;; Ebook reader
(use-package nov
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(use-package ready-player
  :ensure t
  :config
  (ready-player-mode +1))

(use-package annotate
  :ensure t
  :config
  (setq annotate-file "~/.emacs.d/annotations"))

;; function to make insert greek letters after typing their name.
;; also includes other symbols besides greek letters like infinity and setmemership (in).
;; bound to c-x c-g below.
(defun to-greek ()
  "convert word at point to greek letter."
  (interactive)
  (let ((word (thing-at-point 'word))
	(bounds (bounds-of-thing-at-point 'word)))
    (let ((pos1 (car bounds))
	  (pos2 (cdr bounds)))
      ;; (message
      ;;  "word begins at [%s], end at [%s], word is [%s]"
      ;;  pos1 pos2 word)
      (delete-region pos1 pos2)
      (insert
       (pcase word
	 ("lambda" "λ")
	 ("omega" "ω")
	 ("pi" "π")
	 ("rho" "ρ")
	 ("beta" "β")
	 ("alpha" "α")
	 ("gamma" "γ")
	 ("delta" "δ")
	 ("theta" "θ")
	 ("phi" "φ")
	 ("tau" "τ")
	 ("sigma" "σ")
	 ("infinity" "∞")
	 ("in" "∈")
         ("mu" "µ")
	 ("check" "✓")
	 (_ word))))))

(global-set-key (kbd "C-x C-g") 'to-greek)

;;; init.el ends here
