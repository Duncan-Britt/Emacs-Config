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

;; Set font
(set-face-attribute 'default nil :font "Iosevka")
;; font size
(set-face-attribute 'default nil :height 120)

;; Initial Frame Size
(setq initial-frame-alist
      (append initial-frame-alist
              '((left   . 20)
                (top    . 0)
                (width  . 240)
                (height . 60))))

;; enable ligatures in every possible major mode modes
;; (change 't to 'prog-mode to only enable in programming modes.)
(ligature-set-ligatures 't '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                             ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                             "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                             "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                             "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                             "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                             "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                             "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                             "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                             "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

(global-ligature-mode 't)

;; Disable italics
;; (set-face-italic 'font-lock-comment-face nil)

;; ef-themes
(add-to-list 'load-path "~/.emacs.d/manual-packages/ef-themes")

;; Make customisations that affect Emacs faces BEFORE loading a theme
;; (any change needs a theme re-load to take effect).
(require 'ef-themes)

;; If you like two specific themes and want to switch between them, you
;; can specify them in `ef-themes-to-toggle' and then invoke the command
;; `ef-themes-toggle'.  All the themes are included in the variable
;; `ef-themes-collection'.
;; (setq ef-themes-to-toggle '(ef-summer ef-winter))

(setq ef-themes-headings
      '((0 . (variable-pitch light 1.9))
        (1 . (variable-pitch light 1.8))
        (2 . (variable-pitch regular 1.7))
        (3 . (variable-pitch regular 1.6))
        (4 . (variable-pitch regular 1.5))
        (5 . (variable-pitch 1.4)) ; absence of weight means `bold'
        (6 . (variable-pitch 1.3))
        (7 . (variable-pitch 1.2))
        (t . (variable-pitch 1.1))))

;; (setq ef-themes-headings ; read the manual's entry or the doc string
;;       '((0 variable-pitch light 1.9)
;;         (1 variable-pitch light 1.8)
;;         (2 variable-pitch regular 1.7)
;;         (3 variable-pitch regular 1.6)
;;         (4 variable-pitch regular 1.5)
;;         (5 variable-pitch 1.4) ; absence of weight means `bold'
;;         (6 variable-pitch 1.3)
;;         (7 variable-pitch 1.2)
;;         (t variable-pitch 1.1)))

;; They are nil by default...
(setq ef-themes-mixed-fonts nil
      ef-themes-variable-pitch-ui nil)

;; Read the doc string or manual for this one.  The symbols can be
;; combined in any order.
;; (setq ef-themes-region '(intense no-extend neutral))

;; Disable all other themes to avoid awkward blending:
(mapc #'disable-theme custom-enabled-themes)

;; Load the theme of choice:
(load-theme 'ef-winter :no-confirm)

;; OR use this to load the theme which also calls `ef-themes-post-load-hook':
;; (ef-themes-select 'ef-winter)

;; The themes we provide are recorded in the `ef-themes-dark-themes',
;; `ef-themes-light-themes'.

;; We also provide these commands, but do not assign them to any key:
;;
;; - `ef-themes-toggle'
;; - `ef-themes-select'
;; - `ef-themes-select-dark'
;; - `ef-themes-select-light'
;; - `ef-themes-load-random'
;; - `ef-themes-preview-colors'
;; - `ef-themes-preview-colors-current'

;; Choose theme among themes I like
(defun lookup (key assoc-list)
  (cdr (assoc key assoc-list)))

(defun choose-theme ()
  "Open a selection menu in the minibuffer."
  (interactive)
  (let* ((all-themes '(("Light" . ("ef-day" "ef-light" "ef-kassio" "ef-frost" "ef-arbutus" "ef-melissa-light" "ef-maris-light" "ef-elea-light" "ef-summer"))
                       ("Dark" . ("ef-trio-dark" "ef-dark" "ef-duo-dark" "ef-rosa" "ef-symbiosis" "ef-winter" "ef-cherie" "ef-tritanopia-dark"))))
         (options '("Light" "Dark"))
         (brightness-selection (completing-read "Choose category: " options))
         (themes (lookup brightness-selection all-themes))
         (theme-chosen (completing-read "Choose a theme:" themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (intern theme-chosen) t)
    (message "Loaded %s" theme-chosen)))

(global-set-key (kbd "C-t") 'choose-theme)

;; C++ stuff
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
(global-set-key (kbd "C-c '") 'set-vterm-command)

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

(global-set-key (kbd "C-c x") 'invoke-vterm-command)

(add-hook 'octave-mode-hook
          (lambda ()
            (define-key octave-mode-map (kbd "C-c C-c") 'octave-send-defun)))

(add-hook 'c-mode-hook
          (lambda ()
            (define-key c-mode-map (kbd "C-c c") 'recompile)))

(add-hook 'c++-mode-hook
          (lambda ()
            (define-key c++-mode-map (kbd "C-c c") 'recompile)
            (let ((vterm-run-command nil)
                  (switch-to-vterm? nil))
              (define-key c++-mode-map (kbd "C-c '")
                          (lambda ()
                            (interactive)
                            (require 'cl-lib)
                            (setq vterm-run-command
                                  (read-from-minibuffer "Enter the vterm command: "
                                                        (if vterm-run-command
                                                            vterm-run-command
                                                          "")))
                            (setq switch-to-vterm?
                                  (cl-case (intern (read-from-minibuffer-with-validation "Switch to vterm after? (y/n): "
                                                                                         (lambda (input)
                                                                                           (if (or (string-equal (downcase input) "y")
                                                                                                   (string-equal (downcase input) "n"))
                                                                                               t
                                                                                             nil))))
                                         (y t)
                                         (n nil)))))
              (define-key c++-mode-map (kbd "C-c ;")
                          (lambda ()
                            (interactive)
                            (require 'vterm)
                            (require 'cl-lib)
                            (let ((buf (current-buffer)))
                              (unless (get-buffer vterm-buffer-name)
                                (vterm))
                              (unless vterm-run-command

                                (setq vterm-run-command
                                      (read-from-minibuffer "Enter the vterm command: "
                                                            (if vterm-run-command
                                                                vterm-run-command
                                                              "")))
                                (setq switch-to-vterm?
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
                              (vterm-send-string vterm-run-command)
                              (vterm-send-return)
                              (when (not switch-to-vterm?) (switch-to-buffer-other-window buf))))))))

;; disable the feature where prelude disables syntax highlighting after certain line length.
(setq prelude-whitespace nil)

;; Truncate lines by default
(setq-default truncate-lines t)

;; c-n adds newlines
(setq next-line-add-newlines t)

;; Org AI
;; I can't use this because I can't use gpg.
;; I can't use gpg because the new version is incompatible with emacs 29.1
;; There's a fix, but it requires me to update xcode and I can't because I'm still on mac monterrey
;; (use-package org-ai
;;   :ensure
;;   :commands (org-ai-mode org-ai-global-mode)
;;   :init
;;   (add-hook 'org-mode-hook #'org-ai-mode)
;;   (org-ai-global-mode))

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

;; Merriam Webster Thesaurus keybinding
(eval-after-load 'org
  '(define-key org-mode-map (kbd "C-c t") 'mw-thesaurus-lookup-dwim))


;; (defun my-org-custom-keybindings ()
;;   "My custom keybindings for Org mode."
;;   (local-set-key (kbd "C-c ") 'org-insert-my-custom-element)
;;   ;; Add more keybindings as needed
;;   )

;; (add-hook 'org-mode-hook 'my-org-custom-keybindings)

;; setup yasnippet
(add-to-list 'load-path "~/.emacs.d/elpa/yasnippet-20200604.246")
(require 'yasnippet)
(setq yas-snippet-dirs '("~/Dropbox/yasnippets"))
(yas-global-mode 1)

;; (use-package annotate
;;   :ensure t
;;   ;; :config
;;   ;; Here you can place any configuration code for annotate.el
;;   ;; For example, to set a custom annotation file, you could use:
;;   ;; (setq annotate-file "~/.emacs.d/annotations")
;;   )


;; Common Lisp
(add-to-list 'load-path "~/.emacs.d/personal/common-lisp.el")
(add-to-list 'load-path "~/.emacs.d/personal/lass.el")

;; SLIME
;; Common Lisp
(setq inferior-lisp-program (executable-find "sbcl"))
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; replace "sbcl" with the path to your implementation
;; (setq inferior-lisp-program "sbcl --dynamics-space-size 2048")
(setq slime-lisp-implementations '((sbcl ("sbcl" "--dynamic-space-size" "4000"))
                                   (ccl64 ("/usr/local/bin/ccl64"))))
(setq slime-default-lisp 'sbcl)


;; Org Mode Customizations
(add-to-list 'load-path (expand-file-name "~/.emacs.d/personal/org-customizations.el"))
(autoload 'org-customizations "org-customizations")
;;;; Olvetti Mode
(add-to-list 'load-path "~/.emacs.d/personal/olivetti-2.0.5")

;; Emacs Easy Draw
(add-to-list 'load-path (expand-file-name "~/.emacs.d/personal/el-easydraw/"))
(autoload 'edraw-mode "edraw-mode")
(add-to-list 'auto-mode-alist '("\\.edraw\\.svg$" . edraw-mode))
(autoload 'edraw "edraw-mode" nil t)

(with-eval-after-load 'org
  (require 'edraw-org)
  (edraw-org-setup-default))
;; When using the org-export-in-background option (when using the
;; asynchronous export function), the following settings are
;; required. This is because Emacs started in a separate process does
;; not load org.el but only ox.el.
(with-eval-after-load "ox"
  (require 'edraw-org)
  (edraw-org-setup-exporter))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/personal/update-table/"))
(load (expand-file-name "~/.emacs.d/personal/update-table/update-table.el"))

;; MAKE C-s search case-insensitive:
;; (setq case-fold-search t)

;; ORG-AI
(use-package org-ai
  :ensure
  :commands (org-ai-mode org-ai-global-mode)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode)
  (org-ai-global-mode))


(use-package breadcrumb
  :ensure t)

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

;;; init.el ends here



