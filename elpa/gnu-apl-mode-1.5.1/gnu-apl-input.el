;;; gnu-apl-input.el --- Input method for GNU APL -*- lexical-binding: t -*-

;; Copyright (C) 2013-2015 Elias Mårtenson

;;; Code:

(require 'cl-lib)
(require 'quail)
(require 'gnu-apl-symbols)

(quail-define-package "APL-Z" "UTF-8" "⍞" t
                      "Input mode for APL"
                      '(("\t" . quail-completion))
                      t                 ; forget-last-selection
                      nil               ; deterministic
                      nil               ; kbd-translate
                      t                 ; show-layout
                      nil               ; create-decode-map
                      nil               ; maximum-shortest
                      nil               ; overlay-plist
                      nil               ; update-translation-function
                      nil               ; conversion-keys
                      t                 ; simple
                      )

(defvar gnu-apl--transcription-alist)
(defun gnu-apl--update-key-prefix (symbol new)
  (quail-select-package "APL-Z")
  (quail-install-map
   (let* ((prefix (string new))
          (gnu-apl--transcription-alist
           (cl-loop for command in gnu-apl--symbols
                 for key-command = (cl-third command)
                 append (cl-loop for s in (if (listp key-command)
                                           key-command
                                         (list key-command))
                              collect (cons (concat prefix s)
                                            (cl-second command))))))
     (quail-map-from-table
      '((default gnu-apl--transcription-alist)))))
  (set-default symbol new))

(defun gnu-apl--initialize-key-prefix (symbol new)
  (custom-initialize-default symbol new)
  (gnu-apl--update-key-prefix symbol (eval new)))

(defcustom gnu-apl-key-prefix ?.
  "Set a character to serve as prefix key for APL symbol input."
  :type 'character
  :group 'gnu-apl
  :initialize #'gnu-apl--initialize-key-prefix
  :set #'gnu-apl--update-key-prefix)

(provide 'gnu-apl-input)
;;; gnu-apl-input.el ends here
