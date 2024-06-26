(defcustom lass-generate-pretty-p nil
  "Whether to generate pretty .lass files."
  :type 'boolean)

(defun lass-compile-current ()
  (interactive)
  (or
   (when (and (slime-connected-p)
              (or (slime-eval '(cl:not (cl:null (cl:find-package :lass))))
                  (and (slime-eval '(cl:not (cl:null (cl:find-package :ql))))
                       (slime-eval '(ql:quickload :lass)))))
     (message "LASS compiled to %s"
              (slime-eval `(uiop:native-namestring
                            (lass:generate
                             (uiop:parse-native-namestring ,(buffer-file-name))
                             :pretty ,lass-generate-pretty-p)))))
   (message "LASS compiled. %s" (shell-command-to-string (format "lass %s" (shell-quote-argument (buffer-file-name)))))))

(define-derived-mode lass-mode common-lisp-mode
  "LASS" "Mode with auto-compiling for LASS files."
  (add-hook 'after-save-hook 'lass-compile-current nil t))

(add-to-list 'auto-mode-alist '("\\.lass\\'" . lass-mode))

(provide 'lass)
