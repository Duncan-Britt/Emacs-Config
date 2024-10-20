;; This could be useful to add custom keywords for syntax highlighting, and I could use .dir-locals.el to make it directory specific, I think.
;; (add-hook 'lisp-mode-hook
;;           (lambda ()
;;             (font-lock-add-keywords nil
;;                                     '(
;;                                       ("\\<\\(val\\|λ\\)\\>" . font-lock-keyword-face)
;;                                       ("\\<\\(is\\|in\\)\\>" . font-lock-function-name-face)))))
(add-hook 'lisp-mode-hook 'prettify-symbols-mode)

;; ==================
;; COMMON LISP: SLIME
;; ==================
(setq inferior-lisp-program (executable-find "sbcl")) ;; Can replace "sbcl" with the path to your implementation instead of using executable find.
(setq slime-lisp-implementations '((sbcl ((executable-find "sbcl") "--dynamic-space-size" "4000"))
                                   ;; (ccl64 ("/usr/local/bin/ccl64"))
                                   ))

;; For hacking on Nyxt:
;; (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")
;; (setq slime-lisp-implementations '((nyxt ("/opt/homebrew/bin/sbcl" "--dynamic-space-size 3072")
;;                                          :env ("CL_SOURCE_REGISTRY=/Users/duncan/quicklisp/dists/quicklisp/software//:~/code/nyxt//:~/code/nyxt/_build//"))))
(load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq slime-default-lisp 'sbcl)
