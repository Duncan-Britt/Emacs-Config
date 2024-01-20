;;; janet-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "janet-mode" "janet-mode.el" (0 0 0 0))
;;; Generated autoloads from janet-mode.el

(autoload 'janet-mode "janet-mode" "\
Major mode for the Janet language

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.janet\\'" . janet-mode))

(add-to-list 'interpreter-mode-alist '("janet" . janet-mode))

(register-definition-prefixes "janet-mode" '("janet-"))

;;;***

;;;### (autoloads nil nil ("janet-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; janet-mode-autoloads.el ends here
