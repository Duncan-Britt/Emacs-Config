;; mode line settings

;; (use-package spaceline
;;   :ensure t
;;   :config
;;   (require 'spaceline-config)
;;   (spaceline-emacs-theme)

;;   ;; Remove Projectile segment
;;   (spaceline-toggle-projectile-off))

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
  (my-spaceline-theme))


;; display time in mode line
(setq display-time-day-and-date t
      display-time-24hr-format nil)
(display-time)

(display-battery-mode 1)
