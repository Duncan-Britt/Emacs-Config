;;; ob-mongo.el --- Execute mongodb queries within org-mode blocks.
;; Copyright 2013 Kris Jenkins

;; License: GNU General Public License version 3, or (at your option) any later version
;; Author: Kris Jenkins <krisajenkins@gmail.com>
;; Maintainer: Kris Jenkins <krisajenkins@gmail.com>
;; Keywords: org babel mongo mongodb
;; URL: https://github.com/krisajenkins/ob-mongo
;; Created: 17th July 2013
;; Version: 0.1.0
;; Package-Requires: ((org "8"))

;;; Commentary:
;;
;; Execute mongodb queries within org-mode blocks.

;;; Code:
(require 'org)
(require 'ob)

(defgroup ob-mongo nil
  "org-mode blocks for MongoDB."
  :group 'org)

(defcustom ob-mongo:default-db nil
  "Default mongo database."
  :group 'ob-mongo
  :type 'string)

(defcustom ob-mongo:default-host nil
  "Default mongo host."
  :group 'ob-mongo
  :type 'string)

(defcustom ob-mongo:default-port nil
  "Default mongo port."
  :group 'ob-mongo
  :type 'integer)

(defcustom ob-mongo:default-user nil
  "Default mongo user."
  :group 'ob-mongo
  :type 'string)

(defcustom ob-mongo:default-password nil
  "Default mongo password."
  :group 'ob-mongo
  :type 'string)

(defcustom ob-mongo:default-mongo-executable "mongosh"
  "Default mongo executable."
  :group 'ob-mongo
  :type 'string)

;; (defun ob-mongo--make-command (params)
;;   (let* ((is-uri (string-prefix-p "mongodb" (or (cdr (assoc :host params)) "")))
;;          (pdefs `((:mongoexec ,ob-mongo:default-mongo-executable)
;;                      (quiet "--quiet")
;;                      ;; Only add host, port, user, password if not a URI
;;                      (:host ,(unless is-uri ob-mongo:default-host) "--host")
;;                      (:port ,(unless is-uri ob-mongo:default-port) "--port")
;;                      (:password ,(unless is-uri ob-mongo:default-password) "--password")
;;                      (:user ,(unless is-uri ob-mongo:default-user) "--username")
;;                      (:db ,ob-mongo:default-db)
;;                      ;; If it's a URI, include it directly without a flag
;;                      (uri ,(when is-uri "")))))
;;     (mapconcat (lambda (pdef)
;;                  (let* ((opt (or (nth 2 pdef) ""))
;;                        (key (car pdef))
;;                        (val (cdr (assoc key params))))
;;                    ;; Special handling for URI to include it directly
;;                    (if (eq key 'uri)
;;                        val
;;                      (cond ((not opt) (format "%s" val))
;;                            (val (format "%s %s" opt val))
;;                            (t "")))))
;;                pdefs " ")))

;; (defun ob-mongo--make-command (params)
;;   (let* ((is-uri (string-prefix-p "mongodb+srv://" (or (cdr (assoc :host params)) "")))
;;          ;; Define the base command with the executable and the quiet flag.
;;          (base-command (list (cons :mongoexec ob-mongo:default-mongo-executable) (cons 'quiet "--quiet")))
;;          ;; Define parameters, conditionally including them based on whether it's a URI.
;;          (param-defs (unless is-uri
;;                        `((:host ,ob-mongo:default-host "--host")
;;                          (:port ,ob-mongo:default-port "--port")
;;                          (:password ,ob-mongo:default-password "--password")
;;                          (:user ,ob-mongo:default-user "--username")
;;                          (:db ,ob-mongo:default-db))))
;;          ;; Combine the base command and parameter definitions.
;;          (pdefs (if is-uri
;;                     `((:mongoexec ,ob-mongo:default-mongo-executable)
;;                       (quiet "--quiet")
;;                       (uri , (cdr (assoc :host params))))
;;                   (append base-command param-defs))))
;;     ;; Construct the command string.
;;     (mapconcat (lambda (pdef)
;;                  (let* ((key (car pdef))
;;                         (val (cdr (assoc key params (list pdef)))))
;;                    (if (eq key 'uri)
;;                        val
;;                      (let ((opt (cdr pdef)))
;;                        (when val
;;                          (if (eq key :mongoexec)
;;                              val
;;                            (format "%s %s" opt val)))))))
;;                pdefs " ")))


(defun ob-mongo--make-command (params)
  (let ((pdefs `((:mongoexec ,ob-mongo:default-mongo-executable)
                 (quiet "--quiet")
                 (:uri nil)
                 (:host , ob-mongo:default-host "--host")
                 (:port ,ob-mongo:default-port "--port")
                 (:password ,ob-mongo:default-password "--password")
                 (:user ,ob-mongo:default-user "--username")
                 (:db ,ob-mongo:default-db))))
    (mapconcat (lambda (pdef)
                 (if (eq (car pdef) :uri)
                     (enquote (cdr (assoc (car pdef) params)))
                     (let ((opt (or (nth 2 pdef) ""))
                           (val (or (cdr (assoc (car pdef) params))
                                    (nth 1 pdef))))                   
                       (cond ((not opt) (format "%s" val))
                             (val (format "%s %s" opt val))
                             (t "")))))
               pdefs " ")))

(defun enquote (str)
  "Wrap STR in double quotes."
  (concat "\"" str "\""))

;; (nth 2 '(:uri nil))
;; (cdr (assoc (car '(:uri nil))
;;             '((:colname-names)
;;               (:rowname-names)
;;               (:result-params "replace")
;;               (:result-type . value)
;;               (:results . "replace")
;;               (:exports . "code")
;;               (:session . "none")
;;               (:cache . "no")
;;               (:noweb . "no")
;;               (:hlines . "no")
;;               (:tangle . "no")
;;               (:db . "new_mongo_db")
;;               (:uri . "mongodb+srv://cluster0.iiy9g92.mongodb.net/")
;;               (:user . "dbru997")
;;               (:password . "tOurV4VkHOiG8U6j"))))

;; (ob-mongo--make-command '((:colname-names)
;;                           (:rowname-names)
;;                           (:result-params "replace")
;;                           (:result-type . value)
;;                           (:results . "replace")
;;                           (:exports . "code")
;;                           (:session . "none")
;;                           (:cache . "no")
;;                           (:noweb . "no")
;;                           (:hlines . "no")
;;                           (:tangle . "no")
;;                           (:db . "new_mongo_db")
;;                           (:uri . "mongodb+srv://cluster0.iiy9g92.mongodb.net/")
;;                           (:user . "dbru997")
;;                           (:password . "tOurV4VkHOiG8U6j")))


;; mongosh "mongodb+srv://cluster0.iiy9g92.mongodb.net/" --apiVersion 1 --username dbru997 --password tOurV4VkHOiG8U6j
;;;###autoload
(defun org-babel-execute:mongo (body params)
  "org-babel mongo hook." 
  ;; (unless (assoc :db params)
  ;;   (user-error "The required parameter :db is missing."))
  ;; (org-babel-eval (ob-mongo--make-command params) body)
  (org-babel-eval "mongosh \"mongodb+srv://dbru997:tOurV4VkHOiG8U6j@cluster0.iiy9g92.mongodb.net/new_mongo_db\"" body))

;;;###autoload
(eval-after-load "org"
  '(add-to-list 'org-src-lang-modes '("mongo" . js)))

(provide 'ob-mongo)

;;; ob-mongo.el ends here




