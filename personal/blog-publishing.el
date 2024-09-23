(require 'ox-publish)

(setq org-html-validation-link nil                    ;; Don't show validation link
      org-html-head-include-spcript nil               ;; Use our own scripts
      org-html-head-include-default-style nil)        ;; Use our own styles      
;;      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />")

(setq org-html-htmlize-output-type 'css)

(defun read-file (filename)
  (save-excursion
    (let ((new (get-buffer-create filename)) (current (current-buffer)))
      (switch-to-buffer new)
      (insert-file-contents filename)
      (mark-whole-buffer)
      (let ((contents (buffer-substring (mark) (point))))
        (kill-buffer new)
        (switch-to-buffer current)
        contents))))

(require 'org)
(require 'org-element)

;; Org html backend customization
(defun format-org-date (date-string)
  "Convert DATE-STRING from '2024-09-02 Mon 00:00' to 'Monday, September 2, 2024'."
  (let* ((parsed-date (parse-time-string date-string))
         (day (nth 3 parsed-date))
         (month (nth 4 parsed-date))
         (year (nth 5 parsed-date))
         (weekday (format-time-string "%A" (encode-time parsed-date)))
         (month-name (format-time-string "%B" (encode-time parsed-date))))
    (format "%s, %s %d, %d" weekday month-name day year)))

(defun org-html-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options.

Overrides org-html-template defined in Emacs core."
  (concat
   (when (and (not (org-html-html5-p info)) (org-html-xhtml-p info))
     (let* ((xml-declaration (plist-get info :html-xml-declaration))
	    (decl (or (and (stringp xml-declaration) xml-declaration)
		      (cdr (assoc (plist-get info :html-extension)
				  xml-declaration))
		      (cdr (assoc "html" xml-declaration))
		      "")))
       (when (not (or (not decl) (string= "" decl)))
	 (format "%s\n"
		 (format decl
			 (or (and org-html-coding-system
                                  ;; FIXME: Use Emacs 22 style here, see `coding-system-get'.
				  (coding-system-get org-html-coding-system 'mime-charset))
			     "iso-8859-1"))))))
   (org-html-doctype info)
   "\n"
   (concat "<html"
	   (cond ((org-html-xhtml-p info)
		  (format
		   " xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"%s\" xml:lang=\"%s\""
		   (plist-get info :language) (plist-get info :language)))
		 ((org-html-html5-p info)
		  (format " lang=\"%s\"" (plist-get info :language))))
	   ">\n")
   "<head>\n"
   (org-html--build-meta-info info)
   (org-html--build-head info)
   (org-html--build-mathjax-config info)
   "</head>\n"
   "<body>\n"
   (let ((link-up (org-trim (plist-get info :html-link-up)))
	 (link-home (org-trim (plist-get info :html-link-home))))
     (unless (and (string= link-up "") (string= link-home ""))
       (format (plist-get info :html-home/up-format)
	       (or link-up link-home)
	       (or link-home link-up))))
   ;; Preamble.
   (org-html--build-pre/postamble 'preamble info)
   ;; Document contents.
   (let ((div (assq 'content (plist-get info :html-divs))))
     (format "<%s id=\"%s\" class=\"%s\">\n"
             (nth 1 div)
             (nth 2 div)
             (plist-get info :html-content-class)))
   ;; Document title.
   (when (plist-get info :with-title)
     (let ((title (and (plist-get info :with-title)
		       (plist-get info :title)))
	   (subtitle (plist-get info :subtitle))
	   (html5-fancy (org-html--html5-fancy-p info)))
       (when title
	 (format
	  (if html5-fancy
	      "<header>\n<h1 class=\"title\">%s</h1>\n%s</header>"
	    "<h1 class=\"title\">%s%s</h1>\n")
	  (org-export-data title info)
	  (if subtitle
	      (format
	       (if html5-fancy
		   "<p class=\"subtitle\" role=\"doc-subtitle\">%s</p>\n"
		 (concat "\n" (org-html-close-tag "br" nil info) "\n"
			 "<span class=\"subtitle\">%s</span>\n"))
	       (org-export-data subtitle info))
	    "")))))
   ;; BEGIN EDIT ===============
   (let* ((spec (org-html-format-spec info))
         (date (cdr (assq ?d spec))))
     (and (plist-get info :with-date) ;; FIXME If there's no date and this is nil, I think it's fine. But I haven't checked.
	  (org-string-nw-p date)
	  (format "<p class=\"date\">%s</p>\n" (format-org-date date))))
   ;; END EDIT   ===============
   contents
   (format "</%s>\n" (nth 1 (assq 'content (plist-get info :html-divs))))
   ;; Postamble.
   (org-html--build-pre/postamble 'postamble info)
   ;; Possibly use the Klipse library live code blocks.
   (when (plist-get info :html-klipsify-src)
     (concat "<script>" (plist-get info :html-klipse-selection-script)
	     "</script><script src=\""
	     org-html-klipse-js
	     "\"></script><link rel=\"stylesheet\" type=\"text/css\" href=\""
	     org-html-klipse-css "\"/>"))
   ;; Closing document.
   "</body>\n</html>"))
;; end Org html backend customization

(defun my/org-get-filetags (e)
  (let ((type (org-element-type e))
        (key (org-element-property :key e))
        (value (org-element-property :value e)))
    (if (and (eq type 'keyword) (equal key "FILETAGS"))
        (split-string value ":"))))

;; (defun org-publish-find-file-tags (file)
;;   "Return a string of comma separated file tags for the file at FILE"

;;   (let* ((visiting (find-buffer-visiting file))
;;          (buffer (or visiting (find-file-noselect file)))
;;          title)
;;     (with-current-buffer buffer
;;       (message "{")
;;       (message "%s" (org-element-map (org-element-parse-buffer) '(keyword) #'my/org-get-filetags))
;;       (message "}")
;;       (string-join (flatten-tree (org-element-map (org-element-parse-buffer) '(keyword) #'my/org-get-filetags)) ","))))

(defun org-publish-find-file-tags (file)
  "Return a string of comma separated file tags for the file at FILE"

  (let* ((visiting (find-buffer-visiting file))
         (buffer (or visiting (find-file-noselect file)))
         title)
    (with-current-buffer buffer
      (string-join
       (flatten-tree
        (let ((ret))
          (goto-char (point-min))
          (while (search-forward "#+FILETAGS:" nil t)
            (save-excursion
              (beginning-of-line)
              (setq ret (cons (my/org-get-filetags (org-element-at-point)) ret))))
          (nreverse (delq nil ret)))) ","))))

;; http://xahlee.info/emacs/emacs/elisp_parse_org_mode.html
;; http://xahlee.info/emacs/emacs/elisp_property_list.html
;; https://stackoverflow.com/questions/70359184/get-list-of-all-h1-entry-of-org-mode-buffer-or-file
;; https://github.com/tkf/org-mode/blob/master/lisp/org-publish.el
;; https://www.emacswiki.org/emacs/ElispCookbook

;; https://yannesposito.com/posts/0001-new-blog/index.html

;; for testing
;; (defun tt-parse-buff ()
;;   "2019-01-14"
;;   (interactive)
;;   (let ((tt (org-element-parse-buffer )))
;;     (with-output-to-temp-buffer "*xah temp out*"
;;       (print tt))))


(defun org-blog-sitemap-format-entry (entry _style project)
  "Return string for each ENTRY in PROJECT."
  ;; (when (s-starts-with-p "posts/" entry)
  (format (concat "@@html:<span class=\"archive-item\" data-tags=\"%s\">"
                  "<span class=\"archive-date\">@@ %s: @@html:</span>@@"
                  " [[file:%s][%s]]"
                  " @@html:</span>@@")
          (org-publish-find-file-tags (expand-file-name (concat "content/blog/" entry)))
          (format-time-string "%m-%d-%Y" (org-publish-find-date entry project))
          entry
          (org-publish-find-title entry project)))

(defun org-blog-sitemap-function (title list)
  "Return sitemap using TITLE and LIST returned by `org-blog-sitemap-format-entry'."
  (concat "#+TITLE: " title "\n"
          ;; "#+AUTHOR: " author-name "\n"
          ;; "#+EMAIL: " author-email "\n"
          "\n#+begin_archive\n"
          (mapconcat (lambda (li)
                       (format "@@html:<li>@@ %s @@html:</li>@@" (car li)))
                     (seq-filter #'car (cdr list))
                     "\n")
          "\n#+end_archive\n"))

(defvar *path-to-publishing-directory* "~/code/my-website"
  "Path to directory where website is built.")

(defvar *path-to-source-directory* "~/code/my-website-src"
  "Path to directory where website source resides.")

;; Define the publishing project
(setq org-publish-project-alist
      (list
       (list "org-pages"
             :recursive nil
             :base-directory (concat *path-to-source-directory* "/content/")
             :base-extension "org"             
             :publishing-directory (concat *path-to-publishing-directory* "/public")
             :publishing-function 'org-html-publish-to-html
             :exclude ".*\~\\|.*\.draft\.org"
             :with-author nil         ;; Don't include author name
             :with-creator nil        ;; Don't include Emacs and Org versions in footer
             :with-toc nil            ;; Don't include table of contents
             :section-numbers nil     ;; Don't include section numbers
             :time-stamp-file nil     ;; Don't include time stamp in file
             :html-preamble (read-file (concat *path-to-source-directory* "/layout/preamble.html"))
             :auto-sitemap nil)
       (list "blog-content"
             :recursive t
             :base-directory "~/code/my-website-src/content/blog"
             :base-extension "org\\|html"
             :publishing-directory (concat *path-to-publishing-directory* "/public/blog")
             :publishing-function 'org-html-publish-to-html
             :exclude ".*\~\\|.*\.draft\.org"
             :with-author nil         ;; Don't include author name
             :with-creator nil        ;; Don't include Emacs and Org versions in footer
             :with-toc t              ;; Include table of contents
             ;; :with-tags t
             :section-numbers nil     ;; Don't include section numbers
             :time-stamp-file nil     ;; Don't include time stamp in file
             :html-preamble (read-file (concat *path-to-source-directory* "/layout/preamble.html"))
             :auto-sitemap t
             :sitemap-filename "blog-archive.org"
             :sitemap-title "Blog Posts"
             :sitemap-sort-files 'anti-chronologically
             :sitemap-format-entry 'org-blog-sitemap-format-entry
             :sitemap-function 'org-blog-sitemap-function
             :sitemap-style 'list)
       (list "static"
             :recursive t
             :base-directory (concat *path-to-source-directory* "/content")
             :exclude ".*\~\\|.*node_modules\/.*" ;; Using CDN, excluding local node modules
             :base-extension "css\\|js\\|html\\|png\\|jpg\\|jpeg\\|svg\\|webp\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|stl\\|obj\\|wasm"
             :publishing-directory (concat *path-to-publishing-directory* "/public")
             :publishing-function 'org-publish-attachment)
       ))
