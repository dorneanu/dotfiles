;;; +functions.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hugo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dorneanu/hugo-add-slug ()
  "Adds a Hugo slug as EXPORT_FILE_NAME property"
 (interactive)
 (org-set-property "EXPORT_FILE_NAME"
 (concat (format-time-string "%Y") "-" (org-hugo-slug (org-get-heading :no-tags :no-todo)))))

;; see https://www.reddit.com/r/emacs/comments/q0nlgy/extract_link_from_org_header_and_insert_as/
(defun dorneanu/hugo-org-replace-link-by-link-description ()
  "Replace an org link by its description or if empty its address and adds hugo front matter as URL"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2)
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1)))
              (url (org-match-string-no-properties 1)))
          (apply 'delete-region remove)
          (insert description)
          (org-entry-put nil "EXPORT_HUGO_CUSTOM_FRONT_MATTER" (concat ":posturl " url))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; getpocket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copy current url to scratch buffer
(defun dorneanu/pocket-reader-copy-to-scratch ()
  "Copy URL of current item to kill-ring/clipboard."
  (interactive)
  (when-let ((id (tabulated-list-get-id))
             (item (ht-get pocket-reader-items id))
             (url (pocket-reader--get-url item)))
    (with-current-buffer "*scratch*"
      (insert url)
      (newline))
     (message "Added: %s to scratch buffer" url)))

;; Remove 2read and next tags from current pocket reader item
(defun dorneanu/pocket-reader-remove-next()
  (interactive)
  (pocket-reader--remove-tags (list "2read" "next"))
  (message "Removed 2read, next tags from current item")
  (pocket-reader-toggle-archived)
  (message "Archived item")
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tiddlywiki
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dorneanu/tiddlywiki-add-bookmark ()
  "Adds a new bookmark to tiddlywiki. The URL is fetched from clipboard or killring"
    (require 'url-util)
    (interactive)
    (pocket-reader-copy-url)

    (setq my-url (org-web-tools--get-first-url))
    (setq url-html (org-web-tools--get-url my-url))
    (setq url-title (org-web-tools--html-title url-html))
    (setq url-title-mod (read-string "Title: " url-title))
    (setq url-path (url-hexify-string url-title-mod))
    (setq url-note (read-string (concat "Note for " my-url ":")))
    (setq url-tags (concat "Bookmark "(read-string "Additional tags: ")))

    (request (concat "http://127.0.0.1:8181/recipes/default/tiddlers/" url-path)
    :type "PUT"
    :data (json-encode `(("name" . ,url-title-mod) ("note" . ,url-note) ("url" . ,my-url) ("tags" . ,url-tags)))
    :headers '(("Content-Type" . "application/json") ("X-Requested-With" . "TiddlyWiki") ("Accept" . "application/json"))
    :parser 'json-read
    :success
    (cl-function
            (lambda (&key data &allow-other-keys)
                (message "I sent: %S" (assoc-default 'args data))))
    :complete (lambda (&rest _) (message "Added %s" (symbol-value 'url-title-mod)))
    :error (lambda (&rest _) (message "Some error"))
    :status-code '((400 . (lambda (&rest _) (message "Got 400.")))
                    (418 . (lambda (&rest _) (message "Got 418.")))
                    (204 . (lambda (&rest _) (message "Got 202."))))
    )
)

(defun dorneanu/tiddlywiki-add-journal()
  "Adds a new Tiddlywiki journal by fetching the last kill ring entry as content"
  (interactive)
  (let ((current-date (format-time-string "%Y-%m-%d")))
    ;; (message "%s" current-date)
    (request (concat "http://127.0.0.1:8181/recipes/default/tiddlers/" current-date)
    :type "PUT"
    :data (json-encode `(("title" . ,current-date) ("tags" . "Journal") ("text" . ,(current-kill 0 t))))
    :headers '(("Content-Type" . "application/json") ("X-Requested-With" . "TiddlyWiki") ("Accept" . "application/json"))
    :parser 'json-read
    :success
    (cl-function
            (lambda (&key data &allow-other-keys)
                (message "I sent: %S" (assoc-default 'args data))))
    :complete (lambda (&rest _) (message "Finished! %s" current-date))
    :error (lambda (&rest _) (message "Some error"))
    :status-code '((400 . (lambda (&rest _) (message "Got 400.")))
                    (418 . (lambda (&rest _) (message "Got 418.")))
                    (204 . (lambda (&rest _) (message "Got 202."))))
    ))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc/Uncategorized
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dorneanu/org-link-make-string (link &optional description)
  "Make a bracket link, consisting of LINK and DESCRIPTION.
LINK is escaped with backslashes for inclusion in buffer."
  (let* ((zero-width-space (string ?\x200B))
         (description
          (and (org-string-nw-p description)
               ;; Description cannot contain two consecutive square
               ;; brackets, or end with a square bracket.  To prevent
               ;; this, insert a zero width space character between
               ;; the brackets, or at the end of the description.
               (replace-regexp-in-string
                "\\(]\\)\\(]\\)"
                (concat "\\1" zero-width-space "\\2")
                (replace-regexp-in-string "]\\'"
                                          (concat "\\&" zero-width-space)
                                          (org-trim description))))))
    (if (not (org-string-nw-p link)) description
      (format "[[%s|%s]]"
              (org-link-escape link)
              (if description (format "%s" description) "")))))

(defun dorneanu/org-web-tools--org-link-for-url (url)
  "Return Org link to URL using title of HTML page at URL.
If URL is not given, look for first URL in `kill-ring'.  If page
at URL has no title, return URL."
  (let* ((html (org-web-tools--get-url url))
         (title (org-web-tools--html-title html)))
    (if title
        (dorneanu/org-make-link-string url title)
      (message "HTML page at URL has no title")
      url)))

;; https://blog.lazkani.io/posts/text-editors/bookmark-with-org-capture/
(defun org-web-tools-insert-link-for-clipboard-url ()
  "Extend =org-web-tools-inster-link-for-url= to take URL from clipboard or kill-ring"
  (interactive)
  (org-web-tools--org-link-for-url (org-web-tools--get-first-url)))


;; Setup markdown preview impatient mode
;; from: https://blog.bitsandbobs.net/blog/emacs-markdown-live-preview/
(defun my-markdown-filter (buffer)
  (princ
   (with-temp-buffer
     (let ((tmp (buffer-name)))
       (set-buffer buffer)
       (set-buffer (markdown tmp))
       (format "<!DOCTYPE html><html><title>Markdown preview</title><link rel=\"stylesheet\" href = \"https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/3.0.1/github-markdown.min.css\"/>
<body><article class=\"markdown-body\" style=\"box-sizing: border-box;min-width: 200px;max-width: 980px;margin: 0 auto;padding: 45px;\">%s</article></body></html>" (buffer-string))))
   (current-buffer)))

(defun my-markdown-preview ()
  "Preview markdown."
  (interactive)
  (unless (process-status "httpd")
    (httpd-start))
  (impatient-mode)
  (imp-set-user-filter 'my-markdown-filter)
  (imp-visit-buffer))

(defun +remap-faces-at-start-present ()
  (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
                                     (org-verbatim (:height 1.75) org-verbatim)
                                     (org-block (:height 1.25) org-block)))
  (hide-mode-line-mode 1)
  (centaur-tabs-mode 0)
  )

(defun +remap-faces-at-start-present-term ()
  (interactive)
  (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
                                     (org-verbatim (:height 1.75) org-verbatim)
                                     (org-block (:height 1.25) org-block)))
  )

(defun +remap-faces-at-stop-present ()
  (setq-local face-remapping-alist '((default variable-pitch default)))
  (hide-mode-line-mode 0)
  (centaur-tabs-mode 1)
  )

;; custom functions for getpockt
(defun dorneanu/pocket-2share ()
  "Show getpocket items to be shared"
  (interactive)
  (pocket-reader-search ":all t:2share" :add nil))

(defun dorneanu/pocket-2read ()
  "Show getpocket items to be shared"
  (interactive)
  (pocket-reader-search "t:2read" :add nil))


;; From https://github.com/svetlyak40wt/dot-emacs/blob/master/.emacs.d/lib/org-auto-clock.el
;; Auto clock-in when task is marked STARTED
(eval-after-load 'org
  '(progn
     (defun wicked/org-clock-in-if-starting ()
       "Clock in when the task is marked STARTED."
       (when (and (string= org-state "STARTED")
                  (not (string= org-last-state org-state)))
         (org-clock-in)))

     (add-hook 'org-after-todo-state-change-hook
               'wicked/org-clock-in-if-starting)

     (defadvice org-clock-in (after wicked activate)
       "Set this task's status to 'STARTED'."
       (org-todo "STARTED"))


     (defun wicked/org-clock-out-if-waiting ()
       "Clock out when the task is marked WAITING or WIP (Work in Progress)."
       (when (and (or (string= org-state "WAITING")
                      (string= org-state "WIP"))
                  (equal (marker-buffer org-clock-marker) (current-buffer))
                  (< (point) org-clock-marker)
                  (> (save-excursion (outline-next-heading) (point))
                     org-clock-marker)
                  (not (string= org-last-state org-state)))
         (org-clock-out)))

     (add-hook 'org-after-todo-state-change-hook
               'wicked/org-clock-out-if-waiting)))

;; show pomodoro also on CLI
;; from https://colekillian.com/posts/org-pomodoro-and-polybar/
(defun ruborcalor/org-pomodoro-time ()
  "Return the remaining pomodoro time"
  (if (org-pomodoro-active-p)
      (cl-case org-pomodoro-state
        (:pomodoro
           (format "Pomo: %d minutes - %s" (/ (org-pomodoro-remaining-seconds) 60) org-clock-heading))
        (:short-break
         (format "Short break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
        (:long-break
         (format "Long break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
        (:overtime
         (format "Overtime! %d minutes" (/ (org-pomodoro-remaining-seconds) 60))))
    "No active pomo"))

;; from https://github.com/dfeich/org8-wikiexporters/blob/master/ox-tiddly.el
;; Converts an ORG link into Tiddlywiki one
(defun dorneanu/org-tiddly-link (link desc)
  (let ((raw-link (org-element-property :raw-link link)))
    (concat "[["
            (when (org-string-nw-p desc) (format "%s|" desc))
	    raw-link
            "]]")))



;; From https://stackoverflow.com/questions/20866169/change-the-font-of-current-buffer-in-emacs
;; https://emacs.stackexchange.com/questions/3038/using-a-different-font-for-each-major-mode
(defun dorneanu/set-variable-font-current-buffer ()
   "Set font to a variable width (proportional) fonts in current buffer"
   (interactive)
   (face-remap-add-relative 'default :family "Source Code Pro"))

 (defun dorneanu/set-monospace-font-current-buffer ()
   "Sets a fixed width (monospace) font in current buffer"
   (interactive)
   (face-remap-add-relative 'default :family "Fira Mono"))


(defun zz/org-download-paste-clipboard (&optional use-default-filename)
  (interactive "P")
  (require 'org-download)
  (let ((file
         (if (not use-default-filename)
             (read-string (format "Filename [%s]: "
                                  org-download-screenshot-basename)
                          nil nil org-download-screenshot-basename)
           nil)))
    (org-download-clipboard file)))

;; https://emacs.stackexchange.com/questions/63037/fast-way-to-copy-a-link-at-point-in-org-mode
(defun ndk/link-fast-copy ()
   (interactive)
   (let* ((context (org-element-context))
          (type (org-element-type context))
          (beg (org-element-property :begin context))
          (end (org-element-property :end context)))
     (when (eq type 'link)
      (copy-region-as-kill beg end))))

;; https://emacs.stackexchange.com/questions/3981/how-to-copy-links-out-of-org-mode
(defun dorneanu/org-export-url ()
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info
                 (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                 (or (caddr link-info) (point-max))))))
    (if (not text)
        (error "Not in org link")
      (string-match org-bracket-link-regexp text)
      (kill-new (substring text (match-beginning 1) (match-end 1))))))
