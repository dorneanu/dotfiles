;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Victor Dorneanu"
      user-mail-address "john@doe.com")

;; File operations
(setq
        tab-width 4
        inhibit-splash-screen t
        initial-scratch-message nil
        sentence-end-double-space nil
        make-backup-files nil
        indent-tabs-mode nil
        make-backup-files nil
        auto-save-visited-interval 15
        auto-save-default t)
(auto-save-visited-mode +1)

;; Set localleader key
(setq evil-snipe-override-evil-repeat-keys nil)
(setq doom-localleader-key ",")
(setq doom-localleader-alt-key "M-,")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "Source Code Pro for Powerline" :size 12 :weight 'normal)
;;       doom-variable-pitch-font (font-spec :family "Source Code Pro for Powerline" :size 13))
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;         doom-variable-pitch-font (font-spec :family "Noto Sans" :size 13))

;; Set fonts
;; (set-face-font 'default "Source Code Pro Medium 15")
(setq doom-font (font-spec :family "Fira Mono" :size 15))
;; (setq doom-font (font-spec :family "Courier" :size 15))
;;
;; Font/Face customizations
(custom-theme-set-faces
 'user
'(variable-pitch ((t (:family "Fira Mono"))))
'(fixed-pitch ((t ( :family "DejaVu Sans Mono"))))
'(org-block ((t (:inherit fixed-pitch))))
'(org-code ((t (:inherit (shadow fixed-pitch)))))
'(org-document-info ((t (:foreground "dark orange"))))
'(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
'(org-indent ((t (:inherit (org-hide fixed-pitch)))))
'(org-link ((t (:foreground "royal blue" :underline t))))
'(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
'(org-property-value ((t (:inherit fixed-pitch))) t)
'(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
'(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
'(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
'(org-verbatim ((t (:inherit (shadow fixed-pitch))))))


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-monokai-spectrum)


;; set clipboard settings
(require 'xclip)
(xclip-mode 1)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/work/repos/org/")


;; Define captures here
(use-package! org-capture
  :after org
   :custom
    (org-capture-templates
     '(
            ("l" "Ledger")
            ("lb" "Bank" plain (file "~/work/repos/org/main.ledger.gpg")
                "%(org-read-date) * %^{Description}\n\tExpenses:%^{Account}  %^{Amount}EUR\n\tAssets:Current:ING:Visa\n"
                :empty-lines 1)
            ;; ("lc" "Cash" plain (file "~/work/sync/org/main.ledger"),
            ;;     "%(org-read-date) * %^{Payee}
            ;;     Expenses:%^{Account}  €%^{Amount}
            ;;     Assets:Cash:Wallet"
            ;;     :empty-lines 1)

            ("t" "Todo" entry (file+headline "~/work/repos/org/inbox.org" "Tasks")
             "* TODO %?\n")

            ("m" "Meeting" entry (file+headline "~/work/repos/org/inbox.org" "Meetings")
             "* MEETING %?\nSCHEDULED: %t\n%U\n%a\n")

            ("b" "Bookmark (Clipboard)" entry (file+headline "~/work/repos/org/bookmarks.org" "Bookmarks")
             "** %(org-web-tools-insert-link-for-clipboard-url)\n:PROPERTIES:\n:TIMESTAMP: %t\n:END:\n%?"  :prepend t)

            ("s" "Code Snippet" entry
             (file+headline "~/work/repos/org/inbox.org" "Snippets")
             "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")

            ("j" "Journal" entry (file+olp+datetree "~/work/repos/org/journal.org")
             "*  %?\n")
            ))
)

(after! org
  ;; add org-habit
  (add-to-list 'org-modules 'org-habit)

  ;; right-align tags
  (setq org-tags-column 100)
  (setq org-use-tag-inheritance t)

  ;; set indentation
  (org-indent-mode 1)
  (setq org-indent-indentation-per-level 2)
  (setq org-edit-src-content-indentation 0)
  (setq org-src-preserve-indentation t)

  ;; do logging
  (setq org-log-into-drawer t)
  (setq org-log-done t)
  (setq org-log-reschedule nil)
  (setq org-log-redeadline nil)

  ;; disable org-babel execution while exporting
  (setq org-confirm-babel-evaluate nil)
  (setq org-export-use-babel t)

  ;; Use the special C-a, C-e and C-k definitions for Org, which enable some special behavior in headings.
  (setq org-special-ctrl-a/e t)
  (setq org-special-ctrl-k t)

  ;; Clean look
  (setq org-hide-emphasis-markers t
        org-fontify-done-headline t
        org-hide-leading-stars t
        org-pretty-entities t)

  ;; Change list bullets
  (setq org-list-demote-modify-bullet
        (quote (("+" . "-")
                ("-" . "+")
                ("*" . "-")
                ("1." . "-")
                ("1)" . "-")
                ("A)" . "-")
                ("B)" . "-")
                ("a)" . "-")
                ("b)" . "-")
                ("A." . "-")
                ("B." . "-")
                ("a." . "-")
                ("b." . "-"))))

  ;; Allow to create new nodes when refiling
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  (setq org-todo-keywords
        '(
          (sequence "TODO(t)" "WIP(i)"  "MEETING(m)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "BUG(b)" "PING(p)" "|" "DONE(d)")
          (sequence "|" "CANCELED(c)" "DELEGATED(l)" "SOMEDAY(f)")
          ))

  (setq org-todo-keyword-faces
        '(("WIP" . (:foreground "brightblue" :weight bold))
          ("NEXT" . (:foreground "IndianRed1" :weight bold))
          ("TODO" . (:foreground "green" :weight bold))
          ("MEETING" . (:foreground "forest green" :weight bold))
          ("STARTED" . (:foreground "OrangeRed" :weight bold))
          ("WAITING" . (:foreground "coral" :weight bold))
          ("CANCELED" . (:foreground "Red" :weight bold))
          ("DELEGATED" . (:foreground "LimeGreen" :weight bold))
          ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))
          ("BUG" . (:foreground "Orange" :weight bold))
          ("PING" . (:foreground "Green" :weight bold))
          ))

  ;; (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
  ;; add gpg files as well
  (unless (string-match-p "\\.gpg" org-agenda-file-regexp)
    (setq org-agenda-file-regexp
          (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                                    org-agenda-file-regexp)))

  ;; Set agenda files
  (setq org-agenda-files (list org-directory))

  ;; where to put notes
  (setq org-default-notes-file (concat org-directory "notes.org"))

  ;; Set default column view headings: Task Total-Time Time-Stamp
  ;; from http://cachestocaches.com/2016/9/my-workflow-org-agenda/
  (setq org-columns-default-format "%50ITEM(Task) %10TODO %10CLOCKSUM %18CLOSED %18TIMESTAMP_IA")

  ;; add a new item when hitting return in a bulleted list
  (add-hook 'org-mode-hook
            (lambda ()
              (org-autolist-mode)
              ))

  ;; add hook font for org mode
  (add-hook 'org-mode-hook 'dorneanu/set-monospace-font-current-buffer)
)


;; Enable variable and visual line mode in Org mode by default.
(add-hook! org-mode :append
           #'visual-line-mode
           #'variable-pitch-mode)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; setup org agenda
;; (after! org-agenda
;;         (setq org-agenda-include-diary nil)
;;         (setq org-agenda-span 2)
;;         (setq org-agenda-tags-column -100) ; take advantage of the screen width
;;         (setq org-agenda-sticky nil)
;;         (setq org-agenda-inhibit-startup t)
;;         (setq org-agenda-use-tag-inheritance t)
;;         (setq org-agenda-show-log t)
;;         (setq org-agenda-skip-scheduled-if-done t)
;;         (setq org-agenda-skip-deadline-if-done t)
;;         (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
;;         (setq org-agenda-start-day nil) ;; i.e. today
;;         (setq org-agenda-span 1)
;;         (setq org-agenda-start-on-weekday nil)

;;         ;; (setq org-agenda-time-grid
;;         ;; '((daily today require-timed)
;;         ;;         (800 1000 1200 1400 1600 1800 2000)
;;         ;;         "......" "----------------"))
;;         ;; (setq org-agenda-prefix-format
;;         ;;         '((agenda . " %i %-12:c%?-12t% s")
;;         ;;         ;; Indent todo items by level to show nesting
;;         ;;         (todo . " %i %-12:c%l")
;;         ;;         (tags . " %i %-12:c")
;;         ;;         (search . " %i %-12:c")))
;;         ;; custom commands
;;         (setq org-agenda-custom-commands
;;               `(
;;                 ("A" "Agenda"
;;                         ((agenda "" ((org-agenda-span 2)))
;;                         ;; Projects
;;                         (tags "+project-someday-TODO=\"DONE\"-TODO=\"SOMEDAY\"-inactive-evilplans"
;;                                 ((org-tags-exclude-from-inheritance '("project"))
;;                                 (org-agenda-prefix-format "  ")
;;                                 (org-agenda-overriding-header "Projects: ")
;;                                 (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
;;                         ;; Inbox
;;                         (alltodo ""
;;                                 ((org-agenda-prefix-format "%-6e ")
;;                                 (org-agenda-overriding-header "Inbox: ")))
;;                         (todo "WAITING-inactive"
;;                                 ((org-agenda-prefix-format "%-6e ")
;;                                 (org-agenda-overriding-header "Waiting: ")
;;                                 (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))))
;;                         ;; Unscheduled
;;                         (tags-todo "TODO=\"TODO\"-project"
;;                                 ((org-agenda-prefix-format "%-6e ")
;;                                 (org-agenda-overriding-header "Unscheduled TODO entries: ")
;;                                 (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))))
;;                         )
;;                 )
;;                 ("e" "Emacs" (tags "emacs"))
;;                 ("w" "Work related"
;;                         ((agenda "" ((org-agenda-span 1)))
;;                          (tags "+project+CATEGORY=\"work\"-TODO=\"DONE\"-TODO=\"CANCELED\"")
;;                         ))
;;                 ("p" "Private"
;;                  (
;;                         (agenda ""
;;                           ((org-agenda-span 0)
;;                           ))
;;                          (tags-todo "+project+CATEGORY=\"priv\""
;;                                 ((org-agenda-prefix-format "  ")
;;                                 (org-agenda-overriding-header "Projects: ")
;;                                 (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))
;;                         (todo "WAITING+CATEGORY=\"priv\""
;;                                 ((org-agenda-prefix-format "%-6e ")
;;                                 (org-agenda-overriding-header "Waiting: ")
;;                                 (org-agenda-sorting-strategy '(priority-down effort-up tag-up category-keep))))
;;                         )
;;                         ((org-agenda-compact-blocks t)))

;;                 ("h" "Home"
;;                         ((agenda "" ((org-agenda-span 2)))
;;                          (tags-todo "+project+CATEGORY=\"home\""
;;                                 ((org-agenda-prefix-format "  ")
;;                                 (org-agenda-overriding-header "Projects: ")
;;                                 (org-agenda-sorting-strategy '(priority-down tag-up category-keep effort-down))))

;;                         ))
;;                 ("x" "List projects with tasks" my/org-agenda-projects-and-tasks "+project")
;;                 ("t" tags-todo ""
;;                         ((org-agenda-sorting-strategy '(todo-state-up priority-down effort-up))))
;;                 ("W" "Weekly Review"
;;                         ((agenda "" ((org-agenda-span 7))); review upcoming deadlines and appointments
;;                                                         ; type "l" in the agenda to review logged items
;;                         (stuck "") ; review stuck projects as designated by org-stuck-projects
;;                         (todo "PROJECT") ; review all projects (assuming you use todo keywords to designate projects)
;;                         (todo "MAYBE") ; review someday/maybe items
;;                         (todo "WAITING"))) ; review waiting items
;;                         ;; ...other commands here
;;                  ("d" "Timeline for today" ((agenda "" ))
;;                         ((org-agenda-ndays 1)
;;                         (org-agenda-show-log t)
;;                         (org-agenda-log-mode-items '(clock closed))
;;                         (org-agenda-clockreport-mode t)
;;                         (org-agenda-entry-types '())))
;;                 ("." "Waiting for" todo "WAITING")
;;               )
;;         )
;; )

;; Use org-bullets
;; (use-package org-bullets
;;   :after org
;;   :custom
;;   (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
;;   (org-ellipsis "⤵")
;;   :hook (org-mode . org-bullets-mode))

;; from https://zzamboni.org/post/beautifying-org-mode-in-emacs/
(font-lock-add-keywords 'org-mode
 '(("^ *\\([-]\\) "
 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
(font-lock-add-keywords 'org-mode
 '(("^ *\\([+]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

;; From https://www.rousette.org.uk/archives/doom-emacs-tweaks-org-journal-and-org-super-agenda/
(use-package! org-super-agenda
  :after org-agenda
  :init
  (setq
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t

      org-deadline-warning-days 5
      org-agenda-include-deadlines t
      org-agenda-todo-list-sublevels t

      ;; org-agenda-todo-ignore-scheduled 'all
      ;; org-agenda-todo-ignore-deadlines 'all
      ;; org-agenda-todo-ignore-with-date 'all

      org-agenda-block-separator t
      org-agenda-compact-blocks t
      org-agenda-start-day nil ;; i.e. today
      org-agenda-span 1
      org-agenda-start-on-weekday nil)
  (setq org-agenda-custom-commands
        '(
          ("a" "Agenda"
                ((agenda "" ((org-agend-span 'day)
                                (org-super-agenda-groups
                                 '(
                                   (:name "Today"
                                    :time-grid t
                                    :date today
                                    :scheduled today
                                    :order 1)
                                ))))
                (alltodo "" ((org-agenda-overriding-header "")
                                (org-super-agenda-groups
                                '(
                                (:log t)
                                (:discard (:tag "inactive"))
                                (:name "To refile"
                                        :category "inbox"
                                        :todo ""
                                        :order 10)
                                (:name "Next to do"
                                        :todo "NEXT"
                                        :order 20)
                                (:name "Started"
                                        :todo ("STARTED")
                                        :order 30)
                                (:name "WIP"
                                        :todo ("WIP")
                                        :order 40)
                                (:name "Waiting"
                                        :todo "WAITING"
                                        :order 50)
                                (:name "Other"
                                        :auto-property "agenda-group"
                                        :todo "TODO"
                                        :order 60)
                                (:discard (:anything))))))))
          ("w" "Work"
                ((agenda "" ((org-agend-span 'day)
                                (org-super-agenda-groups
                                 '(
                                   (:discard (:not (:category ("work"))))
                                   (:name "Today"
                                    :time-grid t
                                    :date today
                                    :scheduled today
                                    :order 1)
                                ))))
                (alltodo "" ((org-agenda-overriding-header "")
                                (org-super-agenda-groups
                                '(
                                (:discard (:not (:category ("work"))))
                                (:discard (:tag "inactive"))
                                (:log t)
                                (:name "Due Today"
                                        :deadline today
                                        :order 1)
                                (:name "Started"
                                        :todo "STARTED"
                                        :order 1)
                                (:name "Next to do"
                                        :todo "NEXT"
                                        :order 2)
                                (:name "Waiting"
                                        :todo "WAITING"
                                        :order 50)
                                (:discard (:anything))))))))
          ("p" "Private"
                ((agenda "" ((org-agend-span 'day)
                                (org-super-agenda-groups
                                '((:discard (:not (:category ("priv"))))
                                ))))
                (alltodo "" ((org-agenda-overriding-header "")
                                (org-super-agenda-groups
                                '(
                                (:discard (:not (:category ("priv"))))
                                (:discard (:tag "inactive"))
                                (:log t)
                                (:name "Due Today"
                                        :deadline today
                                        :order 1)
                                (:name "Started"
                                        :todo "STARTED"
                                        :order 1)
                                (:name "Next to do"
                                        :todo "NEXT"
                                        :order 2)
                                (:name "Waiting"
                                        :todo "WAITING"
                                        :order 50)
                                (:name "ToDo"
                                        :auto-property "agenda-group"
                                        :todo "TODO"
                                        :order 60)
                                (:discard (:anything))))))))
          ("P" "Private (ALL))"
                ((agenda "" ((org-agend-span 'day)
                                (org-super-agenda-groups
                                '((:discard (:not (:category ("priv"))))
                                ))))
                (alltodo "" ((org-agenda-overriding-header "")
                                (org-super-agenda-groups
                                '(
                                (:discard (:not (:category ("priv"))))
                                (:discard (:tag "inactive"))
                                (:log t)
                                (:name "Due Today"
                                        :deadline today
                                        :order 1)
                                (:name "Started"
                                        :todo "STARTED"
                                        :order 1)
                                (:name "Next to do"
                                        :todo "NEXT"
                                        :order 2)
                                (:name "Waiting"
                                        :todo "WAITING"
                                        :order 50)
                                (:name "ToDo"
                                        :auto-property "agenda-group"
                                        :todo "TODO"
                                        :order 60)
                                (:name "Others"
                                        :todo ""
                                        :order 70)

                                ))))))
          ("h" "Home"
                ((agenda "" ((org-agend-span 'day)
                                (org-super-agenda-groups
                                '((:discard (:not (:category ("home"))))
                                ))))
                (alltodo "" ((org-agenda-overriding-header "Home ")
                                (org-super-agenda-groups
                                '(
                                (:discard (:not (:category ("home"))))
                                (:log t)
                                (:name "Started"
                                        :todo "STARTED"
                                        :order 1)
                                (:name "Next to do"
                                        :todo "NEXT"
                                        :order 2)
                                (:name "Waiting"
                                        :todo "WAITING"
                                        :order 50)
                                (:name "ToDo"
                                        :auto-property "agenda-group"
                                        :todo t
                                        :order 60)
                                (:discard (:anything))))))))

          ("H" "Home (ALL)"
                ((agenda "" ((org-agend-span 'day)
                                (org-super-agenda-groups
                                '((:discard (:not (:category ("home"))))
                                ))))
                (alltodo "" ((org-agenda-overriding-header "Home ")
                                (org-super-agenda-groups
                                '(
                                (:discard (:not (:category ("home"))))
                                (:log t)
                                (:name "Started"
                                        :todo "STARTED"
                                        :order 1)
                                (:name "Next to do"
                                        :todo "NEXT"
                                        :order 2)
                                (:name "Waiting"
                                        :todo "WAITING"
                                        :order 50)
                                (:name "ToDo"
                                        :auto-property "agenda-group"
                                        :todo t
                                        :order 60)
                                ))))))
          ))
  :config
  (org-super-agenda-mode))

;; persist clocks
(after! org-clock
  (setq org-clock-persist t)
  (org-clock-persistence-insinuate))

;; ox-hugo
(use-package! ox-hugo
  :after ox)

;; ox-reveal
(use-package! ox-reveal
  :after ox)

;; plantuml
(use-package plantuml-mode
  :config (setq plantuml-default-exec-mode "jar")
)

;; setup deft
;; (setq deft-directory "~/work/repos/brainfck.org/tw5/tiddlers")
;; (setq deft-recursive t)
;; (setq deft-use-filename-as-title t)
;; (setq deft-text-mode 'org-mode)
;; (setq deft-extensions '("txt" "tex" "org" "md" "tid" "markdown"))

;; setup not-deft
(add-to-list 'load-path "~/work/bin/notdeft")
(add-to-list 'load-path "~/work/bin/notdeft/extras")
(setq notdeft-directories '("~/work/repos/brainfck.org/tw5/tiddlers/"))
(setq notdeft-extension "tid")
(setq notdeft-secondary-extensions '("md" "org"))
(setq notdeft-file-display-function
        (lambda (file w)
          (when (> w 30)
            (let* ((s (file-name-nondirectory
                       (directory-file-name
                        (notdeft-dir-of-file file))))
                   (s (pcase s
                        ("bibliography-notes" "bib")
                        ("homepage-notes" "hp")
                        (_ s)))
                   (s (if (> (string-width s) 12)
                          (truncate-string-to-width s 12)
                        s)))
              (concat " " s)))))
;; (load "notdeft-example")

;; (setq plantuml-jar-path (expand-file-name "~/Downloads/plantuml.jar"))
;; (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;; (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

;; setup company
;; (after! company-box
;;    (setq company-box-max-candidates 5))

;; (after! company
;;     (setq company-tooltip-limit 5
;;           company-tooltip-minimum-width 80
;;           company-tooltip-minimum 5
;;           company-backends
;;           '(company-capf company-dabbrev company-files company-yasnippet)
;;           company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode)))

;; setup dired
(after! dired-k
    (setq dired-k-human-readable t))
(after! dired-k
    (setq dired-k-size-colors
        `((1024 .   ,(doom-lighten (doom-color 'green) 0.3))
        (2048 .   ,(doom-lighten (doom-color 'green) 0.2))
        (3072 .   ,(doom-lighten (doom-color 'green) 0.1))
        (5120 .   ,(doom-color 'green))
        (10240 .  ,(doom-lighten (doom-color 'yellow) 0.2))
        (20480 .  ,(doom-lighten (doom-color 'yellow) 0.1))
        (40960 .  ,(doom-color 'yellow))
        (102400 . ,(doom-lighten (doom-color 'orange) 0.2))
        (262144 . ,(doom-lighten (doom-color 'orange) 0.1))
        (524288 . ,(doom-color 'orange)))))

(setq browse-url-browser-function 'eww-browse-url)

;; setup twitter
(setq twittering-use-master-password t)
(setq twittering-allow-insecure-server-cert t)
;; (setq twittering-oauth-invoke-browser nil)

;; setup elfeed
(setq-default elfeed-search-filter "@2-day-ago +unread ")
(setq rmh-elfeed-org-files (list "~/work/repos/org/elfeed.org.gpg"))

;; setup ox-tiddly
(require 'ox-tiddly)

;; org-pomodoro
(use-package org-pomodoro
  :after org-agenda
  ;; :config
  ;; (add-hook 'org-pomodoro-started-hook
  ;;           (lambda ()
  ;;             (shell-command "ssh mac say 'Pomodoro gestartet'")
  ;;             ))
  ;; (add-hook 'org-pomodoro-finished-hook
  ;;           (lambda ()
  ;;             (shell-command "ssh mac say 'Pomodoro fertig. Mach eine Pause!'")
  ;;             ))
  ;; (add-hook 'org-pomodoro-break-finished-hook
  ;;           (lambda ()
  ;;             (shell-command "ssh mac say 'Pause fertig'")
  ;;             ))
  ;; (add-hook 'org-pomodoro-killed-hook
  ;;           (lambda ()
  ;;             (shell-command "ssh mac say 'Pomodoro gekilled'")
  ;;             ))
  )

;; pocket-reader
;; don't archive automatically on open
(setq pocket-reader-archive-on-open nil)

;; open tid files (Tiddlywiki) in org-mode
(add-to-list 'auto-mode-alist '("\\.tid\\'" . org-mode))

;; Setup wakatime
(use-package wakatime-mode
  :ensure t)
(global-wakatime-mode)

(load! "+functions")
(load! "+bindings")
(load! "+lsp")
(load! "+go")
(load! "+pgp")
(load! "+python")
(load! "+elfeed")
(load! "+mail")
