;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Resources:
;; - https://github.com/zzamboni/dot-emacs/blob/master/init.org
;; - https://ladicle.com/post/config
;; - https://www.suenkler.info/notes/emacs-config/
;; - https://www.gtrun.org/custom/config.html
;; - https://hugocisneros.com/org-config/
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


;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-monokai-spectrum)


;; set clipboard settings
(require 'xclip)
(xclip-mode 1)

;; ORG Mode configuration
(use-package org
  :ensure t
  :bind
  (:map org-mode-map
   ("C-c o c" . org-open-at-point-with-chrome)
   ("C-c o e" . org-open-at-point-with-eww)
   )
  :config
  (progn
    ;; set org-directory
    (setq org-directory "~/work/repos/org/")

    ;; right-align tags
    (setq org-tags-column 100)
    (setq org-agenda-tags-column 100)
    (setq org-use-tag-inheritance t)

    ;; set indentation
    (setq org-startup-indented t)
    (setq org-indent-indentation-per-level 2)
    (setq org-edit-src-content-indentation 0)
    (setq org-src-preserve-indentation t)

    ;; No blank lines before new entries
    (setq org-blank-before-new-entry
          '((heading . nil)
            (plain-list-item . nil)))

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

    ;; Refiling
    ;; Allow to create new nodes when refiling
    (setq org-refile-targets '((nil :maxlevel . 9)
                           (org-agenda-files :maxlevel . 9)))
    (setq org-refile-allow-creating-parent-nodes 'confirm)

    ;; TODO keywords
    (setq org-todo-keywords '((sequence "TODO(t)" "WIP(i)"  "MEETING(m)" "STARTED(s)" "NEXT(n)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))
    (setq org-todo-keyword-faces
          '(("WIP" . (:foreground "brightblue" :weight bold))
            ("NEXT" . (:foreground "IndianRed1" :weight bold))
            ("TODO" . (:foreground "green" :weight bold))
            ("MEETING" . (:foreground "forest green" :weight bold))
            ("STARTED" . (:foreground "OrangeRed" :weight bold))
            ("WAITING" . (:foreground "coral" :weight bold))
            ("CANCELED" . (:foreground "Red" :weight bold))
            ;; ("DELEGATED" . (:foreground "LimeGreen" :weight bold))
            ;; ("SOMEDAY" . (:foreground "LimeGreen" :weight bold))
            ;; ("BUG" . (:foreground "Orange" :weight bold))
            ;; ("PING" . (:foreground "Green" :weight bold))
            ))

    ;; where to put notes
    (setq org-default-notes-file (concat org-directory "notes.org"))

    ;; Set default column view headings: Task Total-Time Time-Stamp
    ;; from http://cachestocaches.com/2016/9/my-workflow-org-agenda/
    (setq org-columns-default-format "%50ITEM(Task) %10TODO %10CLOCKSUM %18CLOSED %18TIMESTAMP_IA")
    )

  ;; Bug for https://github.com/hlissner/doom-emacs/issues/5714
  ;; Solution: https://github.com/hlissner/doom-emacs/issues/4832#issuecomment-822845907
  (defalias '+org--restart-mode-h #'ignore)

  ;; Enable variable and visual line mode in Org mode by default.
  (add-hook! org-mode :append
             #'visual-line-mode
             #'variable-pitch-mode)

  ;; add org-habit
  (add-to-list 'org-modules 'org-habit)

  ;; Effort entries
  (add-to-list 'org-global-properties
               '("Effort_ALL". "0:05 0:15 0:30 1:00 2:00 3:00 4:00"))

  ;; add a new item when hitting return in a bulleted list
  (add-hook 'org-mode-hook
            (lambda ()
              (org-autolist-mode)
              ))

  ;; add hook font for org mode
  (add-hook 'org-mode-hook 'dorneanu/set-monospace-font-current-buffer)

  ;; from https://zzamboni.org/post/beautifying-org-mode-in-emacs/
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([+]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "◦"))))))

  ;; Open GPG files in org mode
  (add-to-list 'auto-mode-alist '("\\.gpg\\'" . org-mode))

  ;; Set custom theme faces
  (custom-theme-set-faces
   'user
   `(variable-pitch ((t (:family "Fira Mono"))))
   `(fixed-pitch ((t ( :family "DejaVu Sans Mono"))))
   `(org-block ((t (:inherit fixed-pitch))))
   `(org-code ((t (:inherit (shadow fixed-pitch)))))
   `(org-document-info ((t (:foreground "dark orange"))))
   `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   `(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   `(org-link ((t (:foreground "royal blue" :underline t))))
   `(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   `(org-property-value ((t (:inherit fixed-pitch))) t)
   `(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   `(org-table ((t (:inherit variable-pitch :foreground "#83a598"))))
   `(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   `(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   )

  ;; Set browser functions
  (defun org-open-at-point-with-chrome ()
    (interactive)
    (let ((browse-url-browser-function 'browse-url-chrome))
      (org-open-at-point )))

  (defun org-open-at-point-with-eww ()
    (interactive)
    (let ((browse-url-browser-function 'eww-browse-url))
      (org-open-at-point )))
  )

;; Define captures here
(use-package org-capture
  :after org
  :defer 1
  :bind
  ("C-c c"  . org-capture)
  :custom
  (org-capture-templates
   '(
     ;; ("l" "Ledger")
     ;; ("lb" "Bank" plain (file "~/work/repos/org/main.ledger.gpg")
     ;;     "%(org-read-date) * %^{Description}\n\tExpenses:%^{Account}  %^{Amount}EUR\n\tAssets:Current:ING:Visa\n"
     ;;     :empty-lines 1)
     ;; ("lc" "Cash" plain (file "~/work/sync/org/main.ledger"),
     ;;     "%(org-read-date) * %^{Payee}
     ;;     Expenses:%^{Account}  €%^{Amount}
     ;;     Assets:Cash:Wallet"
     ;;     :empty-lines 1)

     ;; Docs
     ;; - Elements: https://orgmode.org/manual/Template-elements.html
     ;; - Expansion: https://orgmode.org/manual/Template-expansion.html
     ("t" "Todo" entry (file+headline "~/work/repos/org/inbox.org" "Tasks")
      "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n %i\n")

     ("T" "Project Todo" entry (file+headline "~/work/repos/org/inbox.org" "Tasks")
      "* TODO %^{Description}\n:PROPERTIES:\n:CREATED: %U\n:END:\nDesired outcome: %^{Desired outcome} %i\n")

     ("m" "Meeting" entry (file+headline "~/work/repos/org/inbox.org" "Meetings")
      "* MEETING %?\nSCHEDULED: %t\n:PROPERTIES:\n:CREATED: %U\n:END:\n %i\n")

     ("b" "Bookmark (Clipboard)" entry (file+headline "~/work/repos/org/bookmarks.org" "Bookmarks")
      "** %(org-web-tools-insert-link-for-clipboard-url)\n:PROPERTIES:\n:TIMESTAMP: %t\n:END:\n%?"  :prepend t)

     ("s" "Code Snippet" entry
      (file+headline "~/work/repos/org/inbox.org" "Snippets")
      "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")

     ;; How to use custom lambda for finding the right heading
     ;; ("y" "Work Task" entry (file+function
     ;;                         "~/org/journal/work.org"
     ;;                         (lambda ()
     ;;                           (org-datetree-find-date-create
     ;;                            (org-date-to-gregorian (org-today)) t)
     ;;                           (re-search-forward "^\\*.+ log" nil t)))
     ;;  "* TODO %?\n%U" :empty-lines 1)

     ("j" "Journal" entry (file+datetree "~/work/repos/org/journal.org")
      "*  %?\n" :tree-type week :empty-lines 0)
     )
   )
)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(display-time-mode t)

;; (setq org-agenda-files (directory-files-recursively org-directory "\\.org$"))
;; add gpg files as well
;; (unless (string-match-p "\\.gpg" org-agenda-file-regexp)
  ;; )
;; setup org agenda
(use-package org-agenda
  :ensure t
  :after org
  :bind
  ("C-c a"      . org-agenda)
  :config
  (setq
   org-agenda-files (list org-directory)
   org-agenda-file-regexp
   (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
                             org-agenda-file-regexp)

   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t

   ;; Show warnings for deadlines 7 days in advance.
   org-deadline-warning-days 5
   org-agenda-include-deadlines t
   org-agenda-todo-list-sublevels t

   ;; org-agenda-todo-ignore-scheduled 'all
   ;; org-agenda-todo-ignore-deadlines 'all
   ;; org-agenda-todo-ignore-with-date 'all

   ;; Use straight line as separator between agenda blocks
   ;; https://www.utf8-chartable.de/unicode-utf8-table.pl?start=9472&utf8=dec&unicodeinhtml=dec
   org-agenda-block-separator 9472

   org-agenda-compact-blocks t
   org-agenda-start-day nil ;; i.e. today
   org-agenda-span 1
   org-agenda-start-on-weekday nil

   ;; Clock report settings
   org-agenda-start-with-clockreport-mode t
   org-clock-report-include-clocking-task t
   org-agenda-clockreport-parameter-plist
   '(:link t :maxlevel 6 :fileskip0 t :compact nil :narrow 90)

   ;; http://doc.endlessparentheses.com/Var/org-agenda-prefix-format.html
   org-agenda-prefix-format
   '((agenda . "%5c %4e %?-12t %s")
     (todo   . " %4e %-12c")
     (tags   . " %-22c")
     (search . " %-12c"))
   )
  )

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
;;         ;;
;; )

;; Use org-bullets
;; (use-package org-bullets
;;   :after org
;;   :custom
;;   (org-bullets-bullet-list '("◉" "☯" "○" "☯" "✸" "☯" "✿" "☯" "✜" "☯" "◆" "☯" "▶"))
;;   (org-ellipsis "⤵")
;;   :hook (org-mode . org-bullets-mode))


;; From https://www.rousette.org.uk/archives/doom-emacs-tweaks-org-journal-and-org-super-agenda/
(use-package org-super-agenda
  :after org-agenda
  :config
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
                           (:discard (:anything))))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-agenda-prefix-format '(
                                                     (agenda . "%7c %4e %?-12t %s")
                                                     (todo . " %-8c [%-4e] %?-12t %s")
                                                     (tags   . " %-22c")
                                                     (search . " %-12c")
                                                     ))
                         (org-super-agenda-groups
                          '(
                            (:log t)
                            (:discard (:tag "inactive"))
                            (:name "Started"
                             :todo ("STARTED")
                             :order 1)
                            (:name "Quickies"
                             :and (:effort< "0:15" :not (:tag "recurring"))
                             )
                            (:name "Overdue"
                             :deadline past
                             :scheduled past
                             :order 2)
                            (:name "Soon"
                             :deadline feature
                             :scheduled feature
                             :order 2)
                            (:name "To refile"
                             :category "inbox"
                             :todo ""
                             :order 10)
                            (:name "Next to do"
                             :todo "NEXT"
                             :order 20)
                            (:name "WIP"
                             :todo ("WIP")
                             :order 40)
                            (:name "Waiting"
                             :todo "WAITING"
                             :order 50)
                            (:discard (:anything))))))
            (alltodo "" ((org-agenda-overriding-header "")
                         (org-agenda-hide-tags-regexp "project\\|ticket\\|active")
                         (org-agenda-prefix-format '((todo . " %-8c [%-4e] %?-12t %s")))
                         (org-super-agenda-groups
                          '(
                            (:log t)
                            (:discard (:tag "inactive"))
                            (:name "Projects"
                             :auto-property "project"
                             :order 1)
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
  ;; Save the running clock and all clock history when exiting Emacs,
  ;; load it on startup
  (setq org-clock-persist t)
  (setq org-clock-history-length 35)
  ;; Resume clocking tasks when emacs is restarted
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
;; (add-to-list 'load-path "~/work/bin/notdeft")
;; (add-to-list 'load-path "~/work/bin/notdeft/extras")
;; (setq notdeft-directories '("~/work/repos/brainfck.org/tw5/tiddlers/"))
;; (setq notdeft-extension "tid")
;; (setq notdeft-secondary-extensions '("md" "org"))
;; (setq notdeft-file-display-function
;;         (lambda (file w)
;;           (when (> w 30)
;;             (let* ((s (file-name-nondirectory
;;                        (directory-file-name
;;                         (notdeft-dir-of-file file))))
;;                    (s (pcase s
;;                         ("bibliography-notes" "bib")
;;                         ("homepage-notes" "hp")
;;                         (_ s)))
;;                    (s (if (> (string-width s) 12)
;;                           (truncate-string-to-width s 12)
;;                         s)))
;;               (concat " " s)))))
;; (load "notdeft-example")

;; (setq plantuml-jar-path (expand-file-name "~/Downloads/plantuml.jar"))
;; (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;; (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))

;; setup company
(use-package company
  :diminish company-mode
  :config
  (setq company-format-margin-function #'company-text-icons-margin)
  (setq
   company-show-quick-access t
   company-idle-delay 0
   company-minimum-prefix-length 2
   )
  (use-package company-posframe
    :hook (company-mode . company-posframe-mode))
  :hook
  (after-init . global-company-mode)
  (after-init . company-posframe-mode)
  (plantuml-mode . (lambda () (set (make-local-variable 'company-backends)
                                   '((company-yasnippet
                                      company-dabbrev
                                      )))))
  ((go-mode) . (lambda () (set (make-local-variable 'company-backends)
                               '((company-yasnippet
                                  company-lsp
                                  company-files
                                  company-dabbrev-code
                                  )))))
  )


;; Automatically reload files was modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; setup projectile
(use-package projectile
  :defer 2
  :diminish projectile-mode
  :config
  (projectile-global-mode))


;; (use-package flyspell
;;   :defer 1
;;   :diminish)

;; use paredit
(use-package paredit
  :ensure t)

;; Setup iedit
(use-package iedit
  :config
  (set-face-background 'iedit-occurrence "Magenta")
  :bind
  ("C-;" . iedit-mode))

;; Setup helm
(use-package helm
  :defer 1
  :diminish helm-mode
  :bind
  (("C-x C-f"       . helm-find-files)
   ("C-x C-a"       . helm-do-grep-ag)
   ("C-x C-b"       . helm-buffers-list)
   ("C-x b"         . helm-multi-files)
   ("M-x"           . helm-M-x)
   ("C-c f r"       . helm-recentf)
   :map helm-map
   ("C-j"           . helm-next-line)
   ("C-k"           . helm-previous-line)
   :map helm-find-files-map
   ("C-<backspace>" . helm-find-files-up-one-level)
   ("C-f"           . helm-execute-persistent-action)
   ([tab]           . helm-ff-RET))
  :custom
  (helm-autoresize-max-height 0)
  (helm-autoresize-min-height 40)
  (helm-buffers-fuzzy-matching t)
  (helm-recentf-fuzzy-match t)
  (helm-semantic-fuzzy-match t)
  (helm-imenu-fuzzy-match t)
  (helm-split-window-in-side-p nil)
  (helm-move-to-line-cycle-in-source nil)
  (helm-ff-search-library-in-sexp t)
  (helm-scroll-amount 8)
  (helm-echo-input-in-header-line nil)
  :config
  (require 'helm-config)
  (helm-mode 1)
  (helm-autoresize-mode 1)
)

(use-package helm-flx
  :custom
  (helm-flx-for-helm-find-files t)
  (helm-flx-for-helm-locate t)
  :config
  (helm-flx-mode +1))

(use-package swiper-helm
  :bind
  ("C-s" . swiper))

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
  :commands (org-pomodoro)
  :config
  (setq
   org-pomodoro-length 50
   org-pomodoro-short-break-length 10
   )
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
  :ensure t
  :config
  (setq
   wakatime-python-bin "/home/victor/.pyenv/versions/emacs/bin/python"
   wakatime-cli-path "/home/victor/.pyenv/versions/emacs/bin/wakatime"))
(global-wakatime-mode)

;; Web development with web-mode
;; From https://www.suenkler.info/notes/emacs-config/
(use-package web-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    ;; (setq web-mode-engines-alist
    ;;       '(("django"    . "\\.html\\'")))
    (setq web-mode-ac-sources-alist
          '(("css" . (ac-source-css-property))
            ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-enable-auto-quoting t)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-auto-expanding t)
    (setq web-mode-enable-css-colorization t)))

;; Visualize undo history
(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :bind
  ("M-/" . undo-tree-redo)
  :config
  (global-undo-tree-mode 1))

;; Setup smartparens
(defun zz/sp-enclose-next-sexp (num)
  (interactive "p")
  (insert-parentheses (or num 1)))

(use-package smartparens
  :hook
  (after-init . smartparens-global-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "=" "=" :actions '(wrap))
  (sp-pair "+" "+" :actions '(wrap))
  (sp-pair "<" ">" :actions '(wrap))
  (sp-pair "$" "$" :actions '(wrap)))

(use-package wgrep
  :defer t
  :custom
  (wgrep-enable-key "e")
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package ag
  :custom
  (ag-highligh-search t)
  (ag-reuse-buffers t)
  (ag-reuse-window t)
  :config
  (use-package wgrep-ag))

;; yasnippet
(use-package yasnippet
  :diminish yas-minor-mode
  :custom (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :hook (after-init . yas-global-mode))

(load! "+functions")
(load! "+bindings")
(load! "+lsp")
(load! "+go")
(load! "+pgp")
(load! "+python")
(load! "+elfeed")
(load! "+mail")
