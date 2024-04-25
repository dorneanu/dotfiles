;;; modules.el -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Abdelhak Bougouffa

;; This file can be used to override `minemacs-modules'
;; and `minemacs-core-modules'

;;; Ordered list of enabled core modules
(setq minemacs-core-modules
      '(me-splash         ; Simple splash screen
        me-keybindings    ; Keybinding (general, which-key, hydra, ...)
        me-evil           ; Emacs as Vim (evil, evil-collection, evil-snipe, evil-numbers, ...)
        me-core-ui        ; Core UI (doom-themes, modus-themes, doom-modeline, ...)
        me-completion))   ; Completion (vertico, marginalia, corfu, cape, consult, embark, ...)

;;; List of enabled modules
(setq minemacs-modules
      '(me-ui             ; User interface (svg-lib, focus, mixed-pitch, ...)
        ;; me-nano           ; N Λ N O Emacs, ...
        me-ai             ; AI assistant using ollama (ellama, llm, ...)
        me-editor         ; Editing (tempel, tempel-collection, rainbow-delimiters, expreg, drag-stuff, ...)
        me-daemon         ; Emacs daemon tweaks
        me-undo           ; Better undoing (undo-fu-session, vundo, ...)
        me-multi-cursors  ; Multi-cursors editing (iedit, evil-mc, evil-iedit-state, ...)
        me-vc             ; Version control (magit, forge, core-review, diff-hl, ...)
        me-project        ; Project management (projection, compile-multi, consult-project-extra, ...)
        me-prog           ; Programming stuff (tree-sitter, eldoc-box, apheleia, editorconfig, ...)
        me-checkers       ; Static checkers (flymake-collection, flymenu, flymake-quickdef, flymake-cppcheck, ...)
        me-debug          ; Debugging tools (realgud, disaster, dape, ...)
        me-emacs-lisp     ; Emacs lisp development (parinfer-rust, macrostep, eros, helpful, ...)
        ;; me-common-lisp ; Common Lisp development (sly, sly-quicklisp, ...)
        ;; me-scheme      ; Scheme development (racket-mode, geiser, ...)
        ;; me-clojure     ; Clojure development (clojure-mode, cider, ...)
        ;; me-embedded    ; Embedded systems (embed, arduino, openocd, bitbake, dts-mode, ...)
        ;; me-robot       ; Robotics stuff (ros, robot-mode, ...)
        me-data           ; Data file formats (csv, yaml, toml, json, plantuml-mode, ...)
        ;; me-math        ; Mathematics (maxima, ess, ein, code-cells, julia-mode, ...)
        ;; me-modeling    ; Modeling tools (scad-mode, modelica-mode, ...)
        me-org            ; Org-mode for life (org-contrib, org-modern, org-appear, engrave-faces, ...)
        extras/me-org-extras
        extras/me-writing-mode
        me-extra          ; Extra features (better-jumper, crux, ...)
        ;; me-notes          ; Notes & Zettelkasten (denote, consult-notes, ...)
        ;; me-email       ; Email (mu4e, mu4e-alert, org-msg, ...)
        me-rss         ; News feed (elfeed, ...)
        ;; me-lifestyle   ; *Very* opinionated lifestyle packages (awqat, ...)
        me-docs           ; Documents (pdf-tools, pdf-view-restore, nov, crdt, edraw, markdown-mode, ...)
        ;; me-calendar    ; Calendar (calfw, calfw-org, calfw-ical, ...)
        ;; me-latex          ; LaTeX (auctex, auctex-latexmk, LaTeX-preview-pane, ...)
        ;; me-biblio      ; Bibliography & citations (citar, citar-embark, org-re-reveal-citeproc, ...)
        me-natural-langs  ; Natural language stuff (jinx, spell-fu, go-translate, eglot-ltex, ...)
        me-files          ; Files and directories (dirvish, vlf, sudo-edit, ztree, ...)
        me-tools          ; System tools (vterm, tldr, ssh-deploy, docker, logview, with-editor, ...)
        me-tty            ; Emacs from terminal (xclip, ...)
        ;; me-fun            ; Games and funny packages (xkcd, speed-type, wordel, ...)
        me-media          ; Multimedia (empv, emms, ...)
        me-workspaces  ; Workspace separation (tabspaces, ...)
        me-binary         ; Display binary files in hex or decompile them
        me-window         ; Frame & window tweaks
        obsolete/me-yasnippet))
        ;;obsolete/me-lsp))       ; Good old yasnippet

;;; List of disabled packages
;; You can set `minemacs-disabled-packages' to disable some packages. For
;; example, if you want to use the `me-ui' module, but you want to disable the
;; `focus' package. You can use:
;; (push 'embark minemacs-disabled-packages)
;; (push 'eglot minemacs-disabled-packages)

;; Adding a package to `minemacs-disabled-packages' guarantees disabling its
;; corresponding `use-package' section in MinEmacs' modules. However, please
;; note that, if you want to completely disable a package, you need to make sure
;; you've also disabled its dependent packages (see `M-x straight-dependents'),
;; otherwise it will get installed as a dependency.
;;
;; You can also `push' (or `add-to-list') multiple packages at once (as a list).
;; For example, to completely disable `iedit' and its dependencies
;; `evil-multiedit' and `evil-iedit-state', you can use:
;; (push '(iedit evil-multiedit evil-iedit-state) minemacs-disabled-packages)
(push '(cape-yasnippet treesit-auto lsp-mode) minemacs-disabled-packages)
;;; Using the obsolete modules
;; You can use the obsolete packages configurations by adding the
;; `obsolete/me-*' modules to `minemacs-modules'. However, these modules, as
;; their names indicate, are OBSOLETE and NOT SUPPORTED. This is a
;; non-comprehensive list of obsolete modules, see "modules/obsolete/*.el" for
;; the full list.
;; (setq minemacs-modules
;;       (append
;;        minemacs-modules
;;        '(
;;          obsolete/me-blamer         ; M-x git blame
;;          obsolete/me-chezmoi        ; Integrate chezmoi with Emacs
;;          obsolete/me-cov            ; Show code coverage results (cov, ...)
;;          obsolete/me-eaf            ; EAF apps (browser, jupyter, file-sender, ...)
;;          obsolete/me-evil-escape    ; Escape without ESC (evil-escape, ...)
;;          obsolete/me-expand-region  ; Expand region (included as an alternative for `expreg' in non tree-sitter builds)
;;          obsolete/me-flycheck       ; Static checkers (flycheck, ...)
;;          obsolete/me-flycheck-cmake ; Flycheck + CMake
;;          obsolete/me-flycheck-eglot ; Flycheck + Eglot
;;          obsolete/me-lexic          ; Offline dictionary using sdcv
;;          obsolete/me-ligature       ; Ligatures (needs further customization in function of the used font)
;;          obsolete/me-lsp            ; LSP and DAP (lsp-mode, dap-mode, consult-lsp, lsp-pyright, ccls, ...)
;;          obsolete/me-netextender    ; NetExtender integration (start/stop VPN sessions from Emacs)
;;          obsolete/me-org-present    ; Org presentations in Emacs
;;          obsolete/me-org-roam       ; Org roam configuration (org-roam, consult-org-roam, ...)
;;          obsolete/me-projectile     ; Project management (projectile, consult-projectile, treemacs-projectile, ...)
;;          obsolete/me-smartparens    ; Smartparens
;;          obsolete/me-spell-fu       ; Spell checking (included as an alternative when `jinx' cannot be used)
;;          obsolete/me-tree-sitter    ; Tree-sitter module configuration (this module is automatically activated for Emacs 28 or 29+ built without treesitter support)
;;          obsolete/me-unicode-fonts  ; Better Unicode management mainly for non-latin fonts
;;          obsolete/me-writeroom      ; Replacement for `+writing-mode' (writeroom-mode, ...)
;;          obsolete/me-yasnippet      ; Yasnippet (yasnippet, cape-yasnippet, yasnippet-snippets, ...)
;;         )))
