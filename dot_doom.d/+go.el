;;; +go.el -*- lexical-binding: t; -*-

(setq lsp-gopls-staticcheck t)
(setq lsp-eldoc-render-all t)
(setq lsp-gopls-complete-unimported t)

(use-package go-mode
  :mode "\\.go\\'"
  :custom (gofmt-command "goimports")
  :bind (:map go-mode-map
         ("C-c C-n" . go-run)
         ("C-c ."   . go-test-current-test)
         ("C-c f"   . go-test-current-file)
         ("C-c a"   . go-test-current-project))
  :hook
  (
        (go-mode . lsp-deferred)
        (go-mode . go-guru-hl-identifier-mode)
        (go-mode . go-eldoc-setup)
  )
  :config
        (use-package gotest)
        (use-package go-guru)
        (use-package flycheck-golangci-lint)
        (use-package go-tag
          :config (setq go-tag-args (list "-transform" "camelcase")))
)

;; Go - lsp-mode
;; Set up before-save hooks to format buffer and add/delete imports.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Start LSP Mode and YASnippet mode
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'yas-minor-mode)

;; ;; flycheck-golang-ci-lint
;; (use-package flycheck-golangci-lint
;;   :ensure t
;;   :hook
;;   (go-mode . flycheck-golangci-lint-setup)
;;   )

;; (add-hook 'lsp-after-initialize-hook (lambda
;;                                        ()
;;                                        (flycheck-add-next-checker 'lsp 'golangci-lint)))
