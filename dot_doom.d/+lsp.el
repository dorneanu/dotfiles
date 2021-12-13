;;; +lsp.el -*- lexical-binding: t; -*-

(use-package lsp-mode
  :config
  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet t  ;; Not supported by company capf, which is the recommended company backend
        lsp-pyls-plugins-flake8-enabled t)
  :hook (
         (python-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         ;; (lsp-mode . lsp-headerline-breadcrumb-mode)
         ;; (lsp-mode . lsb-enable-which-key-integration))
         )
  :commands (lsp lsp-deferred)
  :custom
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  (lsp-prefer-flymake t)
)

;; optional - provides fancy overlay information
(use-package lsp-ui
  :ensure t
  :config
  (setq
        ;; lsp-ui-sideline-show-hover t
        ;; lsp-ui-sideline-delay 0.5
        ;; lsp-ui-sideline-ignore-duplicates t
        lsp-ui-sideline-show-hover nil
        lsp-ui-doc-delay 0.5
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-alignment 'frame
        lsp-ui-doc-header nil
        lsp-ui-doc-include-signature t
        lsp-ui-doc-use-childframe t)
  :commands lsp-ui-mode
  :custom
  ;; lsp-ui-doc
  (lsp-ui-doc-enable nil)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature nil)
  (lsp-ui-doc-position 'top) ;; top, bottom, or at-point
  (lsp-ui-doc-max-width 120)
  (lsp-ui-doc-max-height 30)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  :bind
  (:map lsp-ui-mode-map
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references)
              ("C-c u" . lsp-ui-imenu)
              ("C-c d" . lsp-ui-doc-glance)
              ("C-c x" . counsel-flycheck))
)
