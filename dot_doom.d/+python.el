;;; +python.el -*- lexical-binding: t; -*-

(use-package! python-black
  :demand t
  :after python)
(add-hook! 'python-mode-hook #'python-black-on-save-mode)

(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode))

;; Add keybindings for testing in python mode
(map! :map python-mode-map
        :localleader
        :prefix "t"
        :nv "f" #'python-pytest-file
        :nv "t" #'python-pytest-function
        :nv "k" #'python-pytest-file-dwim
        :nv "r" #'python-pytest-repeat
        :nv "p" #'python-pytest-popup)
