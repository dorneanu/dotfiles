;;; Directory Local Variables            -*- no-byte-compile: t -*-
;;; For more information see (info "(emacs) Directory Variables")

((org-mode . ((org-confirm-babel-evaluate . nil)
              (eval .
                    (progn
                      ;; Tangle whole file after being saved
                      (add-hook 'after-save-hook 'org-babel-tangle t t)
                      )))))
