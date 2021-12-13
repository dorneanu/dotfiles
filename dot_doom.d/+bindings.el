;;; +bindings.el -*- lexical-binding: t; -*-


(global-set-key (kbd "C-RET") 'org-insert-subheading)

;; https://github.com/hlissner/doom-emacs/issues/906
(map! (:when IS-MAC
  (:map general-override-mode-map
   :gi "M-v" #'yank)))

(map!
        ;; Ensure there are no conflicts
        :nmvo doom-leader-key nil
        :nmvo doom-localleader-key nil

        ;; Window Movements
        "C-h"    #'evil-window-left
        "C-j"    #'evil-window-down
        "C-k"    #'evil-window-up
        "C-l"    #'evil-window-right

        ;; elfeed
        "C-x w"  #'elfeed

        ;; find file
        :nmvo "C-p"    #'counsel-projectile-find-file

        ;; fzf


        ;; Leader Configs
        ;; From https://github.com/rschmukler/doom.d/blob/master/%2Bbindings.el
        (:leader
         :desc "Ivy open buffers"       :n  "b"   #'ivy-switch-buffer
         (:desc "insert" :prefix "i"
          :desc "From kill-ring" :nv "p" #'counsel-yank-pop
          :desc "From snippet"   :nv "s" #'yas-insert-snippet)
         :desc "Open like spacemacs" "SPC" #'counsel-M-x
         :desc "Run counsel-fzf"  :n  "of"   #'counsel-fzf
         :desc "Run counsel-ag"  :n  "ou"   #'counsel-ag
         )
)

;; pocket-reader
(map! :map pocket-reader-mode-map
      :after pocket-reader
      :nm "d" #'pocket-reader-delete
      :nm "a" #'pocket-reader-toggle-archived
      :nm "B" #'pocket-reader-open-in-external-browser
      :nm "TAB" #'pocket-reader-open-url
      :nm "tr" #'pocket-reader-remove-tags
      :nm "tN" #'dorneanu/pocket-reader-remove-next
      :nm "C-b" #'dorneanu/tiddlywiki-add-bookmark
      :nm "ta" #'pocket-reader-add-tags
      :nm "gr" #'pocket-reader-refresh
      :nm "p" #'pocket-reader-search
      :nm "U" #'pocket-reader-unmark-all
      :nm "y" #'pocket-reader-copy-url
      :nm "Y" #'dorneanu/pocket-reader-copy-to-scratch)

;; elfeed
;; from https://tecosaur.github.io/emacs-config/config.html
(map! :map elfeed-search-mode-map
      :after elfeed-search
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :n "q" #'+rss/quit
      :n "e" #'elfeed-update
      :n "r" #'elfeed-search-untag-all-unread
      :n "u" #'elfeed-search-tag-all-unread
      :n "s" #'elfeed-search-live-filter
      :n "RET" #'elfeed-search-show-entry
      :n "p" #'elfeed-show-pdf
      :n "+" #'elfeed-search-tag-all
      :n "-" #'elfeed-search-untag-all
      :n "S" #'elfeed-search-set-filter
      :n "b" #'elfeed-search-browse-url
      :n "B" #'ap/elfeed-search-browse-org
      :n "a" #'pocket-reader-elfeed-search-add-link
      :n "y" #'elfeed-search-yank)
(map! :map elfeed-show-mode-map
      :after elfeed-show
      [remap kill-this-buffer] "q"
      [remap kill-buffer] "q"
      :n doom-leader-key nil
      :nm "q" #'+rss/delete-pane
      :nm "a" #'pocket-reader-elfeed-entry-add-link
      :n "B" #'ap/elfeed-search-browse-entry
      :nm "o" #'ace-link-elfeed
      :nm "RET" #'org-ref-elfeed-add
      :nm "n" #'elfeed-show-next
      :nm "N" #'elfeed-show-prev
      :nm "p" #'elfeed-show-pdf
      :nm "+" #'elfeed-show-tag
      :nm "-" #'elfeed-show-untag
      :nm "s" #'elfeed-show-new-live-search
      :nm "y" #'elfeed-show-yank)
