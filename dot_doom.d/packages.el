;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.
;;
;; disabled packages
(disable-packages! solaire-mode
                   realgud
                   realgud-trepan-ni
                   anaconda-mode
                   snipe
                   evil-snipe
                   company-anaconda
                   lsp-python-ms
                   pyimport)

(package! python-black)
(package! material-theme)
;; (package! farmhouse-theme)
(package! berrys-theme)
(package! ox-hugo)
;; (package! go-autocomplete)
(package! org-super-agenda)
(package! org-transclusion)
(package! org-bars :recipe (:host github :repo "tonyaldon/org-bars" ))
(package! org-ql)
(package! org-journal)
(package! ranger)
;; (package! lsp-python-ms :disable t)
(package! ob-go)
(package! ob-http)
;;(package! helm-org-rifle)
;; (package! helm-org)
;;(package! helm-flx)
;; (package! swiper-helm)
(package! org-cliplink)
(package! org-web-tools)
(package! shrface)
(package! org-bullets)
(package! pocket-reader)
(package! impatient-mode)
(package! org-re-reveal)
(package! org-fc :recipe (:host github :repo "l3kn/org-fc" :files (:defaults "awk" "demo.org")))
;; (package! emacs-reveal)
(package! org-transform-tree-table)
(package! ox-reveal)
(package! epresent)
(package! org-present)
(package! company)
(package! company-mode)
(package! company-lsp)
(package! company-go)
(package! company-postframe)
(package! company-box)
(package! verb)
(package! tiddlywiki-mode :recipe
  (:host github :repo "whacked/tiddlywiki-org"))
(package! ox-tiddly)
(package! ox-slack)
;; (package! org8-wikiexporters :recipe
;;   (:host github :repo "dfeich/org8-wikiexporters"))
(package! elfeed-goodies)
(package! elfeed-tube :recipe (:host github :repo "karthink/elfeed-tube"))
(package! mpv)
(package! pinentry)
;; (package! xeft :recipe (:host github :repo "casouri/xeft"))
(package! hnreader)
(package! reddigg)
(package! howdoyou)
(package! go-fill-struct)
(package! flycheck-golangci-lint)
(package! rg)
(package! gotest)
(package! lsp-mode)
(package! counsel)
(package! counsel-projectile)
(package! consult-org-roam)
(package! chatgpt
  :recipe (:host github :repo "joshcho/ChatGPT.el" :files ("dist" "*.el")))
(package! wrap-region)
;; (package! jupyter)
(package! ox-pandoc)
(package! org-pandoc-import
  :recipe (:host github
           :repo "tecosaur/org-pandoc-import"
           :files ("*.el" "filters" "preprocessors")))
(package! ace-link)
;; (package! org-roam
;;   :recipe (:host github :repo "org-roam/org-roam"
;;            :files (:defaults "extensions/*")))
;; (package! org-roam)
(package! yaml-pro :recipe (:host github :repo "zkry/yaml-pro"))
(package! leo)
(package! agitate)
(package! wordreference :recipe (:host codeberg :repo "martianh/wordreference.el"))
(package! powerthesaurus)
(package! flyspell-correct)
(package! ox-jira)
(package! ox-slack)
(package! org-autolist)
(package! xclip)
(package! eink-theme)
(package! color-theme-sanityinc-tomorrow)
(package! focus)
(package! vterm)
(package! autorevert)
(package! doom-modeline)
(package! diredfl)
(package! dired-subtree)
(package! dired-sidebar)
(package! dired-narrow)
(package! dired-open)
(package! dired-git-info)
(package! dirvish)
;; (package! all-the-icons-dired)
(package! dimmer)
(package! diminish)
(package! zoom-window)
(package! jiralib2 :recipe (:host github :repo "nyyManni/jiralib2"))
(package! ejira :recipe (:host github :repo "nyyManni/ejira"))
;; (package! ol-notmuch)
;; (package! md4rd
;;   :recipe (:horst github :repo "ahungry/md4rd"))
;;(package! emacs-w3m)
(package! wakatime-mode)
(package! dap-mode)
(package! dank-mode :recipe (:host github :repo "john2x/dank-mode"))
(package! dash-functional)
(package! lsp-treemacs)
;; (package! treemacs-all-the-icons)
(package! tldr)

(package! lsp-pyright)
(package! google-translate)
(package! dictcc)
(package! solidity-mode)

;; asciidoc related
;;(package! adoc-mode)
;;(package! org-asciidoc :recipe (:host github :repo "yashi/org-asciidoc" :branch "asciidoctor-diagram"))

;; (package! nano-emacs :recipe (:host github :repo "rougier/nano-emacs")
;; (package! nano-emacs :recipe (:host github :repo "rougier/nano-emacs"))
;; (package! nano-sidebar :recipe (:host github :repo "rougier/nano-sidebar"))
(package! nano-theme :recipe (:host github :repo "rougier/nano-theme"))
(package! github-theme)
(package! kaolin-themes)
(package! ef-themes :recipe (:host github :repo "protesilaos/ef-themes"))
(package! poet-theme)
(package! iedit)
(package! paredit)
;; (package! undo-tree)
(package! vundo)
(package! dropbox)
(package! transpose-frame)

(package! wgrep)
(package! ag)
(package! wgrep-ag)
(package! gnuplot)
(package! screenshot :recipe (:host github :repo "tecosaur/screenshot"))
(package! emacs-gif-screencast :recipe (:host gitlab :repo "ambrevar/emacs-gif-screencast"))
(package! evil-commentary)
(package! evil-quickscope)
(package! evil-numbers)
(package! evil-lion)
(package! evil-matchit)
(package! evil-surround)
(package! org-download)
(package! yasnippet-snippets)
(package! yankpad)
(package! ivy-prescient)
(package! parinfer)
(package! prettier)
(package! prettier-js)
(package! lsp-tailwindcss :recipe (:host github :repo "merrickluo/lsp-tailwindcss"))
(package! vue-mode)
(package! olivetti)
(package! keycast)
(package! nnreddit)
(package! copy-as-format)
;; (package! nov)
(package! ereader)
(package! ein)
;; (package! pomm :recipe (:host github :repo "SqrtMinusOne/pomm.el" :files (:defaults "resources")))
;; (package! lsp-volar :recipe (:host github :repo "jadestrong/lsp-volar"))
;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
