#!/bin/bash

emacs -q --batch --eval "(require 'ob-tangle)" \
  --eval "(setq org-confirm-babel-evaluate nil)" \
  --eval "(org-babel-tangle-file \"$1\")"
