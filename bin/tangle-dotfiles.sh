#!/bin/sh

emacs --batch \
      --eval "(require 'ob-tangle)" \
      --eval "(org-babel-tangle-file \"$HOME/dotfiles/Emacs.org\")"
