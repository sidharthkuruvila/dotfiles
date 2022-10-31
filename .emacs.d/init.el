(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

;; Set up the visible bell
(setq visible-bell t)

(column-number-mode)
(global-display-line-numbers-mode t)

(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; ANSI color in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Some key bindings

(global-set-key [f3] 'next-match)
(defun prev-match () (interactive nil) (next-match -1))
(global-set-key [(shift f3)] 'prev-match)
(global-set-key [backtab] 'auto-complete)

;; -- Tweaks for OS X -------------------------------------
;; Tweak for problem on OS X where Emacs.app doesn't run the right
;; init scripts when invoking a sub-shell
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to
  match that used by the user's shell.

This is particularly useful under Mac OSX, where GUI apps are not
started from a shell."
  (interactive)
  (let ((path-from-shell
         (replace-regexp-in-string
          "[ \t\n]*$" ""
          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))
         ))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator)))
  )

(set-exec-path-from-shell-PATH)

(server-start)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("non-gnu-elpa" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package diminish)

(when (file-directory-p "/opt/homebrew/share/emacs/site-lisp/asymptote/")
  (add-to-list 'load-path "/opt/homebrew/share/emacs/site-lisp/asymptote/")
  (autoload 'asy-mode "asy-mode.el" "Asymptote major mode." t)
  (autoload 'lasy-mode "asy-mode.el" "hybrid Asymptote/Latex major mode." t)
  (autoload 'asy-insinuate-latex "asy-mode.el" "Asymptote insinuate LaTeX." t)
  (add-to-list 'auto-mode-alist '("\\.asy$" . asy-mode)))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :config
  ;; Enabling inline images with the default emacs scroll behaviour creates a bad experience
  ;; Emacs scrolls a line at a time. It treats the image height as the line height which means
  ;; that scrolling over an image will cause to to jump to show the entire image.
  ;; Emacs has two approaches to solve this. insert-sliced-image, which splits an image into multiple lines,
  ;; it is not clear to me whether insert-sliced-image can be used with org mode. The second alternative
  ;; is pixel based scrolling, which does not appear to be popular in current versions of emacs.
  ;; Emacs 29 apprantly has a better pixel based scrolling. I need to do a bit of research before I can enable
  ;; inline images.
  ;;
  ;; Things to consider
  ;; - What are the performance and user experience issues with pixel based scrolling
  ;; - Can org mode be made to use sliced images
  ;; - Would enabling inline images only for roam notes be acceptable.

  ;(setq org-startup-with-inline-images t)
  :pin org
  :hook (org-mode . efs/org-mode-setup))

(use-package org-contrib)

(setq org-babel-python-command "python3")

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (ocaml . t)
   (dot . t)
   (emacs-lisp . t)
   (asymptote . t)))

(setq org-confirm-babel-evaluate nil)

(use-package org-journal
  :config
  (setq org-journal-dir "~/Documents/Notes/Journal")
  (setq org-journal-file-format "%Y%m%d.org"))

(message "using package org-roam")
(use-package org-roam
  :custom
  (org-roam-directory "~/Documents/Notes/Roam")
  :bind (("C-c n l"  . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup)
  (org-roam-db-autosync-mode))
(message "done setting up org-roam")

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package compat)

;; Use the doom modeline
;; https://github.com/seagle0128/doom-modeline
(use-package doom-modeline
  :init (doom-modeline-mode t))

(use-package all-the-icons
:ensure t)

;; Use doom themes
;; https://github.com/doomemacs/themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
:commands (helpful-callable helpful-variable helpful-command helpful-key)
:custom
(counsel-describe-function-function #'helpful-callable)
(counsel-describe-variable-function #'helpful-variable)
:bind
([remap describe-function] . counsel-describe-function)
([remap describe-command] . helpful-command)
([remap describe-variable] . counsel-describe-variable)
([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :init 
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-project-search-path `("~/Documents/Notes" "~/code" "~/dotfiles")))

;; OCaml configuration
;;  - better error and backtrace matching

(defun set-ocaml-error-regexp ()
  (set
   'compilation-error-regexp-alist
   (list '("[Ff]ile \\(\"\\(.*?\\)\", line \\(-?[0-9]+\\)\\(, characters \\(-?[0-9]+\\)-\\([0-9]+\\)\\)?\\)\\(:\n\\(\\(Warning .*?\\)\\|\\(Error\\)\\):\\)?"
           2 3 (5 . 6) (9 . 11) 1 (8 compilation-message-face)))))

(add-hook 'tuareg-mode-hook 'set-ocaml-error-regexp)
(add-hook 'caml-mode-hook 'set-ocaml-error-regexp)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
