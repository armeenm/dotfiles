(setq user-emacs-directory (file-truename "~/.local/share/emacs"))
(defvar --backup-directory "~/.cache/emacs/backups")

(if (not (file-exists-p --backup-directory))
  (make-directory --backup-directory t))
(setq backup-directory-alist `(("." . ,--backup-directory)))
(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      delete-by-moving-to-trash t
      kept-old-versions 6
      kept-new-versions 9
      auto-save-default t
      auto-save-timeout 20
      auto-save-interval 200)

(setq
  gc-cons-threshold most-positive-fixnum
  visual-bell 0)

(setq
  menu-bar-mode nil
  tool-bar-mode nil
  toggle-scroll-bar nil
  global-hl-line-mode nil
  column-number-mode t
  xterm-mouse-mode t
  save-place-mode t)

(global-display-line-numbers-mode)

(setq
  standard-indent 2
  js-indent-level 2
  c-default-style "k&r"
  c-basic-offset 2
  verilog-auto-newline nil)

(setq
  indent-line-function 'insert-tab)

(setq-default
  indent-tabs-mode nil
  tab-width 2)

(set-default 'semantic-case-fold t)

(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(defconst my-leader "SPC")
(defconst my-local-leader "SPC m")
(defconst my-lisp-modes '(clojure-mode emacs-lisp-mode common-lisp-mode scheme-mode))

(setq evil-want-keybinding nil
      evil-want-Y-yank-to-eol t
      evil-search-wrap t
      evil-regexp-search t)
      ; evil-undo-system 'undo-fu)

(evil-mode)

;(set-face-attribute 'default nil
;                    :family "Tamsyn"
;                    :height 120
;                    :weight 'normal
;                    :width 'normal)


;;; straight.el ;;
;(defvar bootstrap-version)
;(let ((bootstrap-file
;       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;      (bootstrap-version 5))
;  (unless (file-exists-p bootstrap-file)
;    (with-current-buffer
;        (url-retrieve-synchronously
;         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;         'silent 'inhibit-cookies)
;      (goto-char (point-max))
;      (eval-print-last-sexp)))
;  (load bootstrap-file nil 'nomessage))
;
;(setq straight-disable-native-compilation t)
;
;;; use-package ;;
;(setq
; straight-use-package-by-default t
; use-package-always-ensure t)
;
;(straight-use-package 'use-package)
;(require 'use-package-ensure)
;
;;; general.el ;;
;(use-package general
;  :config
;  (general-create-definer my-leader-def
;    :prefix my-leader)
;
;  (general-create-definer my-local-leader-def
;    :prefix my-local-leader)
;
;  (my-leader-def
;    :states 'motion
;    :keymaps 'override
;    "SPC" 'save-buffer
;    "g" 'magit
;    "w" 'evil-window-map
;    "l" 'lsp-command-map
;    "r" 'ivy-resume
;    "c" 'counsel-git
;    "p" 'projectile-command-map
;    "b b" 'ivy-switch-buffer
;    "b e" 'eval-buffer
;    "b k" 'kill-buffer
;    "b l" 'list-buffers
;    "t i" 'ivy-mode
;    "/ c" 'avy-goto-char-2
;    "/ f" 'find-file
;    "/ l" 'find-library
;    "/ a" 'counsel-ag
;    "/ g" 'counsel-git-grep
;    "k f" 'describe-function
;    "k v" 'describe-variable
;    "k s" 'describe-symbol
;    "x m" 'lsp-ui-imenu)
;  
;  (general-define-key
;   "M-/" 'avy-goto-char)
;  
;  (general-def 'normal
;    "u" 'undo-fu-only-undo
;    "C-r" 'undo-fu-only-redo
;    "/" 'swiper)
;
;  (general-def 'normal lsp-mode-map
;    "K" 'lsp-describe-thing-at-point))
;
;(use-package frames-only-mode
;  :config
;  (frames-only-mode))
;
;;; swiper/ivy/counsel ;;
;(use-package ivy
;  :diminish
;  :custom
;  (ivy-count-format "(%d/%d) ")
;  (ivy-use-virtual-buffers t)
;  (enable-recursive-minibuffers t)
;  :config (ivy-mode))
;
;(use-package ivy-rich
;  :after ivy
;  :custom
;  (ivy-virtual-abbreviate 'full
;                          ivy-rich-switch-buffer-align-virtual-buffer t
;                          ivy-rich-path-style 'abbrev)
;  :config
;  (ivy-set-display-transformer 'ivy-switch-buffer
;                               'ivy-rich-switch-buffer-transformer))
;
;(use-package counsel
;  :after ivy
;  :config (counsel-mode))
;
;(use-package swiper
;  :after ivy)
;
;;; yasnippet ;;
;(use-package yasnippet
;  :config (yas-global-mode))
;(use-package yasnippet-snippets)
;
;(use-package avy)
;
;(use-package undo-fu)  
;(use-package undo-fu-session
;  :config
;  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
;  (global-undo-fu-session-mode))
;
;;; evil ;;
;(use-package evil
;  :init (setq evil-want-keybinding nil
;              evil-want-Y-yank-to-eol t
;              evil-search-wrap t
;              evil-regexp-search t
;              evil-undo-system 'undo-fu)
;  :config (evil-mode))
;
;(use-package evil-collection
;  :after evil
;  :ensure t
;  :config (evil-collection-init))
;
;;; elisp-slime-nav ;;
;(use-package elisp-slime-nav
;  :hook (turn-on-elisp-slime-nav-mode))
;
;(use-package dracula-theme
;  :config
;  (load-theme 'dracula t))
;
;(use-package lispy
;  :commands lispy-mode
;  :config (defun enable-lispy-mode () (lispy-mode 1))
;  :hook (my-lisp-modes . enable-lispy-mode))
;
;(use-package lispyville
;  :commands lispyville-mode
;  :hook (lispy-mode . lispyville-mode)
;  :config (lispyville-set-key-theme '(operators c-w additional)))
;
;(use-package smooth-scrolling
;  :config
;  (smooth-scrolling-mode)
;  (setq smooth-scrolling-margin 5))
;
;(use-package auto-package-update
;  :config
;  (auto-package-update-maybe)
;  (progn
;    (add-hook 'auto-package-update-before-hook
;              (lambda () (message "Updating packages...")))))
;
;(use-package projectile
;  :config
;  (setq projectile-completion-system 'ivy)
;  (projectile-mode))
;
;(use-package which-key
;  :config
;  (which-key-mode)
;  (setq which-key-idle-delay 0.01))
;
;(use-package deadgrep)
;
;(use-package smex
;  :config
;  (smex-initialize))
;
;(use-package company
;  :config
;  (progn
;    (add-hook 'after-init-hook 'global-company-mode)))
;
;(use-package lsp-mode
;  :init (setq lsp-keymap-prefix "C-c l")
;  :after (direnv evil)
;  :hook ((c++-mode . lsp-deferred)
;         (c-mode . lsp-deferred)
;         (vhdl-mode . lsp-deferred)
;         (verilog-mode . lsp-deferred)
;         (haskell-mode . lsp-deferred)
;         (typescript-mode . lsp-deferred)
;         (haskell-literate-mode . lsp-deferred)
;         (python-mode . lsp-deferred)
;         (js-mode . lsp-deferred)
;         (html-mode . lsp-deferred)
;         (rust-mode . lsp-deferred)
;         (lsp-mode . lsp-enable-which-key-integration))
;  :config
;  (setq lsp-eslint-package-manager "yarn")
;  (setq lsp-lens-enable t)
;  (setq lsp-modeline-code-actions-enable nil)
;  (advice-add 'lsp :before #'direnv-update-environment)
;  :commands lsp lsp-deferred)
;
;(use-package lsp-ui
;  :hook (prog-mode . lsp-ui-mode)
;  :config (setq lsp-ui-doc-position :bottom))
;
;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;
;(use-package lsp-haskell
;  :config
;  (setq lsp-haskell-formatting-provider "ormolu")
;  :init
;  (add-hook 'haskell-mode-hook #'lsp)
;  (add-hook 'haskell-literate-mode-hook #'lsp))
;
;(use-package dap-mode)
;
;(use-package treemacs)
;(use-package treemacs-evil
;  :after (treemacs evil))
;(use-package treemacs-projectile
;  :after (treemacs projectile))
;(use-package treemacs-icons-dired
;  :after (treemacs dired)
;  :config (treemacs-icons-dired-mode))
;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;
;(use-package all-the-icons)
;(use-package all-the-icons-ivy
;  :config
;  (all-the-icons-ivy-setup))
;
;(use-package clojure-mode)
;(use-package solidity-mode)
;(use-package haskell-mode)
;(use-package typescript-mode
;  :mode "\\.mts\\'")
;(use-package nix-mode
;  :mode "\\.nix\\'")
;(use-package rust-mode)
;(use-package meson-mode
;  :init
;  (add-hook 'meson-mode-hook 'company-mode))
;
;(use-package cloc)
;(use-package hl-todo
;  :config (global-hl-todo-mode))
;
;(use-package direnv
;  :init (add-hook 'prog-mode-hook #'direnv-update-environment)
;  :config (direnv-mode))
;
;(use-package magit)
;(use-package magit-delta
;  :hook (magit-mode . magit-delta-mode))
;(use-package magit-todos
;  :config (magit-todos-mode))
;
;(with-eval-after-load 'ox-latex
;  (add-to-list 'org-latex-classes
;               '("org-plain-latex"
;                 "\\documentclass{article}
;[NO-DEFAULT-PACKAGES]
;[PACKAGES]
;[EXTRA]"
;                 ("\\section{%s}" . "\\section*{%s}")
;                 ("\\subsection{%s}" . "\\subsection*{%s}")
;                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
;                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
;                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
