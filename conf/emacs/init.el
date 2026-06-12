(add-to-list 'default-frame-alist '(undecorated-round . t))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(tool-bar-lines . nil))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(defun my/apply-frame-transparency (&optional frame)
  "Apply macOS transparency parameters to FRAME (defaults to selected frame)."
  (with-selected-frame (or frame (selected-frame))
    (set-frame-parameter nil 'alpha-background 0.7)
    (set-frame-parameter nil 'ns-background-blur 30)
    (set-frame-parameter nil 'ns-alpha-elements '(ns-alpha-all))))

;; ns-background-blur must be in default-frame-alist to configure the
;; NSWindow backing material at frame creation time (required for blur).
;; This ensures emacsclient frames inherit it automatically.
(add-to-list 'default-frame-alist '(ns-background-blur . 30))
(add-to-list 'default-frame-alist '(ns-alpha-elements ns-alpha-all))

(add-hook 'after-make-frame-functions #'my/apply-frame-transparency)
(unless (daemonp)
  (add-hook 'window-setup-hook #'my/apply-frame-transparency))

;; Apply transparency immediately for non-daemon graphical startup,
;; where neither after-make-frame-functions nor window-setup-hook fires.
(when (display-graphic-p)
  (my/apply-frame-transparency))

(auto-compression-mode 1)

(let ((path (shell-command-to-string ". ~/.zshenv; . ~/.profile; echo -n $PATH")))
  (setenv "PATH" path)
  (setq exec-path
    (append
      (split-string-and-unquote path ":")
      exec-path)))

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

;; Move autosave and lockfiles to .emacs.d.
(let ((save-files-directory
        (file-name-concat user-emacs-directory
          "auto-save/")))
  (make-directory save-files-directory :parents)
  (setq auto-save-file-name-transforms
    `((".*" ,save-files-directory t))))

(let ((lock-files-directory
        (file-name-concat user-emacs-directory
          "lock-files/")))
  (make-directory lock-files-directory :parents)
  (setq lock-file-name-transforms
    `((".*" ,lock-files-directory t))))

(setq visual-bell 1)
(setq ring-bell-function 'ignore)
(setq frame-resize-pixelwise t)
(setq save-place-mode t)
(setq blink-cursor-mode nil)
(setq custom-safe-themes t)
(setq read-process-output-max (* 1024 1024))
(setq display-line-numbers-type 'relative)

(setq inhibit-startup-screen t
  inhibit-startup-echo-area-message (user-login-name))

(setq initial-major-mode 'fundamental-mode
  initial-scratch-message nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(set-face-background 'mouse "#ffffff")

(line-number-mode)
(column-number-mode)
(global-display-line-numbers-mode)
(electric-pair-mode)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(savehist-mode 1)
(add-to-list 'savehist-additional-variables #'vertico-repeat-history)

(add-to-list 'auto-mode-alist '("\\.svelte\\'" . mhtml-mode))

(setq
  js-indent-level 2
  c-default-style "k&r"
  c-basic-offset 2
  tcl-indent-level 2
  verilog-indent-level 2
  verilog-indent-level-declaration 2
  verilog-indent-level-directive 2
  verilog-indent-level-behavioral 2
  verilog-indent-level-module 2
  verilog-auto-newline nil
  verilog-indent-lists nil)

(setq-default indent-tabs-mode nil
  tab-width 2
  c-basic-offset 2)

(set-default 'semantic-case-fold t)

(setq-default show-trailing-whitespace t)
(dolist (hook '(special-mode-hook
                 term-mode-hook
                 comint-mode-hook
                 compilation-mode-hook
                 minibuffer-setup-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace nil))))

(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
          (replace-regexp-in-string
            "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
            crm-separator)
          (car args))
    (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(setq completion-cycle-threshold 3
  tab-always-indent 'complete)

;; Do not allow the cursor in the minibuffer prompt.
(setq minibuffer-prompt-properties
  '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate
  #'command-completion-default-include-p)

(setq enable-recursive-minibuffers t)

(setq sentence-end-double-space nil)

(prefer-coding-system 'utf-8)

(transient-mark-mode 1)
(global-hl-line-mode)

(setq scroll-step 1
  scroll-margin 7
  scroll-conservatively 100000)

(xterm-mouse-mode 1)

(setq select-enable-clipboard t
  select-enable-primary t
  save-interprogram-paste-before-kill t)

(setq mouse-yank-at-point t)

(defun set-background-for-terminal (&optional frame)
  (or frame (setq frame (selected-frame)))
  "Unsets the background color in terminal mode."
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)))

;; Set up wl-copy and wl-paste in terminal Emacs
(when (and (string= (getenv "XDG_SESSION_TYPE") "wayland")
        (executable-find "wl-copy")
        (executable-find "wl-paste"))
  (defun my-wl-copy (text)
    "Copy with wl-copy if in terminal, otherwise use the original value of `interprogram-cut-function'."
    (if (display-graphic-p)
      (gui-select-text text)
      (let ((wl-copy-process
              (make-process :name "wl-copy"
                :buffer nil
                :command '("wl-copy")
                :connection-type 'pipe)))
        (process-send-string wl-copy-process text)
        (process-send-eof wl-copy-process))))
  (defun my-wl-paste ()
    "Paste with wl-paste if in terminal, otherwise use the original value of `interprogram-paste-function'."
    (if (display-graphic-p)
      (gui-selection-value)
      (shell-command-to-string "wl-paste --no-newline")))
  (setq interprogram-cut-function #'my-wl-copy)
  (setq interprogram-paste-function #'my-wl-paste))

(add-hook 'after-make-frame-functions 'set-background-for-terminal)
(add-hook 'window-setup-hook 'set-background-for-terminal)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package bind-key
  :config
  (add-to-list 'same-window-buffer-names "*Personal Keybindings*"))

(use-package avy)
(use-package devdocs)
(use-package git-link)
(use-package git-timemachine)
(use-package haskell-mode)
(use-package julia-mode)
(use-package magit)
(use-package markdown-mode)
(use-package nix-mode)
(use-package prism)
(use-package rainbow-mode)
(use-package rust-mode)
(use-package smartparens)
(use-package solidity-mode)
(use-package symbol-overlay)
(use-package terraform-mode)
(use-package treemacs)
(use-package undo-fu)
(use-package vterm)
(use-package vundo)
(use-package wgrep)

(use-package bazel
  :config
  (add-to-list 'auto-mode-alist '("\\.bazel\\'" . bazel-starlark-mode))
  (add-to-list 'auto-mode-alist '("\\.bxl\\'" . bazel-starlark-mode)))

(use-package fpga
  :init
  (setq fpga-feture-list '(xilinx)))

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Cozette"))

(use-package typescript-mode
  :config
  (setq typescript-indent-level 2)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

(use-package emsg-blame
  :config
  (defun my--emsg-blame-display ()
    (posframe-show "*emgs-blame-posframe*"
      :string (format " %s\n %s\n %s " emsg-blame--commit-author emsg-blame--commit-date emsg-blame--commit-summary)
      :timeout 5
      :max-width 30
      :left-fringe 10
      :right-fringe 10
      :position (point)
      :poshandler #'posframe-poshandler-frame-top-right-corner
      :border-width 1
      :border-color "#5ccfe6"))
  (setq emsg-blame-display #'my--emsg-blame-display))
;; (global-emsg-blame-mode))

(use-package cape
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-sgml)
  (add-to-list 'completion-at-point-functions #'cape-rfc1345)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-elisp-symbol))

(use-package color-identifiers-mode
  :config
  (global-color-identifiers-mode))

(use-package consult
  :config
  (defun noct-consult-line-evil-history (&rest _)
    "Add latest `consult-line' search pattern to the evil search history ring.
     This only works with orderless and for the first component of the search."
    (when (and (bound-and-true-p evil-mode)
            (eq evil-search-module 'evil-search))
      (let ((pattern (cadr (orderless-compile (car consult--line-history)))))
        (add-to-history 'evil-ex-search-history pattern)
        (setq evil-ex-search-pattern (list pattern t t))
        (setq evil-ex-search-direction 'forward)
        (when evil-ex-search-persistent-highlight
          (evil-ex-search-activate-highlight evil-ex-search-pattern)))))

  (advice-add #'consult-line :after #'noct-consult-line-evil-history))

(use-package corfu
  :config
  (setq
    corfu-cycle t
    corfu-auto t
    corfu-quit-no-match t
    corfu-preselect 'directory)
  :bind (:map corfu-map ("RET" . nil))
  :init (global-corfu-mode))

(use-package copilot)

(use-package coterm
  :config
  (coterm-mode))

(use-package direnv
  :config
  (direnv-mode))

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package embark
  :after frames-only-mode
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  :config
  ;; Hide the mode line of the Embark live/completions buffers.
  (add-to-list 'display-buffer-alist
    '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
       nil
       (window-parameters (mode-line-format . none))))

  (add-to-list 'frames-only-mode-use-window-functions
    'embark-act))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package evil
  :init
  (setq evil-want-keybinding nil
    evil-want-Y-yank-to-eol t
    evil-search-wrap t
    evil-search-module 'evil-search
    evil-regexp-search t)
  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-mc
  :config
  (global-evil-mc-mode 1))

(use-package evil-snipe
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package frames-only-mode
  :config
  (frames-only-mode))

(use-package general
  :config
  (general-evil-setup t)

  (general-define-key
    :states 'motion
    :prefix "SPC"
    :keymaps 'override
    "SPC" 'save-buffer
    "g" 'magit
    "w" 'evil-window-map
    "p" 'projectile-command-map
    "e" 'eglot-mode-map
    "r" 'vertico-repeat
    "t" 'treemacs
    "s i" 'symbol-overlay-put
    "s n" 'symbol-overlay-switch-forward
    "s p" 'symbol-overlay-switch-backward
    "s m" 'symbol-overlay-mode
    "s x" 'symbol-overlay-remove-all
    "b b" 'consult-buffer
    "b e" 'eval-buffer
    "b k" 'kill-buffer
    "b l" 'list-buffers
    "/ f" 'find-file
    "/ r" 'consult-ripgrep
    "/ g" 'consult-git-grep
    "k f" 'describe-function
    "k v" 'describe-variable
    "k s" 'describe-symbol)

  (general-define-key
    :keymaps 'override
    "M-/" 'avy-goto-char-timer
    "C-." 'embark-act
    "M-." 'embark-dwim
    "M-s e" 'consult-isearch-history)

  (general-define-key
    :keymaps 'isearch-mode-map
    "M-e" 'consult-isearch-history
    "M-s l" 'consult-line
    "M-s L" 'consult-line-multi)

  (general-def 'normal
    "u" 'undo-fu-only-undo
    "C-r" 'undo-fu-only-redo
    "/" 'consult-line)

  (general-def 'visual
    "A" 'evil-mc-make-cursor-in-visual-selection-end
    "I" 'evil-mc-make-cursor-in-visual-selection-beg))

(use-package highlight-thing
  :config
  (global-highlight-thing-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(use-package indent-guide
  :config
  (indent-guide-global-mode))

(use-package literate-calc-mode
  :config
  (setq literate-calc-mode-idle-time 0.1)
  (literate-calc-mode))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package magit-todos
  :config
  (magit-todos-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
    completion-category-defaults nil
    completion-category-overrides '((file (styles partial-completion)))))

(use-package projectile
  :config
  (projectile-mode))

(use-package rainbow-delimiters
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package smex
  :config
  (smex-initialize))

(use-package smooth-scrolling
  :config
  (setq smooth-scrolling-margin 5)
  (smooth-scrolling-mode))

(use-package spacious-padding
  :config
  (spacious-padding-mode))

(use-package treesit-auto)

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once))

(use-package undo-fu-session
  :config
  (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
  (global-undo-fu-session-mode))

(use-package vertico
  :config
  (vertico-mode))

;; (use-package vertico-grid)
;; (use-package vertico-indexed)
;; (use-package vertico-quick)

;; (use-package vertico-mouse
;;   :config
;;   (vertico-mouse-mode))

;; (use-package vertico-repeat
;;   :config
;;   (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))

(use-package which-key
  :config
  (setq which-key-idle-delay 0.01)
  (which-key-mode))

(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode))

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)
