;; directories ;;
(setq user-emacs-directory (file-truename "~/.local/share/emacs"))
(defvar --backup-directory "~/.cache/emacs/backups")

;; window ;;
(load-theme 'gruvbox-dark-hard t)

(setq
 menu-bar-mode nil
 tool-bar-mode nil
 scroll-bar-mode nil
 global-hl-line-mode nil
 column-number-mode t
 xterm-mouse-mode t
 save-place-mode t
 visible-bell t)

(global-display-line-numbers-mode)
(frames-only-mode)

;; code ;;
(setq
 standard-indent 2
 indent-line-function 'insert-tab)

(setq-default
 indent-tabs-mode nil
 tab-width 2)

(setq
 js-indent-level 2
 c-default-style "k&r"
 c-basic-offset 2
 verilog-auto-newline nil)

(set-default 'semantic-case-fold t)

;(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

;; backups ;;
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

;; evil ;;
(setq
 evil-want-keybinding nil
 evil-want-Y-yank-to-eol t
 evil-search-wrap t
 evil-regexp-search t
 evil-undo-system 'undo-fu)
(evil-mode)
(evil-collection-init)

;; general.el ;;
(general-create-definer my-leader-def
  :prefix "SPC")

(general-create-definer my-local-leader-def
  :prefix "SPC m")

(general-evil-setup)

(my-local-leader-def
 :states 'motion
 :keymaps 'override
 "SPC" 'save-buffer
 "c" 'counsel-git
 "g" 'magit
 "l" 'lsp-command-map
 "r" 'ivy-resume
 "w" 'evil-window-map
 "p" 'projectile-command-map
 "b b" 'ivy-switch-buffer
 "b e" 'eval-buffer
 "b k" 'kill-buffer
 "b l" 'list-buffers
 "t i" 'ivy-mode
 "/ a" 'counsel-ag
 "/ c" 'avy-goto-char-2
 "/ f" 'find-file
 "/ g" 'counsel-git-grep
 "/ l" 'find-library
 "k f" 'describe-function
 "k s" 'describe-symbol
 "k v" 'describe-variable
 "x m" 'lsp-ui-imenu)

(general-define-key
 "M-/" 'avy-goto-char)

(general-def 'normal
  "u" 'undo-fu-only-undo
  "C-r" 'undo-fu-only-redo
  "/" 'swiper)

(general-def 'normal lsp-mode-map
  "K" 'lsp-describe-thing-at-point)

;; swiper/ivy/counsel ;;
(ivy-count-format "(%d/%d) ")
(ivy-use-virtual-buffers t)
(enable-recursive-minibuffers t)

(ivy-virtual-abbreviate 'full
                        ivy-rich-switch-buffer-align-virtual-buffer t
                        ivy-rich-path-style 'abbrev)

(ivy-set-display-transformer 'ivy-switch-buffer
                             'ivy-rich-switch-buffer-transformer)

(counsel-mode)
(ivy-mode)

(setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
(global-undo-fu-session-mode)

(yas-global-mode)

(setq smooth-scrolling-margin 5)
(smooth-scrolling-mode)

(setq projectile-completion-system 'ivy)
(projectile-mode)

(setq which-key-idle-delay 0.01)
(which-key-mode)

(add-hook 'after-init-hook #'global-company-mode)

(setq lsp-keymap-prefix "C-c l")
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

(add-hook 'c++-mode-hook #'lsp-deferred)
(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'haskell-mode-hook #'lsp-deferred)
(add-hook 'html-mode-hook #'lsp-deferred)
(add-hook 'js-mode-hook #'lsp-deferred)
(add-hook 'python-mode-hook #'lsp-deferred)
(add-hook 'rust-mode-hook #'lsp-deferred)
(add-hook 'typescript-mode-hook #'lsp-deferred)
(add-hook 'verilog-mode-hook #'lsp-deferred)

(setq lsp-eslint-package-manager "yarn")
(setq lsp-lens-enable t)
(setq lsp-modeline-code-actions-enable nil)
(advice-add 'lsp :before #'direnv-update-environment)

(setq lsp-ui-doc-position :bottom)

(treemacs-icons-dired-mode)

(add-hook 'meson-mode-hook 'company-mode)

(global-hl-todo-mode)

(add-hook 'prog-mode-hook #'direnv-update-environment)
(direnv-mode)

(magit-mode)
(magit-delta-mode)
(magit-todos-mode)
