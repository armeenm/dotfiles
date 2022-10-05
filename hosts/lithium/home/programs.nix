{ config, pkgs, lib, root, user, ... }:

{
  aria2.enable = true;
  dircolors.enable = true;
  fzf.enable = true;
  home-manager.enable = true;
  nix-index.enable = true;
  noti.enable = true;
  zathura.enable = true;
  zoxide.enable = true;

  bash = {
    enable = true;
    historyFile = "${config.xdg.cacheHome}/bash/history";
  };

  bat = {
    enable = true;
    config = {
      theme = "Dracula";
    };
    themes = {
      dracula = builtins.readFile (pkgs.fetchFromGitHub
        {
          owner = "dracula";
          repo = "sublime";
          rev = "26c57ec282abcaa76e57e055f38432bd827ac34e";
          sha256 = "019hfl4zbn4vm4154hh3bwk6hm7bdxbr1hdww83nabxwjn99ndhv";
        } + "/Dracula.tmTheme");
    };
  };

  broot = {
    enable = true;
    settings = {
      modal = true;
    };
  };

  direnv = {
    enable = true;
    nix-direnv.enable = true;
  };

  emacs = {
    enable = true;
    package = pkgs.emacsPgtkNativeComp;

    init = {
      enable = true;
      packageQuickstart = false;
      recommendedGcSettings = true;
      usePackageVerbose = false;

      earlyInit = ''
        (auto-compression-mode 1)
        (push '(menu-bar-lines . 0) default-frame-alist)
        (push '(tool-bar-lines . nil) default-frame-alist)
        (push '(vertical-scroll-bars . nil) default-frame-alist)

        (setq frame-title-format "")

        (set-face-attribute 'default nil
                            :family "Tamsyn"
                            :height 120
                            :weight 'normal
                            :width 'normal)
      '';

      prelude = ''
        (setq gc-cons-threshold most-positive-fixnum)

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

        (setq visual-bell 1)

        (setq save-place-mode t)

        (setq inhibit-startup-screen t
              inhibit-startup-echo-area-message (user-login-name))

        (setq initial-major-mode 'fundamental-mode
              initial-scratch-message nil)

        (setq blink-cursor-mode nil)

        (setq custom-safe-themes t)

        (set-face-background 'mouse "#ffffff")

        (defalias 'yes-or-no-p 'y-or-n-p)

        (setq read-process-output-max (* 1024 1024))

        (line-number-mode)
        (column-number-mode)

        (put 'narrow-to-region 'disabled nil)
        (put 'upcase-region 'disabled nil)
        (put 'downcase-region 'disabled nil)

        (setq
         js-indent-level 2
         c-default-style "k&r"
         c-basic-offset 2
         verilog-auto-newline nil)

        (setq-default indent-tabs-mode nil
                      tab-width 2
                      c-basic-offset 2)

        (set-default 'semantic-case-fold t)

        (setq-default show-trailing-whitespace t)

        (setq sentence-end-double-space nil)

        (prefer-coding-system 'utf-8)

        (transient-mark-mode 1)

        (setq scroll-step 1
              scroll-conservatively 5)

        (global-hl-line-mode 1)

        (setq select-enable-clipboard t
              select-enable-primary t
              save-interprogram-paste-before-kill t)

        (setq mouse-yank-at-point t)
      '';

      usePackage = {
        evil = {
          enable = true;
          init = ''
            (setq evil-want-keybinding nil
                  evil-want-Y-yank-to-eol t
                  evil-search-wrap t
                  evil-regexp-search t)
          '';
          config = ''
            (evil-mode)
          '';
        };

        evil-collection = {
          enable = true;
          after = [ "evil" ];
          config = ''
            (evil-collection-init)
          '';
        };

        gruvbox-theme = {
          enable = true;
          config = ''
            (load-theme 'gruvbox-dark-medium)
          '';
        };

        general = {
          enable = true;
          config = ''
            (general-evil-setup t)

            (general-define-key
              :states 'motion
              :prefix "SPC"
              :keymaps 'override
              "SPC" 'save-buffer
              "g" 'magit
              "w" 'evil-window-map
              "l" 'lsp-command-map
              "r" 'ivy-resume
              "c" 'counsel-git
              "p" 'projectile-command-map
              "b b" 'ivy-switch-buffer
              "b e" 'eval-buffer
              "b k" 'kill-buffer
              "b l" 'list-buffers
              "t i" 'ivy-mode
              "/ c" 'avy-goto-char-2
              "/ f" 'find-file
              "/ l" 'find-file
              "/ a" 'counsel-ag
              "/ g" 'counsel-git-grep
              "k f" 'describe-function
              "k v" 'describe-variable
              "k s" 'describe-symbol
              "x m" 'lsp-ui-imenu)

            (general-define-key
             "M-/" 'avy-goto-char)

            (general-def 'normal
              "u" 'undo-fu-only-undo
              "C-r" 'undo-fu-only-redo
              "/" 'swiper)

            (general-def 'normal lsp-mode-map
              "K" 'lsp-describe-thing-at-point)
          '';
        };

        frames-only-mode = {
          enable = true;
          config = ''
            (frames-only-mode)
          '';
        };

        ivy = {
          enable = true;
          config = ''
            (setq
             ivy-count-format "(%d/%d) "
             ivy-use-virtual-buffers t
             enable-recursive-minibuffers t)
            (ivy-mode)
          '';
        };

        ivy-rich = {
          enable = true;
          after = [ "ivy" ];
          config = ''
            (setq
             ivy-virtual-abbreviate 'full
             ivy-rich-switch-buffer-align-virtual-buffer t
             ivy-rich-path-style 'abbrev)
            (ivy-set-display-transformer 'ivy-switch-buffer
                                         'ivy-rich-switch-buffer-transform)
          '';
        };

        counsel = {
          enable = true;
          after = [ "ivy" ];
          config = ''
            (counsel-mode)
          '';
        };

        swiper = {
          enable = true;
          after = [ "ivy" ];
        };

        yasnippet = {
          enable = true;
          config = ''
            (yas-global-mode)
          '';
        };

        yasnippet-snippets = {
          enable = true;
          after = [ "yasnippet" ];
        };

        avy = {
          enable = true;
        };

        undo-fu = {
          enable = true;
        };

        undo-fu-session = {
          enable = true;
          config = ''
            (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
            (global-undo-fu-session-mode)
          '';
        };

        smooth-scrolling = {
          enable = true;
          config = ''
            (setq smooth-scrolling-margin 5)
            (smooth-scrolling-mode)
          '';
        };

        projectile = {
          enable = true;
          config = ''
            (setq projectile-completion-system 'ivy)
            (projectile-mode)
          '';
        };

        which-key = {
          enable = true;
          config = ''
            (setq which-key-idle-delay 0.01)
            (which-key-mode)
          '';
        };

        deadgrep = {
          enable = true;
        };

        smex = {
          enable = true;
          config = ''
            (smex-initialize)
          '';
        };

        company = {
          enable = true;
          config = ''
            (progn
              (add-hook 'after-init-hook 'global-company-mode))
          '';
        };

        lsp-mode = {
          enable = true;
          command = [ "lsp" "lsp-deferred" ];
          init = ''
            (setq lsp-keymap-prefix "C-c l")
          '';
          after = [ "direnv" "evil" ];
          hook = [
            "(c++-mode . lsp-deferred)"
            "(c-mode . lsp-deferred)"
            "(vhdl-mode . lsp-deferred)"
            "(verilog-mode . lsp-deferred)"
            "(haskell-mode . lsp-deferred)"
            "(haskell-literate-mode . lsp-deferred)"
            "(typescript-mode . lsp-deferred)"
            "(python-mode . lsp-deferred)"
            "(js-mode . lsp-deferred)"
            "(html-mode . lsp-deferred)"
            "(rust-mode . lsp-deferred)"
            "(lsp-mode . lsp-enable-which-key-integration)"
          ];
          config = ''
            (setq lsp-eslint-package-manager "yarn")
            (setq lsp-lens-enable t)
            (setq lsp-modeline-code-actions-enable nil)
            (advice-add 'lsp :before #'direnv-update-environment)
          '';
        };

        lsp-ui = {
          enable = true;
          hook = [
            "(prog-mode . lsp-ui-mode)"
          ];
          config = ''
            (setq lsp-ui-doc-position :bottom)
          '';
        };

        lsp-ivy = {
          enable = true;
          command = [ "lsp-ivy-workspace-symbol" ];
        };

        lsp-treemacs = {
          enable = true;
          command = [ "lsp-treemacs-error-list" ];
        };

        treemacs = {
          enable = true;
        };

        treemacs-evil = {
          enable = true;
          after = [ "treemacs" "evil" ];
        };

        treemacs-projectile = {
          enable = true;
          after = [ "treemacs" "projectile" ];
        };

        treemacs-icons-dired = {
          enable = true;
          after = [ "treemacs" "dired" ];
          config = ''
            (treemacs-icons-dired-mode)
          '';
        };

        all-the-icons = {
          enable = true;
        };

        all-the-icons-ivy = {
          enable = true;
          config = ''
            (all-the-icons-ivy-setup)
          '';
        };

        clojure-mode = {
          enable = true;
        };

        solidity-mode = {
          enable = true;
        };

        haskell-mode = {
          enable = true;
        };

        typescript-mode = {
          enable = true;
          mode = [ "\\.mts\\'" ];
        };

        nix-mode = {
          enable = true;
          mode = [ "\\.nix\\'" ];
        };

        rust-mode = {
          enable = true;
        };

        cloc = {
          enable = true;
        };

        hl-todo = {
          enable = true;
          config = ''
            (global-hl-todo-mode)
          '';
        };

        magit = {
          enable = true;
        };

        magit-todos = {
          enable = true;
          config = ''
            (magit-todos-mode)
          '';
        };

        direnv = {
          enable = true;
          config = ''
            (direnv-mode)
          '';
        };
      };

      postlude = ''
        (setq gc-cons-threshold (* 2 1000 1000))
      '';
    };
  };

  exa = {
    enable = true;
    enableAliases = true;
  };

  foot = {
    enable = true;
    server.enable = true;

    settings = {
      main = {
        term = "xterm-256color";
        font = "Tamsyn:size=10";
        dpi-aware = "yes";
      };

      mouse = {
        hide-when-typing = "yes";
      };

      colors = {
        background = "282828";
        foreground = "ebdbb2";
        regular0 = "282828";
        regular1 = "cc241d";
        regular2 = "98971a";
        regular3 = "d79921";
        regular4 = "458588";
        regular5 = "b16286";
        regular6 = "689d6a";
        regular7 = "a89984";
        bright0 = "928374";
        bright1 = "fb4934";
        bright2 = "b8bb26";
        bright3 = "fabd2f";
        bright4 = "83a598";
        bright5 = "d3869b";
        bright6 = "8ec07c";
        bright7 = "ebdbb2";
      };
    };
  };

  gh = {
    enable = false;
  };

  gpg = {
    enable = true;
    homedir = "${config.xdg.dataHome}/gnupg";
  };

  git = {
    enable = true;
    userEmail = user.email;
    userName = user.name;

    aliases = {
      a = "add";
      aa = "add -A";
      br = "branch";
      ci = "commit";
      co = "checkout";
      d = "diff";
      st = "status";
      sw = "switch";
      wt = "worktree";
    };

    delta = {
      enable = true;
      options = {
        syntax-theme = "Dracula";
        line-numbers = true;
      };
    };

    includes = [{ path = "${root}/conf/git/general.inc"; }];
  };

  ion = {
    enable = true;
  };

  mako = {
    enable = true;
    extraConfig = ''
      [mode=do-not-disturb]
      invisible=1
    '';
  };

  mpv = {
    enable = true;
    config = {
      #gpu-api = "vulkan";
      #gpu-context = "wayland";
      #gpu-context = "x11vk";
      #hwdec = "vaapi";
      #profile = "gpu-hq";
      #spirv-compiler = "shaderc";
    };
  };

  ncmpcpp = {
    enable = true;
    bindings = [
      { key = "j"; command = "scroll_down"; }
      { key = "k"; command = "scroll_up"; }
      { key = "J"; command = [ "select_item" "scroll_down" ]; }
      { key = "K"; command = [ "select_item" "scroll_up" ]; }
    ];
  };

  readline = {
    enable = true;
    extraConfig = ''
      set editing-mode vi
    '';
  };

  starship = {
    enable = true;
    settings = {
      add_newline = false;
      character = {
        success_symbol = "[>](bold green)";
        error_symbol = "[x](bold red)";
        vicmd_symbol = "[<](bold green)";
      };
    };
  };

  waybar = {
    enable = true;
    settings = {
      mainBar = {
        output = [ "DP-2" ];
        modules-right = [
          "idle_inhibitor"
          "pulseaudio"
          "network"
          "cpu"
          "memory"
          "temperature"
          "backlight"
          "keyboard-state"
          "sway/language"
          "clock"
          "tray"
        ];
      };
    };
  };

  zsh = {
    enable = true;
    enableCompletion = true;
    enableAutosuggestions = true;

    autocd = true;
    defaultKeymap = "viins";

    dotDir = "${builtins.baseNameOf config.xdg.configHome}/zsh";
    history.path = "${config.xdg.cacheHome}/zsh/history";

    initExtraFirst = ''
      source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
      if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
        source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
      fi
      [[ ! -f ~/.config/zsh/.p10k.zsh ]] || source ~/.config/zsh/.p10k.zsh
    '';

    initExtraBeforeCompInit = ''
      autoload -Uz zcalc
      autoload -Uz edit-command-line

      zle -N edit-command-line
      bindkey -M vicmd v edit-command-line
      bindkey -v '^?' backward-delete-char

      setopt globdots
      setopt autopushd
    '';
  };
}
