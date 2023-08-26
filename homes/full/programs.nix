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
      theme = "ayu";
    };
    themes = {
      ayu = builtins.readFile (pkgs.fetchFromGitHub
        {
          owner = "dempfi";
          repo = "ayu";
          rev = "4.0.3";
          hash = "sha256-O0zoKAmCgSAHv2gcORYrorIlw0kdXN1+2k2Emtntc2g=";
        } + "/ayu-dark.tmTheme");
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
    package = pkgs.emacs-pgtk;

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
        (setq display-line-numbers-type 'relative)
        (global-display-line-numbers-mode)

        (put 'narrow-to-region 'disabled nil)
        (put 'upcase-region 'disabled nil)
        (put 'downcase-region 'disabled nil)

        (setq
         js-indent-level 2
         c-default-style "k&r"
         c-basic-offset 2
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
          (add-hook hook
            (lambda () (setq show-trailing-whitespace nil))))

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

        (setq scroll-step 1
              scroll-margin 7
              scroll-conservatively 100000)

        (global-hl-line-mode 1)

        (xterm-mouse-mode 1)

        (setq select-enable-clipboard t
              select-enable-primary t
              save-interprogram-paste-before-kill t)

        (setq mouse-yank-at-point t)
      '';

      usePackage = {
        all-the-icons.enable = true;
        avy.enable = true;
        bazel.enable = true;
        cloc.enable = true;
        clojure-mode.enable = true;
        consult.enable = true;
        devdocs.enable = true;
        git-link.enable = true;
        git-timemachine.enable = true;
        haskell-mode.enable = true;
        hyperbole.enable = true;
        julia-mode.enable = true;
        magit.enable = true;
        nix-mode.enable = true;
        prism.enable = true;
        rainbow-mode.enable = true;
        rust-mode.enable = true;
        smartparens.enable = true;
        solidity-mode.enable = true;
        symbol-overlay.enable = true;
        treemacs.enable = true;
        typescript-mode.enable = true;
        undo-fu.enable = true;
        vterm.enable = true;
        vundo.enable = true;
        wgrep.enable = true;

        blamer = {
          enable = true;
          config = ''
            (global-blamer-mode 1)
          '';
        };

        cape = {
          enable = true;
          init = ''
            (add-to-list 'completion-at-point-functions #'cape-dabbrev)
            (add-to-list 'completion-at-point-functions #'cape-file)
            (add-to-list 'completion-at-point-functions #'cape-elisp-block)
            (add-to-list 'completion-at-point-functions #'cape-history)
            ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
            ;; (add-to-list 'completion-at-point-functions #'cape-tex)
            ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
            ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
            (add-to-list 'completion-at-point-functions #'cape-abbrev)
            ;; (add-to-list 'completion-at-point-functions #'cape-dict)
            ;; (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
            ;; (add-to-list 'completion-at-point-functions #'cape-line)
          '';
        };

        color-identifiers-mode = {
          enable = true;
          config = ''
            (global-color-identifiers-mode)
          '';
        };

        corfu = {
          enable = true;
          config = ''
            (setq
              corfu-cycle t
              corfu-auto t)
            (global-corfu-mode)
          '';
        };

        corfu-terminal = {
          enable = true;
          config = ''
            (unless (display-graphic-p)
              (corfu-terminal-mode +1))
          '';
        };

        coterm = {
          enable = true;
          config = ''
            (coterm-mode)
          '';
        };

        direnv = {
          enable = true;
          config = ''
            (direnv-mode)
          '';
        };

        diff-hl = {
          enable = true;
          config = ''
            (global-diff-hl-mode)
          '';
        };

        embark = {
          enable = true;
          after = [ "frames-only-mode" ];

          init = ''
            (setq prefix-help-command #'embark-prefix-help-command)
            (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
          '';

          config = ''
            ;; Hide the mode line of the Embark live/completions buffers
            (add-to-list 'display-buffer-alist
                         '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                           nil
                           (window-parameters (mode-line-format . none))))

             (add-to-list 'frames-only-mode-use-window-functions
                          'embark-act)
          '';
        };

        embark-consult = {
          enable = true;
          hook = [ "(embark-collect-mode . consult-preview-at-point-mode)" ];
        };

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

        evil-mc = {
          enable = true;
          config = ''
            (global-evil-mc-mode 1)
          '';
        };

        frames-only-mode = {
          enable = true;
          config = ''
            (frames-only-mode)
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
              "p" 'projectile-command-map
              "r" 'vertico-repeat
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
             "M-." 'embark-dwim)

            (general-def 'normal
              "u" 'undo-fu-only-undo
              "C-r" 'undo-fu-only-redo
              "/" 'consult-line)

            (general-def 'visual
              "A" 'evil-mc-make-cursor-in-visual-selection-end
              "I" 'evil-mc-make-cursor-in-visual-selection-beg)

            (general-define-key
             :keymaps 'vertico-map
             "C-'" 'vertico-quick-jump
             "C-o" 'vertico-quick-exit
             "C-i" 'vertico-quick-insert)
          '';
        };

        ayu-theme = {
          enable = true;
          config = ''
            (load-theme 'ayu-dark)
          '';
        };

        highlight-thing = {
          enable = true;
          config = ''
            (global-highlight-thing-mode)
          '';
        };

        hl-todo = {
          enable = true;
          config = ''
            (global-hl-todo-mode)
          '';
        };

        indent-guide = {
          enable = true;
          config = ''
            (indent-guide-global-mode)
          '';
        };

        literate-calc-mode = {
          enable = true;
          config = ''
            (setq literate-calc-mode-idle-time 0.1)
            (literate-calc-mode)
          '';
        };

        marginalia = {
          enable = true;
          config = ''
            (marginalia-mode)
          '';
        };

        magit-todos = {
          enable = true;
          config = ''
            (magit-todos-mode)
          '';
        };

        orderless = {
          enable = true;
          config = ''
            (setq completion-styles '(orderless basic)
                  completion-category-defaults nil
                  completion-category-overrides '((file (styles partial-completion))))
          '';
        };

        projectile = {
          enable = true;
          config = ''
            (projectile-mode)
          '';
        };

        rainbow-delimiters = {
          enable = true;
          config = ''
            (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
          '';
        };

        savehist = {
          enable = true;
          config = ''
            (savehist-mode)
            (add-to-list 'savehist-additional-variables #'vertico-repeat-history)
          '';
        };

        smex = {
          enable = true;
          config = ''
            (smex-initialize)
          '';
        };

        smooth-scrolling = {
          enable = true;
          config = ''
            (setq smooth-scrolling-margin 5)
            (smooth-scrolling-mode)
          '';
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

        undo-fu-session = {
          enable = true;
          config = ''
            (setq undo-fu-session-incompatible-files '("/COMMIT_EDITMSG\\'" "/git-rebase-todo\\'"))
            (global-undo-fu-session-mode)
          '';
        };

        vertico = {
          enable = true;
          config = ''
            (vertico-mode)
          '';
        };

        vertico-grid = {
          enable = true;
          config = ''
            ;; TODO: Enable this only sometimes.
            ;; (vertico-grid-mode)
          '';
        };

        vertico-indexed = {
          enable = true;
          config = ''
            (vertico-indexed-mode)
          '';
        };

        vertico-quick.enable = true;

        vertico-mouse = {
          enable = true;
          config = ''
            (vertico-mouse-mode)
          '';
        };

        vertico-repeat = {
          enable = true;
          config = ''
            (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
          '';
        };

        which-key = {
          enable = true;
          config = ''
            (setq which-key-idle-delay 0.01)
            (which-key-mode)
          '';
        };

        whitespace-cleanup-mode = {
          enable = true;
          config = ''
            (global-whitespace-cleanup-mode)
          '';
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
        font = "Tamsyn:size=12";
        dpi-aware = "no";
      };

      mouse = {
        hide-when-typing = "yes";
      };

      colors = {
        # Ayu theme.
        background = "000919";
        foreground = "c3c0bb";

        regular0 = "242936"; # black
        regular1 = "f28779"; # red
        regular2 = "d5ff80"; # green
        regular3 = "ffd173"; # yellow
        regular4 = "73d0ff"; # blue
        regular5 = "dfbfff"; # magenta
        regular6 = "5ccfe6"; # cyan
        regular7 = "cccac2"; # white

        bright0 = "fcfcfc"; # bright black
        bright1 = "f07171"; # bright red
        bright2 = "86b300"; # bright gree
        bright3 = "f2ae49"; # bright yellow
        bright4 = "399ee6"; # bright blue
        bright5 = "a37acc"; # bright magenta
        bright6 = "55b4d4"; # bright cyan
        bright7 = "5c6166"; # bright white
      };
    };
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
      ds = "diff --staged";
      f = "fuzzy";
      pl = "pull";
      ps = "push";
      psf = "push --force-with-lease";
      st = "status";
      sw = "switch";
      wt = "worktree";
    };

    delta = {
      enable = true;
      options = {
        syntax-theme = "ayu";
        line-numbers = true;
      };
    };

    extraConfig = {
      init = {
        defaultBranch = "master";
      };

      credential = {
        helper = "store";
      };

      core = {
        editor = "${config.home.sessionVariables.EDITOR}";
      };
    };
  };

  mpv = {
    enable = true;
    config = {
      gpu-api = "vulkan";
      gpu-context = "wayland";
      hwdec = "vaapi";
      profile = "gpu-hq";
      spirv-compiler = "shaderc";
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

  waybar = {
    enable = true;
    systemd.enable = true;

    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 24;

        modules-left = [
          "wlr/workspaces"
          "wlr/mode"
        ];

        modules-center = [ "hyprland/window" ];

        modules-right = [
          "pulseaudio"
          "network"
          "temperature"
          "cpu"
          "memory"
          "battery"
          "tray"
          "clock"
        ];

        "wlr/workspaces" = {
          disable-scroll = true;
          all-outputs = false;
          on-click = "activate";
        };

        "wlr/mode" = { format = "<span style=\"italic\">{}</span>"; };
        "tray" = {
          # "icon-size" = 21,
          "spacing" = 10;
        };

        "clock" = { "format-alt" = "{:%Y-%m-%d}"; "on-click" = ""; };
        "cpu" = {
          "format" = "{usage}% 󰍛";
        };

        "memory" = { "format"= "{}% "; };

        "temperature" = {
          "critical-threshold" = 80;
          "format" = "{}℃  󰏈";
          "format-critical" = "{}℃ 󰇺";
          "interval" = 5;
        };

        "battery" = {
            "bat"= "BAT0";
            "states"= {
                # "good"= 95;
                "warning"= 30;
                "critical"= 15;
            };
            "format"= "{capacity}% {icon}";
            # "format-good"= ""; # An empty format will hide the module
            # "format-full"= "";
            "format-icons"= ["" "" "" "" ""];
        };

        "network" = {
            "format-wifi"= "{essid} ({signalStrength}%) ";
            "format-ethernet"= "{ifname}= {ipaddr}/{cidr} ";
            "format-disconnected"= "Disconnected ⚠";
        };

        "pulseaudio" = {
            #"scroll-step"= 1;
            "format"= "{volume}% {icon}";
            "format-bluetooth"= "{volume}% {icon}";
            "format-muted"= "";
            "format-icons"= {
                "headphones" = "";
                "handsfree" = "";
                "headset" = "";
                "phone" = "";
                "portable" = "";
                "car" = "";
                "default" = [ "" "" ];
            };
            "on-click"= "pavucontrol";
        };

        "hyprland/window" = {
          "format" = {};
          "seperate-outputs" = true;
        };
      };
    };
  };

  zsh = {
    enable = true;
    enableAutosuggestions = true;
    enableCompletion = true;
    enableVteIntegration = true;
    syntaxHighlighting.enable = true;
    historySubstringSearch.enable = true;

    autocd = true;
    defaultKeymap = "viins";

    dotDir = "${builtins.baseNameOf config.xdg.configHome}/zsh";

    history = {
      path = "${config.xdg.cacheHome}/zsh/history";
      ignoreSpace = true;
    };

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
