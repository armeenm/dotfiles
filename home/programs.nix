{ config
, isHeadless
, isPortable
, enableSocial
, pkgs
, lib
, user
, ...
}:

let
  inherit (pkgs.stdenv) hostPlatform;
in {
  programs = {
    aria2.enable = true;
    bacon.enable = true;
    bashmount.enable = true;
    bat.enable = true;
    carapace.enable = true;
    clock-rs.enable = true;
    dircolors.enable = true;
    direnv-instant.enable = true;
    fastfetch.enable = true;
    git-worktree-switcher.enable = true;
    gitui.enable = hostPlatform.isLinux;
    home-manager.enable = true;
    htop.enable = true;
    imv.enable = hostPlatform.isLinux && !isHeadless;
    mpv.enable = hostPlatform.isLinux && !isHeadless;
    navi.enable = true;
    lesspipe.enable = true;
    nix-index-database.comma.enable = true;
    nix-init.enable = true;
    nix-your-shell.enable = true;
    noti.enable = !isHeadless;
    pay-respects.enable = true;
    ripgrep-all.enable = true;
    ripgrep.enable = true;
    scmpuff.enable = true;
    zoxide.enable = true;

    aerospace = {
      enable = !isHeadless && hostPlatform.isDarwin;
      launchd.enable = true;

      settings = let
        launch = x: "exec-and-forget open -n -a ${x}";
        sketchybar = "${config.programs.sketchybar.package}/bin/sketchybar";
      in {
        automatically-unhide-macos-hidden-apps = true;
        exec-on-workspace-change = [
          "${pkgs.bash}/bin/bash"
          "-c"
          "${sketchybar} --trigger aerospace_workspace_change FOCUSED_WORKSPACE=$AEROSPACE_FOCUSED_WORKSPACE"
        ];
        on-focus-changed = [ "move-mouse window-lazy-center" ];
        on-window-detected = [
          {
            "if".app-id = "net.whatsapp.WhatsApp";
            run = [ "layout tiling" ];
          }
          {
            "if".app-id = "org.alacritty";
            run = [ "layout tiling" ];
          }
          # {
          #   "if".app-name-regex-substring = "emacs";
          #   run = [ "layout tiling" ];
          # }
        ];

        gaps = {
          inner.horizontal = 8;
          inner.vertical = 8;
        };

        mode.main.binding = {
          alt-h = "focus --boundaries all-monitors-outer-frame left";
          alt-j = "focus --boundaries all-monitors-outer-frame down";
          alt-k = "focus --boundaries all-monitors-outer-frame up";
          alt-l = "focus --boundaries all-monitors-outer-frame right";

          alt-ctrl-h = "join-with left";
          alt-ctrl-j = "join-with down";
          alt-ctrl-k = "join-with up";
          alt-ctrl-l = "join-with right";

          alt-shift-h = "move --boundaries all-monitors-outer-frame left";
          alt-shift-j = "move --boundaries all-monitors-outer-frame down";
          alt-shift-k = "move --boundaries all-monitors-outer-frame up";
          alt-shift-l = "move --boundaries all-monitors-outer-frame right";

          alt-1 = "workspace 1";
          alt-2 = "workspace 2";
          alt-3 = "workspace 3";
          alt-4 = "workspace 4";
          alt-5 = "workspace 5";
          alt-6 = "workspace 6";
          alt-7 = "workspace 7";
          alt-8 = "workspace 8";
          alt-9 = "workspace 9";

          alt-shift-1 = "move-node-to-workspace 1";
          alt-shift-2 = "move-node-to-workspace 2";
          alt-shift-3 = "move-node-to-workspace 3";
          alt-shift-4 = "move-node-to-workspace 4";
          alt-shift-5 = "move-node-to-workspace 5";
          alt-shift-6 = "move-node-to-workspace 6";
          alt-shift-7 = "move-node-to-workspace 7";
          alt-shift-8 = "move-node-to-workspace 8";
          alt-shift-9 = "move-node-to-workspace 9";

          alt-tab = "workspace-back-and-forth";
          alt-space = "layout floating tiling";
          alt-f = "fullscreen";
          alt-r = "mode resize";
          alt-p = "exec-and-forget ${config.programs.emacs.finalPackage}/bin/emacsclient -c -n";
          alt-shift-enter = ''exec-and-forget ${config.programs.alacritty.package}/bin/alacritty msg create-window'';
        };

        mode.resize.binding = {
          esc = "mode main";
          enter = "mode main";
          backspace = "flatten-workspace-tree";
          b = "balance-sizes";

          h = "resize width -50";
          j = "resize height -50";
          k = "resize height +50";
          l = "resize width +50";
        };
      };
    };

    alacritty = {
      enable = !isHeadless;
      settings = {
        window = {
          blur = true;
          decorations = if hostPlatform.isDarwin then "Buttonless" else "Full";
          dynamic_padding = true;
          option_as_alt = "OnlyLeft";
          padding = { x = 20; y = 20; };
        };
      };
    };

    atuin = {
      enable = true;
      settings = {
        auto_sync = false;
        enter_accept = true;
        filter_mode_shell_up_key_binding = "session";
        inline_height = 30;
        invert = false;
        show_preview = true;
        style = "compact";
        update_check = false;
        workspaces = true;
      };
    };

    bash = {
      enable = true;
      historyFile = "${config.xdg.cacheHome}/bash/history";
    };

    beets = {
      # enable = hostPlatform.isLinux;
      package = pkgs.stable.beets;
      settings = {
        directory = config.xdg.userDirs.music;
      };
    };

    btop = {
      enable = true;
      settings = {
        rounded_corners = false;
        vim_keys = true;
        log_level = "WARNING";
      };
    };

    cava = {
      enable = hostPlatform.isLinux;
      settings = {
        general.framerate = 144;
      };
    };

    delta = {
      enable = true;
      enableGitIntegration = true;
      options = {
        line-numbers = true;
        syntax-theme = "base16-stylix";
        navigate = true;
      };
    };

    discord = {
      enable = enableSocial;
      settings = {
        DANGEROUS_ENABLE_DEVTOOLS_ONLY_ENABLE_IF_YOU_KNOW_WHAT_YOURE_DOING = true;
      };
    };

    distrobox = {
      enable = hostPlatform.isLinux;

      settings = let
        volumes = [
          "/nix/store"
          "/etc/profiles/per-user"
          "/etc/static/profiles/per-user"
        ];
      in {
        container_additional_volumes = lib.concatStringsSep " " (map (x: "${x}:${x}:ro") volumes);
      };

      containers = {
        alma.iamge = "almalinux:latest";
        alpine.image = "alpine:latest";
        arch.image = "archlinux:latest";
        debian.image = "debian:latest";
        rhel.image = "redhat/ubi9:latest";
        suse.image = "opensuse/leap:latest";
        ubuntu.image = "ubuntu:latest";
      };
    };

    emacs = {
      enable = true;
      package = pkgs.emacsWithPackagesFromUsePackage ({
        alwaysEnsure = true;

        config = pkgs.writeTextFile {
          text = ''
            ${builtins.readFile ../conf/emacs/init.el}
            ${config.programs.emacs.extraConfig}
          '';
          name = "config.el";
        };

        override = epkgs: epkgs // {
          emsg-blame = epkgs.trivialBuild {
            pname = "emsg-blame";
            version = "unstable-2025-02-22";

            src = pkgs.fetchFromGitHub {
              owner = "ISouthRain";
              repo = "emsg-blame";
              rev = "7b0bdae8398a38b0bdb103f8cdeaaf62053496cb";
              hash = "sha256-bI3zBUJ/B2TsyQx4N8fmIrACEUEw0IX3OBpNGzVpq2Y=";
            };

            packageRequires = [ epkgs.async ];
          };
        };

        package = let
          darwinPackage = pkgs.emacs-unstable.overrideAttrs (old: {
            patches = old.patches ++ [ ./frame-transparency.patch ];
            __noChroot = true;
          });
        in if hostPlatform.isDarwin then darwinPackage else pkgs.emacs-igc-pgtk;

        extraEmacsPackages = epkgs: (config.programs.emacs.extraPackages epkgs) ++ (with epkgs; [
          treesit-grammars.with-all-grammars
        ]);

      } // (if hostPlatform.isDarwin
            then { configAsDefaultInitFile = true; }
            else { defaultInitFile = true; }));
    };

    eza = {
      enable = true;
      git = true;
    };

    firefox = {
      enable = !isHeadless;
      configPath = "${config.xdg.configHome}/mozilla/firefox";
    };

    foot = {
      enable = hostPlatform.isLinux && !isHeadless;
      server.enable = true;

      settings = {
        mouse.hide-when-typing = "yes";
        scrollback.lines = 1000000;

        key-bindings.pipe-command-output =
          ''[sh -c "f=$(mktemp); cat - > $f; emacsclient -c $f; rm $f"] Control+Shift+g'';

        main = {
          # font = lib.mkForce "Tamsyn:size=12";
          term = "xterm-256color";
          pad = "20x20";
        };
      };
    };

    gh = {
      enable = true;
      extensions = with pkgs; [
        gh-eco
      ];
    };

    gh-dash.enable = true;

    git = {
      enable = true;
      signing.format = null;

      ignores = [
        ".claude"
        ".vscode"
      ];

      settings = {
        inherit user;

        advice.addEmptyPathspec = false;
        core.editor = ''${config.home.sessionVariables.EDITOR}'';
        credential.helper = "store";
        init.defaultBranch = "master";
        push.autoSetupRemote = true;

        diff.guitool = "meld";
        "difftool \"meld\"".cmd = ''meld "$LOCAL" "$REMOTE"'';

        merge.guitool = "meld";
        "mergetool \"meld\"".cmd = ''meld "$LOCAL" "$BASE" "$REMOTE" --output "$MERGED"'';

        alias = {
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
          r = "rebase";
          rev = "rev-parse HEAD";
          st = "status";
          sw = "switch";
          wt = "worktree";
        };
      };
    };

    hyprlock = {
      enable = hostPlatform.isLinux && !isHeadless;
      settings = {
        auth."fingerprint:enabled" = true;
        general = {
          grace = 3;
          hide_cursor = true;
        };
      };
    };

    less = {
      enable = true;
      config = ''
        #env

        #command
        / forw-search ^W
      '';
    };

    mergiraf = {
      enable = true;
      enableGitIntegration = true;
    };

    nh = {
      enable = true;
      flake = "${config.home.homeDirectory}/src/dotfiles";
    };

    numbat = {
      enable = true;
      settings = {
        intro-banner = "short";
        prompt = "> ";
      };
    };

    opencode = {
      enable = enableSocial;
    };

    readline = {
      enable = true;
      extraConfig = ''
      set editing-mode vi
    '';
    };

    sketchybar = {
      enable = !isHeadless && hostPlatform.isDarwin;
      config = {
        source = ../conf/sketchybar;
        recursive = true;
      };
    };

    skim = {
      enable = true;
      defaultOptions = [ "--ansi" ];
    };

    ssh = {
      enable = true;
      package = pkgs.openssh;
      enableDefaultConfig = false;

      settings = {
        "*" = {
          compression = true;
          controlMaster = "auto";
        };

        "carbon.armeen.xyz" = {
          proxyCommand = "${pkgs.cloudflared}/bin/cloudflared access ssh --hostname %h";
        };
      };

      extraOptionOverrides = lib.optionalAttrs hostPlatform.isDarwin {
        SecurityKeyProvider = "/usr/lib/ssh-keychain.dylib";
      };
    };

    tealdeer = {
      enable = true;
      settings = {
        updates.auto_update = true;
      };
    };

    television = {
      enable = true;
      enableZshIntegration = false;
      settings = {
        keybindings = {
          quit = [ "esc" "ctrl-c" ];
        };
      };
    };

    translate-shell = {
      enable = enableSocial;
      settings = {
        hl = "en";
        tl = [ "hi" ];
      };
    };

    tofi = {
      enable = hostPlatform.isLinux && !isHeadless;
      settings = {
        "width" = "100%";
        "height" = "100%";
        "border-width" = 0;
        "outline-width" = 0;
        "padding-left" = "35%";
        "padding-top" = "35%";
        "result-spacing" = 25;
        "num-results" = 5;
        "font" = lib.mkForce "${pkgs.fira-code}/share/fonts/truetype/FiraCode-VF.ttf";
        "font-size" = lib.mkForce "";
      };
    };

    vscode = {
      enable = !isHeadless;
      package = if hostPlatform.isLinux then pkgs.vscode.fhs else pkgs.vscode;
      mutableExtensionsDir = false;

      extensions = with pkgs.vscode-extensions; [
        eamodio.gitlens
        llvm-vs-code-extensions.lldb-dap
        llvm-vs-code-extensions.vscode-clangd
        mkhl.direnv
        ms-vscode.cpptools-extension-pack
        ms-vscode.hexeditor
        vscodevim.vim
      ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
        {
          name = "llama-vscode";
          publisher = "ggml-org";
          version = "0.0.36";
          sha256 = "sha256-26Lt4/Bajg3b3TSif6k/o1l0adgRkmXA37QZxP1X3eU=";
        }
      ] ++ lib.optionals hostPlatform.isLinux [
        ms-vscode.cpptools
      ];

      profiles.default = {
        userSettings = {
          "vim.foldfix" = true;
          "clangd.path" = "${pkgs.llvmPackages_21.clang-tools}/bin/clangd";
          "C_Cpp.intelliSenseEngine" = "disabled";
          "[cpp]" = {
            "editor.defaultFormatter" = "llvm-vs-code-extensions.vscode-clangd";
          };
        };
      };
    };

    yazi = {
      enable = true;
      shellWrapperName = "y";
    };

    yt-dlp = {
      enable = enableSocial && hostPlatform.isLinux;
      settings = {
        embed-thumbnail = true;
        downloader = "aria2c";
        downloader-args = "aria2c:'-c -x8 -s8 -k1M'";
      };
    };

    zathura = {
      enable = hostPlatform.isLinux && !isHeadless;
      mappings = {
        "<C-i>" = "recolor";
      };
    };

    zellij = {
      enable = true;
      enableZshIntegration = false;

      settings = {
        pane_frames = false;
        default_layout = "compact";

        keybinds = {
          unbind = "Ctrl g";

          normal = {
            "bind \"Ctrl m\"" = { SwitchToMode = "Locked"; };
          };

          locked."bind \"Ctrl m\"" = { SwitchToMode = "Normal"; };
        };
      };
    };
  };
}
