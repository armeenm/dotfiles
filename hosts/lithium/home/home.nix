{ config, sys, pkgs, lib, root, user, inputs, ... }:

let
  symlink = config.lib.file.mkOutOfStoreSymlink;

  home = "/home/${user.login}";

  files = "${home}/files";
  common = "${home}/common";
in
{
  username = user.login;
  homeDirectory = home;
  stateVersion = lib.mkForce "21.05";

  packages =
    ## CLI Utils ##
    (with pkgs; [
      bottom
      direnv
      fd
      gh
      git-crypt
      htop
      hyperfine
      joshuto
      procs
      ripgrep
      sd
      sops
      tldr
      xplr
    ])
    ++
    (with pkgs.pkgsMusl; [
      btop
      cloc
      fasd
      file
      jq
      killall
      lsof
      mediainfo
      nix-tree
      p7zip
      #pandoc
      patchutils
      pstree
      sl
      strace
      tcpdump
      tmux
      trash-cli
      unrar
      unzip
      zip
    ])
    ++

    ## Networking ##
    (with pkgs; [
      curlie
      dog
      gping
      iperf
      ipfs
      miraclecast
      mosh
      remmina
      scrcpy
      wayvnc
      wireshark
      xh
    ])
    ++
    (with pkgs.pkgsMusl; [
      ldns
      nmap
      speedtest-cli
      w3m
      wget
      whois
    ])
    ++

    ## Privacy and Security ##
    (with pkgs; [
      keepassxc
      ledger-live-desktop
      monero
      monero-gui
      tor-browser-bundle-bin
      usbguard
      veracrypt
      yubikey-manager
      yubikey-manager-qt
      yubikey-personalization
      yubikey-personalization-gui
      yubioath-desktop
    ])
    ++
    (with pkgs.pkgsMusl; [
      bubblewrap
    ])
    ++

    ## Desktop Environment ##
    (with pkgs; [
      firefox-wayland
      google-chrome-dev
      librewolf-wayland

      gimp-with-plugins
      libreoffice-fresh

      imv
      nomacs
      river
      session-desktop-appimage
      simple-scan
      swappy

      breeze-icons
      gnome3.adwaita-icon-theme
      noto-fonts-emoji

      gtk3
    ])
    ++
    (with pkgs.pkgsMusl; [
      bemenu
      grim
      slurp
      tamsyn
      wl-clipboard
      wlr-randr
      xdg-user-dirs
      xdg_utils
      xorg.xeyes
      xorg.xkill

      fira-code
      fira-code-symbols
      font-awesome
      hicolor-icon-theme
      noto-fonts
      noto-fonts-cjk
    ])
    ++

    ## Windows ##
    (with pkgs; [
      ntfs3g
      wineWowPackages.stable
    ])
    ++
    (with pkgs.pkgsMusl; [
      dosfstools
      efibootmgr
      exfatprogs
    ])
    ++

    ## Media ##
    (with pkgs; [
      mpc_cli
      pamixer
      pavucontrol
      streamlink
      vlc
      #yt-dlp
    ])
    ++
    (with pkgs.pkgsMusl; [
      playerctl
    ])
    ++

    ## Communication ##
    (with pkgs; [
      discord-canary
      element-desktop
      gajim
      profanity
      slack-dark
      tdesktop
      whatsapp-for-linux
      zoom-us
    ])
    ++
    (with pkgs.pkgsMusl; [
      weechat
    ])
    ++

    ## Filesync/Backup ##
    (with pkgs; [
      seafile-client
    ])
    ++
    (with pkgs.pkgsMusl; [
      seafile-shared
    ]);

  file = {
    desktop.source = symlink "${files}/desktop";
    dl.source = symlink "${files}/dl";
    docs.source = symlink "${files}/docs";
    media.source = symlink "${files}/media";
    music.source = symlink "${files}/music";
    ss.source = symlink "${files}/ss";
    templates.source = symlink "${files}/templates";

    dnsCheck = {
      source = "${root}/conf/bin/dnscheck.sh";
      target = ".local/bin/dnscheck";
      executable = true;
    };

    lesskey = {
      target = ".lesskey";
      text = ''
        #env
        LESSHISTFILE=${config.xdg.cacheHome}/less/history

        #command
        / forw-search ^W
      '';
    };

    lock = {
      target = ".local/bin/lock";
      executable = true;
      text = ''
        #!${pkgs.bash}/bin/bash
        ${pkgs.playerctl}/bin/playerctl -a pause
        ${sys.security.wrapperDir}/doas ${pkgs.physlock}/bin/physlock
        #${pkgs.vbetool}/bin/vbetool dpms off
      '';
    };
  };

  sessionPath = [ "${home}/.local/bin" ];

  sessionVariables = {
    # General
    MANPAGER = "sh -c 'col -bx | bat -l man -p'";

    # Wayland
    GBM_BACKEND = "nvidia-drm";
    MOZ_ENABLE_WAYLAND = "1";
    WLR_NO_HARDWARE_CURSORS = "1";
    XKB_DEFAULT_OPTIONS = "caps:escape";
    #WLR_DRM_NO_ATOMIC = "1";
    #WLR_DRM_NO_MODIFIERS = "1";
    #NIXOS_OZONE_WL = "1";

    # Cleaning up home dir
    ANDROID_EMULATOR_HOME = "${config.xdg.dataHome}/android";
    ANDROID_SDK_HOME = "${config.xdg.dataHome}/android";
    ANDROID_SDK_ROOT = "${config.xdg.dataHome}/android";
    CUDA_CACHE_PATH = "${config.xdg.cacheHome}/nv";
    IPFS_PATH = "${config.xdg.dataHome}/ipfs";
  };

  shellAliases = {
    cat = "bat";
    diff = "delta";
    g = "git";
    open = "xdg-open";
    rlf = "readlink -f";
    zc = "zcalc -r";

    noti = "noti ";
    doas = "doas ";
    sudo = "doas ";

    sc = "systemctl";
    jc = "journalctl";
    uc = "systemctl --user";
    udc = "udisksctl";

    vi = "$EDITOR";
    vim = "$EDITOR";

    rscp = "rsync -ahvP";
  };
}
