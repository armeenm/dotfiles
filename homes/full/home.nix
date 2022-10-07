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
      comma
      direnv
      dos2unix
      du-dust
      fd
      ffmpeg
      gh
      git-crypt
      google-cloud-sdk
      htop
      hyperfine
      joshuto
      libva-utils
      ncdu
      pandoc
      powertop
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
      libnotify
      lsof
      mediainfo
      nix-tree
      p7zip
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
      bluetuith
      croc
      curlie
      dog
      gping
      iperf
      ipfs
      miraclecast
      mosh
      remmina
      scrcpy
      w3m
      wayvnc
      wireshark
      xh
    ])
    ++
    (with pkgs.pkgsMusl; [
      ldns
      nmap
      speedtest-cli
      wget
      whois
    ])
    ++

    ## Privacy and Security ##
    (with pkgs; [
      bubblewrap
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
    ])
    ++

    ## Desktop Environment ##
    (with pkgs; [
      firefox-wayland
      google-chrome

      audacity
      gimp-with-plugins
      inkscape
      libreoffice-fresh

      bemenu
      grim
      imv
      nomacs
      river
      session-desktop-appimage
      simple-scan
      slurp
      swappy
      swaylock
      wl-clipboard
      wlr-randr

      breeze-icons
      gnome.adwaita-icon-theme

      fira-code
      fira-code-symbols
      hicolor-icon-theme
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji

      gtk3
    ])
    ++
    (with pkgs.pkgsMusl; [
      tamsyn
      xdg-user-dirs
      xdg-utils
      xorg.xeyes
      xorg.xkill
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
      yt-dlp
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
      kotatogram-desktop
      slack
      whatsapp-for-linux
      zoom-us
    ])
    ++
    (with pkgs.pkgsMusl; [
      weechat
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

    chell = {
      target = ".local/bin/chell";
      executable = true;
      text = ''
        #!/bin/sh
        ${pkgs.xdg-desktop-portal}/libexec/xdg-desktop-portal -r &
        ${pkgs.xdg-desktop-portal-gtk}/libexec/xdg-desktop-portal-gtk -r &
        ${pkgs.xdg-desktop-portal-wlr}/libexec/xdg-desktop-portal-wlr -l INFO -r &
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
    MOZ_ENABLE_WAYLAND = "1";
    XKB_DEFAULT_OPTIONS = "caps:escape";

    # Cleaning up home dir
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

    vi = "$EDITOR -t";
    vim = "$EDITOR -t";

    rscp = "rsync -ahvP";
  };
}
