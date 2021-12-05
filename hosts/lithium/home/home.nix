{ config, sys, pkgs, lib, root, user, ... }:

let
  symlink = config.lib.file.mkOutOfStoreSymlink;

  home = "/home/${user.login}";

  seafile = "${config.xdg.dataHome}/seafile/Seafile";
  files = "${seafile}/mylib";
  common = "${seafile}/common";
in
{
  username = user.login;
  homeDirectory = home;
  stateVersion = lib.mkForce "21.05";

  file = {
    common.source = symlink common;
    files.source = symlink files;
    dl.source = symlink "${files}/dl";
    media.source = symlink "${files}/media";
    music.source = symlink "${files}/music";
    docs.source = symlink "${files}/docs";
    desktop.source = symlink "${files}/desktop";
    templates.source = symlink "${files}/templates";
    ss.source = symlink "${files}/ss";

    dnsCheck = {
      source = "${root}/conf/bin/dnscheck.sh";
      target = ".local/bin/dnscheck";
      executable = true;
    };

    editor = {
      target = ".local/bin/editor";
      executable = true;
      text = ''
        #!/bin/sh
        emacsclient -c -t "$@"
      '';
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

    profanity = {
      source = "${root}/conf/profanity/profrc";
      target = "${config.xdg.configHome}/profanity/profrc";
    };

    river = {
      source = "${root}/conf/river/init";
      target = "${config.xdg.configHome}/river/init";
      executable = true;
    };
  };

  sessionVariables = {
    PATH = "$PATH:$HOME/.local/bin";
    ANDROID_SDK_ROOT = "${config.xdg.dataHome}/android";
    ANDROID_SDK_HOME = "${config.xdg.dataHome}/android";
    ANDROID_EMULATOR_HOME = "${config.xdg.dataHome}/android";
    CUDA_CACHE_PATH = "${config.xdg.cacheHome}/nv";
    IPFS_PATH = "${config.xdg.dataHome}/ipfs";
    GNUPGHOME = "${config.xdg.dataHome}/gnupg";
    EDITOR = "$HOME/.local/bin/editor";
    MANPAGER = "sh -c 'col -bx | bat -l man -p'";
  };

  packages = with pkgs; [
    wlr-randr
    arandr
    asciinema
    atool
    avrdude
    caffeine-ng
    cloc
    compsize
    cowsay
    cura
    direnv
    discord
    dosfstools
    efibootmgr
    element-desktop
    evince
    exfatprogs
    fasd
    fd
    figlet
    file
    fira-code
    fira-code-symbols
    firefox
    font-awesome-ttf
    foot
    fortune
    gajim
    gh
    gimp-with-plugins
    gnome.gtk
    gparted
    gqrx
    hdparm
    highlight
    home-manager
    iperf
    ipfs
    jq
    keepassxc
    killall
    ldns
    ledger-live-desktop
    libnotify
    libreoffice-fresh
    linuxPackages.cpupower
    lm_sensors
    lolcat
    lshw
    lsof
    mediainfo
    miraclecast
    monero
    monero-gui
    mosh
    mpc_cli
    mtools
    mtr
    mullvad-vpn
    neofetch
    niv
    nix-tree
    nixopsUnstable
    nmap
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    ntfs3g
    openscad
    p7zip
    pamixer
    pandoc
    parted
    patchutils
    pavucontrol
    pciutils
    playerctl
    procs
    profanity
    pstree
    qdirstat
    qemu_full
    ranger
    remmina
    ripgrep
    river
    rng-tools
    rsync
    scrcpy
    seafile-client
    seafile-shared
    simple-scan
    sl
    slack-dark
    speedtest-cli
    strace
    streamlink
    swtpm
    tamsyn
    tcpdump
    tdesktop
    texlive.combined.scheme-full
    tmux
    toilet
    torbrowser
    tpm-tools
    trash-cli
    trousers # TODO
    unrar
    unzip
    usbguard
    usbutils
    vbetool
    virt-manager
    virt-manager
    vlc
    vulkan-loader
    vulkan-tools
    w3m
    weechat
    wget
    whatsapp-for-linux
    whois
    wineWowPackages.stable
    wireshark
    xautolock
    xclip
    xdg-user-dirs
    xmobar
    xorg.xdpyinfo
    xorg.xev
    xorg.xinit
    xorg.xkill
    yubico-pam
    yubikey-manager-qt
    yubikey-personalization
    yubikey-personalization-gui
    yubioath-desktop
    zoom-us
  ];
}
