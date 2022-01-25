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

  packages = with pkgs; [
    swtpm
    #vulnix
    cura
    direnv
    discord
    element-desktop
    evince
    fd
    firefox
    gajim
    gh
    gimp-with-plugins
    google-chrome-dev
    gparted
    gqrx
    gtk3
    htop
    iperf
    ipfs
    keepassxc
    ledger-live-desktop
    libreoffice-fresh
    lshw
    miraclecast
    monero
    monero-gui
    mosh
    mpc_cli
    mtr
    mullvad-vpn
    neofetch
    noto-fonts-emoji
    ntfs3g
    openscad
    pamixer
    parted
    pavucontrol
    procs
    profanity
    qdirstat
    qemu_full
    ranger
    remmina
    ripgrep
    river
    rng-tools
    rxvt_unicode.terminfo
    scrcpy
    seafile-client
    simple-scan
    slack-dark
    sops
    streamlink
    swappy
    tdesktop
    torbrowser
    tpm-tools
    usbguard
    usbutils
    veracrypt
    virt-manager
    vlc
    wayvnc
    whatsapp-for-linux
    wineWowPackages.stable
    wireshark
    yt-dlp
    yubikey-manager
    yubikey-manager-qt
    yubikey-personalization
    yubikey-personalization-gui
    yubioath-desktop
    zoom-us
  ] ++ (with pkgs.pkgsMusl; [
    asciinema
    atool
    bemenu
    btop
    bubblewrap
    cloc
    cowsay
    dosfstools
    efibootmgr
    exfatprogs
    fasd
    figlet
    file
    fira-code
    fira-code-symbols
    font-awesome-ttf
    fortune
    grim
    hdparm
    hicolor-icon-theme
    highlight
    jq
    killall
    ldns
    libnotify
    lm_sensors
    lolcat
    lsof
    mediainfo
    nix-tree
    nmap
    noto-fonts
    noto-fonts-cjk
    p7zip
    pandoc
    patchutils
    pciutils
    playerctl
    pstree
    seafile-shared
    sl
    slurp
    speedtest-cli
    strace
    tamsyn
    tcpdump
    tmux
    trash-cli
    unrar
    unzip
    vbetool
    vulkan-loader
    vulkan-tools
    w3m
    weechat
    wget
    whois
    wl-clipboard
    wlr-randr
    xdg-user-dirs
    xorg.xeyes
    xorg.xkill
    zip
  ]);

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
  };

  sessionPath = [ "$HOME/.local/bin" ];

  sessionVariables = {
    # General
    EDITOR = "$HOME/.local/bin/editor";
    MANPAGER = "sh -c 'col -bx | bat -l man -p'";

    # Wayland
    GBM_BACKEND = "nvidia-drm";
    MOZ_ENABLE_WAYLAND = "1";
    WLR_NO_HARDWARE_CURSORS = "1";
    XKB_DEFAULT_OPTIONS="caps:escape";

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
    zc = "zcalc -r";

    noti = "noti ";
    doas = "doas ";
    sudo = "doas ";

    sc = "systemctl";
    uc = "systemctl --user";
    udc = "udisksctl";

    vi = "$EDITOR";
    vim = "$EDITOR";

    rscp = "rsync -ahvP";
  };
}
