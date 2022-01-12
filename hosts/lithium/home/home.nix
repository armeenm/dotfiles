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
    asciinema
    atool
    bemenu
    btop
    bubblewrap
    cloc
    cowsay
    cudaPackages.cudatoolkit_11_5
    cura
    direnv
    discord
    dosfstools
    efibootmgr
    electron
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
    fortune
    gajim
    gh
    gimp-with-plugins
    gnome.gtk
    google-chrome-dev
    gparted
    gqrx
    grim
    hdparm
    hicolor-icon-theme
    highlight
    htop
    iperf
    ipfs
    jq
    keepassxc
    killall
    ldns
    ledger-live-desktop
    libnotify
    libreoffice-fresh
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
    scrcpy
    seafile-client
    seafile-shared
    simple-scan
    sl
    slack-dark
    slurp
    sops
    speedtest-cli
    strace
    streamlink
    swappy
    swtpm
    tamsyn
    tcpdump
    tdesktop
    tmux
    torbrowser
    tpm-tools
    trash-cli
    unrar
    unzip
    usbguard
    usbutils
    vbetool
    veracrypt
    virt-manager
    vlc
    vulkan-loader
    vulkan-tools
    vulnix
    w3m
    weechat
    wget
    whatsapp-for-linux
    whois
    wineWowPackages.stable
    wireshark
    wl-clipboard
    wlr-randr
    xdg-user-dirs
    xorg.xeyes
    xorg.xkill
    yt-dlp
    yubico-pam
    yubikey-manager
    yubikey-manager-qt
    yubikey-personalization
    yubikey-personalization-gui
    yubioath-desktop
    zip
    zoom-us
  ];

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

    # Cleaning up home dir
    ANDROID_EMULATOR_HOME = "${config.xdg.dataHome}/android";
    ANDROID_SDK_HOME = "${config.xdg.dataHome}/android";
    ANDROID_SDK_ROOT = "${config.xdg.dataHome}/android";
    CUDA_CACHE_PATH = "${config.xdg.cacheHome}/nv";
    IPFS_PATH = "${config.xdg.dataHome}/ipfs";
  };

  shellAliases = {
    doas = "doas ";
    sudo = "doas ";
    noti = "noti ";
    cat = "bat";
    diff = "delta";
    g = "git";
    open = "xdg-open";
    userctl = "systemctl --user";
    vi = "$EDITOR";
    vim = "$EDITOR";
    zc = "zcalc -r";
  };
}
