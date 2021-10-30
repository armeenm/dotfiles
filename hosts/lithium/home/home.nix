{ config, sys, pkgs, lib, root, user, ... }:

let
  symlink = config.lib.file.mkOutOfStoreSymlink;

  home = "/home/${user.login}";

  seafile = "${home}/.seafile/Seafile";
  files = "${seafile}/mylib";
  shared = "${seafile}/common";
in
{
  username = user.login;
  homeDirectory = home;
  stateVersion = lib.mkForce "21.05";

  file.lock = {
    target = ".local/bin/lock";
    executable = true;
    text = ''
      #!${pkgs.bash}/bin/bash
      ${pkgs.playerctl}/bin/playerctl -a pause
      ${sys.security.wrapperDir}/doas ${pkgs.physlock}/bin/physlock
      #${pkgs.vbetool}/bin/vbetool dpms off
    '';
  };

  file.dnsCheck = {
    source = "${root}/conf/bin/dnscheck.sh";
    target = ".local/bin/dnscheck";
    executable = true;
  };

  file.profile = {
    target = ".profile";
    text = "";
  };

  sessionVariables = {
    PATH = "$PATH:$HOME/.local/bin";
    LESSHISTFILE = "${config.xdg.cacheHome}/less/history";
    ANDROID_SDK_ROOT = "${config.xdg.dataHome}/android";
    ANDROID_SDK_HOME = "${config.xdg.dataHome}/android";
    ANDROID_EMULATOR_HOME = "${config.xdg.dataHome}/android";
    CUDA_CACHE_PATH = "${config.xdg.cacheHome}/nv";
    IPFS_PATH = "${config.xdg.dataHome}/ipfs";
  };

  file.shared.source = symlink shared;
  file.files.source = symlink files;
  file.dl.source = symlink "${files}/dl";
  file.media.source = symlink "${files}/media";
  file.music.source = symlink "${files}/music";
  file.docs.source = symlink "${files}/docs";
  file.desktop.source = symlink "${files}/desktop";
  file.templates.source = symlink "${files}/templates";
  file.ss.source = symlink "${files}/ss";

  packages = with pkgs; [
    (winetricks.override { wine = wineWowPackages.stable; })
    arandr
    atool
    bash
    bottom
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
    font-awesome-ttf
    fortune
    gimp-with-plugins
    gksu
    gnome.gtk
    gparted
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
    mathematica
    mediainfo
    miraclecast
    monero
    mosh
    mpc_cli
    mtools
    mullvad-vpn
    neofetch
    niv
    nix-tree
    nmap
    nodejs
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    ntfs3g
    openscad
    pamixer
    pandoc
    parted
    patchutils
    pavucontrol
    pciutils
    playerctl
    python3
    qdirstat
    qemu_full
    ranger
    ripgrep
    rng-tools
    rsync
    scrcpy
    seafile-client
    seafile-shared
    sl
    slack-dark
    speedtest-cli
    strace
    streamlink
    tamsyn
    tcpdump
    tdesktop
    texlive.combined.scheme-full
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
    vlc
    vulkan-loader
    vulkan-tools
    w3m
    weechat
    wget
    whatsapp-for-linux
    whois
    wineWowPackages.stable
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
