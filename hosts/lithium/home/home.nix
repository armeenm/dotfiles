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
    openscad
    slack-dark
    cura
    jq
    mullvad-vpn
    home-manager
    mathematica
    discord
    gimp-with-plugins
    trash-cli
    element-desktop
    mosh
    monero
    qdirstat
    texlive.combined.scheme-full
    pandoc
    evince
    vbetool
    bash
    nix-tree
    caffeine-ng
    file
    killall
    torbrowser
    niv
    tcpdump
    arandr
    pamixer
    pavucontrol
    ldns
    mpc_cli
    xautolock
    xclip
    xorg.xinit
    xorg.xev
    xorg.xdpyinfo
    xorg.xkill
    zoom-us
    bottom
    python3
    nodejs
    usbutils
    pciutils
    fasd
    libnotify
    wget
    keepassxc
    ledger-live-desktop
    ntfs3g
    tdesktop
    whatsapp-for-linux
    neofetch
    vlc
    streamlink
    unrar
    unzip
    direnv
    miraclecast
    weechat
    whois
    ripgrep
    cloc
    nmap
    iperf
    seafile-client
    seafile-shared
    speedtest-cli
    wineWowPackages.stable
    (winetricks.override { wine = wineWowPackages.stable; })
    ranger
    mediainfo
    highlight
    atool
    w3m
    xmobar
    yubico-pam
    yubikey-manager-qt
    yubikey-personalization
    yubikey-personalization-gui
    yubioath-desktop
    rsync
    strace
    tamsyn
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
    fira-code
    fira-code-symbols
    font-awesome-ttf
    libreoffice-fresh
    gparted
    parted
    exfatprogs
    dosfstools
    mtools
    sl
    tpm-tools
    rng-tools
    trousers # TODO
    gksu
    efibootmgr
    playerctl
    toilet
    figlet
    lolcat
    cowsay
    fortune
    vulkan-loader
    vulkan-tools
    linuxPackages.cpupower
    gnome.gtk
    xdg-user-dirs
    ipfs
    fd
    lsof
    hdparm
    qemu_full
    virt-manager
    compsize
    scrcpy
    patchutils
    lm_sensors
    usbguard
    lshw
  ];
}
