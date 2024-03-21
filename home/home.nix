{ config, osConfig, pkgs, lib, root, user, inputs, ... }:

let
  homeDir = "/home/${user.login}";

  # TODO: Factor this out along with nixpkgs.hostPlatform
  nix-misc = inputs.nix-misc.packages.x86_64-linux;

  editor = lib.getBin (pkgs.writeShellScript "editor" ''
    exec ${lib.getBin config.services.emacs.package}/bin/emacsclient -ct $@
  '');
in
{
  home = {
    # XXX: https://github.com/nix-community/home-manager/issues/4826
    activation.batCache = lib.mkForce (lib.hm.dag.entryAfter [ "linkGeneration" ] '''');

    username = user.login;
    stateVersion = osConfig.system.stateVersion;

    packages =
      ## Language Specific ##
      (with pkgs; [
        gnuapl
        python3
        bacon
        #nixd
      ]) ++

      ## CLI Utils ##
      (with nix-misc; [
        git-fuzzy
      ]) ++

      (with pkgs; [
        boxes
        btop
        bubblewrap
        comma
        direnv
        dos2unix
        duf
        expect
        fasd
        fd
        ffmpeg
        file
        gh
        htop
        httpie
        hyperfine
        jq
        killall
        libnotify
        libva-utils
        lsof
        mediainfo
        miniserve
        ncdu
        nix-output-monitor
        nix-tree
        nurl
        onefetch
        patchutils
        powertop
        procs
        ripgrep
        scc
        shell_gpt
        sops
        strace
        tcpdump
        tig
        toilet
        ueberzugpp
        unzip
        zellij
        zip
      ]) ++

      ## Networking ##
      (with pkgs; [
        bluetuith
        iperf
        ldns
        monero
        nmap
        remmina
        speedtest-cli
        wget
        whois
        wireshark
      ]) ++

      ## Desktop Environment ##
      (with pkgs; [
        firefox-wayland
        google-chrome

        #gimp-with-plugins
        libreoffice-fresh

        bemenu
        grim
        imv
        nomacs
        simple-scan
        slurp
        swappy
        swaylock
        wl-clipboard
        wlr-randr

        xdg-user-dirs
        xdg-utils
        xorg.xeyes
        xorg.xkill

        breeze-icons
        gnome.adwaita-icon-theme
        material-design-icons

        fira-code
        fira-code-symbols
        hack-font
        hicolor-icon-theme
        noto-fonts
        noto-fonts-cjk
        noto-fonts-emoji
        tamsyn

        gtk3

        vial
      ]) ++

      ## Windows ##
      (with pkgs; [
        ntfs3g
        dosfstools
        exfatprogs
      ]) ++

      ## Media ##
      (with pkgs; [
        easyeffects
        mpc_cli
        pamixer
        pavucontrol
        vlc
        playerctl
      ]) ++

      ## Communication ##
      (with pkgs; [
        discord
        weechat
        zoom-us
      ]);

    file = {
      dnsCheck = {
        source = "${root}/conf/bin/dnscheck.sh";
        target = ".local/bin/dnscheck";
        executable = true;
      };

      lesskey = {
        target = ".lesskey";
        text = ''
        #env

        #command
        / forw-search ^W
      '';
      };

      emacs-ayu-dark = {
        source = "${root}/conf/emacs/ayu-dark-theme.el";
        target = ".emacs.d/ayu-dark-theme.el";
      };
    };

    sessionPath = [ "${config.home.homeDirectory}/.local/bin" ];

    sessionVariables = {
      # General
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      MANROFFOPT = "-c";

      # Wayland
      MOZ_ENABLE_WAYLAND = "1";
      XKB_DEFAULT_OPTIONS = "caps:escape";

      EDITOR = editor;
    };

    shellAliases = {
      cat = "bat";
      g = "git";
      open = "xdg-open";
      ovpn = "openvpn3";
      rlf = "readlink -f";
      tf = "terraform";
      zc = "zcalc -r";
      zj = "zellij";
      bz = "bazel";
      ms = "miniserve -HWqrgzl --readme --index index.html";

      noti = "noti ";
      doas = "doas ";
      sudo = "doas ";

      sc = "systemctl";
      jc = "journalctl";
      uc = "systemctl --user";
      udc = "udisksctl";

      vi = "${editor} -t";
      vim = "${editor} -t";

      rscp = "rsync -ahvP";
    };
  };
}
