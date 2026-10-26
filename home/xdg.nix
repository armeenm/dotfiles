{ config
, pkgs
, lib
, enableSocial
, ...
}:

let
  inherit (pkgs.stdenv) hostPlatform;
  conf = ../conf;
  home = config.home.homeDirectory;

  browserPicker = pkgs.writeShellScript "browser-picker" ''
    local_state="${config.xdg.configHome}/google-chrome/Local State"

    declare -A profile_dirs
    options="firefox"

    if [ -f "$local_state" ]; then
      while IFS=$'\t' read -r dir label; do
        entry="chrome ($label)"
        profile_dirs["$entry"]="$dir"
        options="$options"$'\n'"$entry"
      done < <(${lib.getExe pkgs.jq} -r '
        .profile.info_cache
        | to_entries[]
        | [
            .key,
            (if (.value.user_name // "") != "" then .value.user_name else .value.name end)
          ]
        | @tsv
      ' "$local_state")
    else
      options="$options"$'\n'"chrome"
    fi

    choice=$(printf "%s\n" "$options" | ${lib.getExe pkgs.tofi} --prompt-text "open in: ")

    case "$choice" in
      firefox)
        exec firefox "$@"
        ;;
      chrome)
        exec google-chrome-stable "$@"
        ;;
      "chrome ("*)
        if [ -n "''${profile_dirs[$choice]+x}" ]; then
          exec google-chrome-stable --profile-directory="''${profile_dirs[$choice]}" "$@"
        fi
        ;;
    esac
  '';
in
{
  xdg = {
    enable = true;
    portal.config.common.default = "hyprland";

    desktopEntries = lib.optionalAttrs hostPlatform.isLinux {
      browser-picker = {
        name = "Browser Picker";
        exec = "${browserPicker} %u";
        terminal = false;
        type = "Application";
        mimeType = [
          "text/html"
          "x-scheme-handler/http"
          "x-scheme-handler/https"
          "x-scheme-handler/about"
          "x-scheme-handler/unknown"
        ];
      };

      openchamber = lib.mkIf enableSocial {
        name = "OpenChamber";
        comment = "Desktop GUI for the OpenCode AI coding agent";
        exec = "openchamber";
        icon = "${pkgs.openchamber-desktop}/share/icons/hicolor/scalable/apps/openchamber.svg";
        categories = [ "Development" ];
        terminal = false;
        type = "Application";
        # Electron reports its own app name, not the wrapper's.
        settings.StartupWMClass = "openchamber";
      };
    };

    mime.enable = hostPlatform.isLinux;
    mimeApps = {
      enable = hostPlatform.isLinux;
      defaultApplications = {
        "text/html" = ["browser-picker.desktop"];
        "x-scheme-handler/http" = ["browser-picker.desktop"];
        "x-scheme-handler/https" = ["browser-picker.desktop"];
        "x-scheme-handler/about" = ["browser-picker.desktop"];
        "x-scheme-handler/unknown" = ["browser-picker.desktop"];
        "x-scheme-handler/pdf" = ["org.pwmt.zathura.desktop"];
        "application/pdf" = ["org.pwmt.zathura.desktop"];
        "inode/directory" = ["dolphin.desktop"];
      };
    };

    configFile = {
      "zsh/p10k.zsh".source = ../conf/zsh/p10k.zsh;
      "satty/config.toml".source = ../conf/satty/config.toml;
    };

    userDirs = {
      enable = hostPlatform.isLinux;
      # Opt into the new default functionality.
      setSessionVariables = false;

      desktop = "${home}/desktop";
      documents = "${home}/docs";
      download = "${home}/dl";
      music = "${home}/music";
      pictures = "${home}/media";
      publicShare = "${home}/shared";
      templates = "${home}/templates";
      videos = "${home}/media";
    };

    systemDirs.data = lib.optionals hostPlatform.isLinux (
      [
        "/var/lib/flatpak/exports/share"
        "${home}/.local/share/flatpak/exports/share"
      ]
      ++ lib.optionals config.wayland.windowManager.hyprland.enable [
        "${config.wayland.windowManager.hyprland.package}/share"
      ]
    );
  };
}
