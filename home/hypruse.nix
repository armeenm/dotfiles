{ lib, pkgs }:

let
  version = "0.13.0";

  # hypruse drives Hyprland through its IPC surface plus a handful of wlroots
  # binaries it finds on PATH; bake the ones it needs in so the MCP server works
  # no matter what PATH the agent host launches it with. `hyprctl` and `busctl`
  # are deliberately left to the session, so hypruse talks to the compositor and
  # systemd the host is actually running rather than a pinned copy.
  runtimeInputs = with pkgs; [
    grim
    imagemagick
    wl-clipboard
    wtype
  ];
in
pkgs.python3Packages.buildPythonApplication rec {
  pname = "hypruse";
  inherit version;
  pyproject = true;

  src = pkgs.fetchurl {
    url = "https://files.pythonhosted.org/packages/8b/d4/61354f93e776131e202f02855e04dd152c7dfe10aae7589136e33fa35394/hypruse-${version}.tar.gz";
    hash = "sha256-MhNiORIjKDokUEuZMtqrJnJnxBWbQLwtheDVp6xOEUs=";
  };

  build-system = [ pkgs.python3Packages.hatchling ];

  dependencies = [ pkgs.python3Packages.mcp ];

  nativeBuildInputs = [ pkgs.makeWrapper ];

  makeWrapperArgs = [
    "--prefix"
    "PATH"
    ":"
    (lib.makeBinPath runtimeInputs)
  ];

  pythonImportsCheck = [ "hypruse" ];

  meta = {
    description = "Computer use for Hyprland: an MCP server giving AI agents native hands on a Wayland desktop";
    homepage = "https://github.com/IlyasKhallouki/hypruse";
    license = lib.licenses.mit;
    mainProgram = "hypruse";
    platforms = lib.platforms.linux;
  };
}
