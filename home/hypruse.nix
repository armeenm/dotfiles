{ lib, pkgs }:

let
  version = "0.12.1";

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
    url = "https://files.pythonhosted.org/packages/4d/aa/84f922deef47b36bebd7353969f1d493ebca29a7bbf09cf23d63c53fd086/hypruse-${version}.tar.gz";
    hash = "sha256-wEykJM+nNdHiuFwubNHjX/lwtY8wz1RpVQBJdWQv0xE=";
  };

  build-system = [ pkgs.python3Packages.hatchling ];

  dependencies = [ pkgs.python3Packages.mcp ];

  nativeBuildInputs = [ pkgs.makeWrapper ];

  makeWrapperArgs = [
    "--prefix PATH : ${lib.makeBinPath runtimeInputs}"
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
