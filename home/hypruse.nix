{ lib, pkgs }:

let
  version = "0.15.1";

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
    url = "https://files.pythonhosted.org/packages/5c/c8/b2fd60cef21ad7600de374fb55202bbf2eaf4afa2ee00a061aac7775d4d6/hypruse-${version}.tar.gz";
    hash = "sha256-fPydxj3fb9dS86s7a+sRo00EYYWvBec+gwglhOCIoGo=";
  };

  build-system = [ pkgs.python3Packages.hatchling ];

  # hypruse pins `mcp>=1.21.1,<2`; nixpkgs' `mcp` is 2.x, so use the 1.x
  # package nixpkgs keeps alongside it.
  dependencies = [ pkgs.python3Packages.mcp_1 ];

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
