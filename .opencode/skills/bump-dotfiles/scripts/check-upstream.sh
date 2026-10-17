#!/usr/bin/env bash
# Print the latest upstream version of every hand-pinned package in this repo,
# so a bump can be checked at a glance. Read-only; needs network and `gh`.
set -euo pipefail

# Evaluate a python expression against JSON on stdin (jq may not be installed).
jget() { python3 -c "import sys,json;d=json.load(sys.stdin);print($1)"; }

echo "== flake inputs (ref-pinned; nix flake update will not move these) =="
printf 'lanzaboote     %s\n' "$(gh release list --repo nix-community/lanzaboote --limit 1 | cut -f1)"

echo
echo "== hand-pinned packages =="
printf 'opencode       %s\n' "$(curl -fsS https://registry.npmjs.org/@opencode/cli-linux-x64 | jget 'd["dist-tags"]["latest"]')"
printf 'openchamber    %s\n' "$(git ls-remote --tags https://github.com/openchamber/openchamber | sed 's#.*refs/tags/##' | grep -v '\^{}' | sort -V | tail -1)"
printf 'hypruse        %s\n' "$(curl -fsS https://pypi.org/pypi/hypruse/json | jget 'd["info"]["version"]')"
printf 'emsg-blame     %s\n' "$(gh api repos/ISouthRain/emsg-blame/commits --jq '.[0].sha')"
printf 'llama-vscode   %s\n' "$(curl -fsS -X POST 'https://marketplace.visualstudio.com/_apis/public/gallery/extensionquery' \
  -H 'Accept: application/json;api-version=3.0-preview.1' \
  -H 'Content-Type: application/json' \
  -d '{"filters":[{"criteria":[{"filterType":7,"value":"ggml-org.llama-vscode"}]}],"flags":950}' \
  | jget 'd["results"][0]["extensions"][0]["versions"][0]["version"]')"
