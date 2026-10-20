---
name: Bump dotfiles
description: Bump the nix flake inputs and hand-pinned packages in this dotfiles repo, prove every Linux configuration still builds, and open a pull request. Use for the weekly dotfiles bump.
---

# Bump dotfiles

Weekly routine: refresh the flake inputs and the hand-pinned packages, prove
every Linux configuration builds, and open a PR.

## 0. Work in a worktree

Do the bump in a fresh worktree, never directly on `master` (OpenChamber:
`session.moveToWorktree` with folder `bump-flake`).

## 1. Stop GitHub API rate limits

`nix flake update` queries the GitHub API and **silently keeps cached
versions** when it is rate-limited. Always pass the `gh` token:

```sh
export NIX_CONFIG="access-tokens = github.com=$(gh auth token)"
```

## 2. Update the flake inputs

```sh
nix flake update
```

Then bump the ref-pinned inputs, which `nix flake update` will not move:

| Input | Current form | How to check |
| --- | --- | --- |
| `lanzaboote` | `github:nix-community/lanzaboote/vX.Y.Z` | `gh release list --repo nix-community/lanzaboote --limit 1` |
| `nixpkgs-stable` | `github:nixos/nixpkgs/nixos-XX.YY-small` | if the branch is frozen/EOL, move to the current stable branch |
| `nix` | flakehub `DeterminateSystems/nix-src/<ver>` | resolves to the latest `3.x`; leave the URL, the lock moves |

After editing `flake.nix`:

```sh
nix flake update nixpkgs-stable lanzaboote
```

A frozen `nixpkgs-stable` branch looks like a `lastModified` months old in
`flake.lock`; confirm with
`git ls-remote https://github.com/nixos/nixpkgs refs/heads/nixos-<next>-small`.

## 3. Bump the hand-pinned packages

`scripts/check-upstream.sh` prints the latest upstream version of each one.
Compare, then update the file:

| File | Package | Bump |
| --- | --- | --- |
| `home/opencode.nix` | opencode | `version` + the three `sha512-…` hashes |
| `overlay/openchamber.nix` | openchamber-desktop | `version`, `src` hash, **and** the two `node-modules*` fixed-output hashes |
| `home/hypruse.nix` | hypruse | `version` + `src` hash (the tarball URL embeds the version) |
| `home/programs.nix` | llama-vscode | `version` + `sha256` |
| `home/programs.nix` | emsg-blame | `version` (date), `rev`, `hash` |

### Getting hashes

Plain files (npm tarball, VS Code `.vsix`):

```sh
nix-prefetch-url <url>            # base32
nix hash convert --hash-algo sha256 --to sri <base32>
```

`fetchFromGitHub` sources (emsg-blame):

```sh
nix-prefetch-url --unpack https://github.com/<owner>/<repo>/archive/<rev>.tar.gz
nix hash convert --hash-algo sha256 --to sri <base32>
```

`openchamber-desktop`'s two `bun install` trees cannot be prefetched: set both
`hash = lib.fakeHash;`, build, and copy the `got:` values nix reports:

```sh
nix build --impure --keep-going --file /tmp/openchamber-build.nix
```

with a small expression that imports the flake and returns
`pkgs.openchamber-desktop`. Then build it for real to confirm 2.x still has the
`build:web-assets` / `bundle:main` scripts the derivation calls.

Smoke-test the packaged app against an isolated profile instead of replacing the
running desktop app:

```sh
mkdir -p /tmp/opencode/oc/home/{.config,.local/share,.cache}
env HOME=/tmp/opencode/oc/home \
  XDG_CONFIG_HOME=/tmp/opencode/oc/home/.config \
  XDG_DATA_HOME=/tmp/opencode/oc/home/.local/share \
  XDG_CACHE_HOME=/tmp/opencode/oc/home/.cache \
  $(nix build --impure --print-out-paths --file /tmp/openchamber-build.nix)/bin/openchamber-desktop \
  > /tmp/opencode/oc/run.log 2>&1
```

Check `packages/electron/package.json` for the Electron major it expects. The
wrapper uses nixpkgs' `electron`, which lags upstream; when the app needs a
newer one, pass it from `overlay/default.nix`
(`electron = final.electron_44;`). 2.5.0 needs Electron 44 for
`webContents.setZoomMode`; without it startup dies with
`setZoomMode is not a function` and no window appears.

## 4. Verify

```sh
nix flake check --no-build      # evaluation only, does NOT build
nix build --keep-going --print-out-paths \
  .#nixosConfigurations.lithium.config.system.build.toplevel \
  .#nixosConfigurations.argentum.config.system.build.toplevel \
  .#nixosConfigurations.carbon.config.system.build.toplevel \
  .#nixosConfigurations.hydrogen.config.system.build.toplevel
```

Every host must build. **`nix flake check` is not enough on its own** — without
`--no-build` it does build, but it also fails on the pre-existing darwin /
`deploy` problems, so build the four Linux toplevels explicitly.

`nixos-unstable-small` regularly ships regressions. Work around them in
`overlay/default.nix` with a `TODO` naming the upstream issue, and delete the
workaround once upstream fixes it (see the existing `TODO`s there for the shape
of one).

## 5. Package list and PR text

```sh
nvd diff /run/current-system <new-toplevel>
```

`/run/current-system` is usually still the pre-bump system, so this is the
old→new diff. Report the `[U.]`/`[U*]` upgrades; `[C.]` lines are same-version
changes, not updates.

### One table, with changelogs

Put the `nvd` upgrades and the hand-pinned bumps in a **single** table
(`Package | Old | New | Changelog`), not one table per source. Deduplicate
aliases into one row (`code`/`vscode`, `zathura`/`zathura-with-plugins`,
`emacs-*-with-packages`). `[U*]` rows whose old version is `<none>` were newly
pulled in, not upgraded; leave them out.

Changelog links come from nixpkgs metadata first:

```sh
nix eval --impure --json --expr '
let
  f = builtins.getFlake (toString ./.);
  p = f.inputs.nixpkgs.legacyPackages.x86_64-linux;
  get = n: {
    inherit n;
    homepage = p.${n}.meta.homepage or null;
    changelog = p.${n}.meta.changelog or null;
  };
in map get [ "starship" "uv" "upower" ]
'
```

For packages nested elsewhere, use their real attr path (e.g.
`p.tree-sitter-grammars.<name>`). If `meta.changelog` is null:

- `gh api repos/<owner>/<repo>/releases/tags/<tag> --jq .body` to find the
  release, or `gh api repos/<owner>/<repo>/tags`
- a tag with no release gets a compare link
  (`https://github.com/<owner>/<repo>/compare/<old>...<new>`)
- VS Code: `https://code.visualstudio.com/updates/v1_<minor>`
- MELPA snapshot packages (magit, posframe, …): link the upstream repo or
  `https://melpa.org/#/<pkg>`
- follow a moved repo via the package's npm/PyPI metadata (e.g. opencode's
  repository is now `anomalyco/opencode`)
- check every link with `curl -sIL -o /dev/null -w '%{http_code}' <url>`
  (some sites reject HEAD, so fall back to `curl -s -o /dev/null -w`)

### Highlights

Read the release notes of the notable bumps and add a short "Worth a look"
section in the PR body. Aim for 5-10 bullets, and prefer things that matter
here:

- breaking changes (OpenCode 2.1 renamed `providerState`/`resultState`)
- security fixes (Starship GHSA releases)
- changes to tools this repo drives or embeds (Hypruse background mode,
  claude-code's `--worktree` cleanup fix, an app's new Electron requirement)
- notable features (VS Code canvases, OpenChamber find-in-changes)

Skip "bug fixes only" releases from the highlights, but keep them in the
table with their changelog link.

### PR body shape

```md
Weekly bump of the flake inputs and hand-pinned packages.

<nixpkgs rev change, frozen-input status, unchanged pins>

## Bumps

<the single table>

## Worth a look

<a few bullets, linked>

## Verification

<four toplevels, flake check, runtime smoke test if a GUI app moved, fixes>
```

## 6. Commit and open the PR

```sh
git commit -am "chore: bump flakes and pinned packages"
git push -u origin <branch>
gh pr create --base master --head <branch> \
  --title "chore: bump flakes and pinned packages" --body-file -
```

Use separate commits for genuine fixes (e.g. `fix: …`) rather than folding them
into the bump.
