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

## 5. Package list for the PR

```sh
nvd diff /run/current-system <new-toplevel>
```

`/run/current-system` is usually still the pre-bump system, so this is the
old→new diff. Report the `[U.]`/`[U*]` upgrades; `[C.]` lines are same-version
changes, not updates.

## 6. Commit and open the PR

```sh
git commit -am "chore: bump flakes and pinned packages"
git push -u origin <branch>
gh pr create --base master --head <branch> \
  --title "chore: bump flakes and pinned packages" --body-file -
```

Use separate commits for genuine fixes (e.g. `fix: …`) rather than folding them
into the bump.
