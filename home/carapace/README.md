# CLI completions

Three sources feed carapace's spec directory:

- `specs/*.yaml` — hand-written specs for single commands.
- `families.nix` — one definition per family of related tools (ntfs, exfat,
  f2fs, pipewire, monero, …), rendered to one spec per command name at build
  time. Use this when several commands share most of their flag surface.
- `delegates.json` — commands that share the interface of an existing
  completer (`iptables-nft` → `iptables`, `mount.nfs` → `mount`,
  `fsck.fat` → `fsck`). One zsh function (`_completion_delegate`) is compdef'd
  for every alias and rewrites the command word before calling the canonical
  completer. Carapace spec names cannot contain dots, so dotted commands
  (`fsck.*`, `mkfs.*`, `mount.*`, `gimp-3.2`) must go through a delegate.

`util/check-completions` reads the same files, so adding a command to a family
or to `delegates.json` is enough for it to count.

## Adding a command

1. Same interface as an existing completer: add it to `delegates.json`.
2. Related to an existing family: add it to that family in `families.nix`.
3. Otherwise: add `specs/<name>.yaml`.

Then run `util/check-completions --update` after switching.

## Validation

`generated.json` is paired with `generated-meta.json`: each entry records
whether it came from real help output (`source: help`) or is an explicit
file-only fallback (`source: file`), plus the help flag, exit code, class and
parsed flag count. `util/check-completions` reports the split.

Re-audit without writing anything:

```sh
util/generate-completions --audit --from-file <command-list>
```

Classes are `ok`, `no-flags`, `not-help`, `error`, `wrong-program`,
`no-output` and `gui`. Only `ok`/`no-flags` with a matching program name are
used for flags; everything else gets a file-only spec.
