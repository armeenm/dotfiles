# Carapace spec files

Hand-written `carapace-spec` YAML for commands that have no upstream
completion: no carapace spec, no bash/fish completion, and no completion
generator of their own.

They are installed to `~/.config/carapace/specs/<name>.yaml` by
`home/completions.nix`.

Format: <https://carapace-sh.github.io/carapace-spec/>

Validate a spec by pointing carapace at a temporary config dir:

```sh
tmp=$(mktemp -d)
mkdir -p "$tmp/carapace/specs"
cp mycmd.yaml "$tmp/carapace/specs/"
XDG_CONFIG_HOME="$tmp" carapace mycmd zsh mycmd --help
```

`util/check-completions` measures the result.
