#!/usr/bin/env bash
set -euo pipefail

target=$1
tmp=$(mktemp)

cp -L "$target" "$tmp"
rm "$target"
mv "$tmp" "$target"
chmod +w "$target"
