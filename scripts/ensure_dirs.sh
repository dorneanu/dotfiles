#!/usr/bin/env bash
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
. "$DIR/base.sh"
. "$DIR/ansi"

ansi --yellow "Ensuring required directories exist.."
mkdir -pv $HOME/.local
mkdir -pv $HOME/.local/gopath
