#!/usr/bin/env bash
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
. "$DIR/base.sh"
. "$DIR/ansi"

TARGET="/tmp/$(id -u)-go.tar.gz"

ansi --green "Downloading Go binary"
wget https://golang.org/dl/go1.22.0.linux-amd64.tar.gz -O $TARGET

ansi --green "Installing Go"
tar -C $HOME/.local/ -xvzf $TARGET
