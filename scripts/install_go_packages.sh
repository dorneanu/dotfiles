#!/usr/bin/env bash
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
. "$DIR/base.sh"
. "$DIR/ansi"

if ! isavailable go; then
    ansi --yellow "Go not installed. Skipping"
    exit 0
fi

ansi --green "Installing Chezmoi"
go install github.com/twpayne/chezmoi@latest

ansi --green "Installing gopls.."
GO111MODULE=on go install golang.org/x/tools/gopls@latest

ansi --green "Installing golangci-lint"
# GO111MODULE=on CGO_ENABLED=0 go install -v -trimpath -ldflags '-s -w' github.com/golangci/golangci-lint/cmd/golangci-lint
go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest

ansi --green "Installing go tools"
go install -v golang.org/x/tools/cmd/godoc@latest
go install -v golang.org/x/tools/cmd/goimports@latest
go install -v github.com/stamblerre/gocode@latest
go install -v golang.org/x/tools/cmd/gorename@latest
go install -v golang.org/x/tools/cmd/guru@latest
go install -v github.com/cweill/gotests/...@latest
go install github.com/go-delve/delve/cmd/dlv@latest

ansi --green "Installing other golang tools"
go install -v github.com/davidrjenni/reftools/cmd/fillstruct@latest
go install -v github.com/fatih/gomodifytags@latest
go install -v github.com/godoctor/godoctor@latest
go install -v github.com/haya14busa/gopkgs/cmd/gopkgs@latest
go install -v github.com/josharian/impl@latest
go install -v github.com/rogpeppe/godef@latest
