#!/usr/bin/env bash
set -euo pipefail

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" >/dev/null 2>&1 && pwd)"
. "$DIR/base.sh"

PYTHON_REQUIREMENTS="$DIR/requirements.txt"
PYTHON_VERSION=3.9.7
PYENV_DIR="$HOME/.pyenv"

if [ ! -d "$PYENV_DIR" ]; then
    curl https://pyenv.run | bash
    export PATH="$HOME/.pyenv/bin:$PATH"
    pyenv install -s $PYTHON_VERSION
fi

eval "$(pyenv init -)"
pyenv local $PYTHON_VERSION
pip install --upgrade -r $PYTHON_REQUIREMENTS
mv $DIR/../.python-version $HOME
