# Adapted from https://github.com/benmezger/dotfiles

LOGFILE="/tmp/$(id -u)-dotfiles.log"

default: run
help:
	@echo 'Management commands for dotfiles:'
	@echo
	@echo 'Usage:'
	@echo '    make install-go-packages    Install go dependencies.'

pyenv:
	@echo "Installing pyenv.."
	bash ./scripts/install_pyenv.sh | tee -a $(LOGFILE) || exit 1

ensure-dirs:
	@echo "Ensuring directories.."
	bash ./scripts/ensure_dirs.sh | tee -a $(LOGFILE) || exit 1

install-chezmoi:
	@echo "Installing chezmoi.."
	bash ./scripts/install_chezmoi.sh | tee -a $(LOGFILE) || exit 1

install-go:
	@echo "Installing go ..."
	bash ./scripts/install_go.sh | tee -a $(LOGFILE) || exit 1

install-go-packages:
	@echo "Installing go packages.."
	bash ./scripts/install_go_packages.sh | tee -a $(LOGFILE) || exit 1

install-pyenv:
	@echo "Installing pyenv"
	bash ./scripts/install_pyenv.sh | tee -a $(LOGFILE) || exit 1

install-shell-deps:
	@echo "Installing shell deps"
	bash ./scripts/install_shell_deps.sh | tee -a $(LOGFILE) || exit 1

chezmoi-init:
	@echo "Initializing chezmoi.."
	chezmoi init -S ${CURDIR} -v 

chezmoi-apply: 
	@echo "Applying chezmoi.."
	chezmoi apply -v
