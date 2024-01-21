# Phony target to tangle all org files
.PHONY: tangle
tangle:
	find . -name "*.org" -exec sh -c 'echo "[!] Tangling {}"; ./tangle_script.sh "{}"' \;
