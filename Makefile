.PHONY: switch
switch:
	sudo nixos-rebuild switch --flake . --keep-going

.PHONY: trace
trace:
	sudo nixos-rebuild switch --flake . --show-trace
