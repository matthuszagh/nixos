.PHONY: switch
switch:
	sudo nixos-rebuild switch --flake .

.PHONY: trace
trace:
	sudo nixos-rebuild switch --flake . --show-trace
