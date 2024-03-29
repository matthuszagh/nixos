.PHONY: switch
switch:
	sudo nixos-rebuild switch --flake . --keep-going --verbose

.PHONY: trace
trace:
	sudo nixos-rebuild switch --flake . --show-trace --verbose

.PHONY: vm
vm:
	sudo nixos-rebuild build-vm --flake . --keep-going

.PHONY: iso
iso:
	nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=iso.nix
