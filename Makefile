.PHONY: nix
nix: 
	NIXPKGS_ALLOW_INSECURE=1 nix develop --extra-experimental-features flakes --impure

