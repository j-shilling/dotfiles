.PHONY: apply
apply:
	home-manager switch --flake .

.PHONY: update
update:
	nix-channel --update
	nix flake update
	home-manager switch --flake .

.PHONY: clean
clean:
	nix-collect-garbage -d
