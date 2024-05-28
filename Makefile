.PHONY: update
update:
	home-manager switch --flake .

.PHONY: clean
clean:
	nix-collect-garbage -d
