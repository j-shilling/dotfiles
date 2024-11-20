.PHONY: home
home:
	guix home --fallback reconfigure --no-grafts --allow-downgrades ./config/home.scm
