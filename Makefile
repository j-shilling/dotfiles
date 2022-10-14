##
# Jake Shilling - Dotfiles
#
# @file
# @version 0.1

HOME_CONFIG=./config.scm
RDE_TARGET=home

guix/channel:
	guix pull --allow-downgrades -C ./channels.scm

guix/home:
	guix home --fallback reconfigure --no-grafts --allow-downgrades \
	${HOME_CONFIG}

guix/home/build:
	guix home --fallback build --no-grafts --allow-downgrades \
	${HOME_CONFIG}

guix/system:
	RDE_TARGET=system sudo -E guix system --fallback reconfigure \
	${HOME_CONFIG}

guix/system/build:
	RDE_TARGET=system sudo -E guix system --fallback build --no-grafts --allow-downgrades \
	${HOME_CONFIG}

# guix/profile:
# 	guix package -m ./guix/.config/guix/jrs/profiles/default.scm

# guix/profile/build:
# 	guix build -m ./guix/.config/guix/jrs/profiles/default.scm

# end
