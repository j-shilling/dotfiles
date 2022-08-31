##
# Jake Shilling - Dotfiles
#
# @file
# @version 0.1

HOME_CONFIG=./guix/.config/guix/rde/configs.scm
RDE_TARGET=home
# GUILE_LOAD_PATH=./guix/.config/guix
GUILE_LOAD_PATH=/home/jake/dotfiles/guix/.config/guix/channel/:/home/jake/.config/guix:/home/jake/.guix-profile/share/guile/site/3.0:/home/jake/.guix-profile/share/guile/site/3.0:/run/current-system/profile/share/guile/site/3.0

channel:
	guix pull --allow-downgrades -C ./channels.scm

build:
	guix home --fallback build --no-grafts --allow-downgrades \
	${HOME_CONFIG}

# end
