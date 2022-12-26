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
	RDE_TARGET=system sudo -E guix system --fallback reconfigure --allow-downgrades \
	${HOME_CONFIG}

guix/system/build:
	RDE_TARGET=system sudo -E guix system --fallback build --no-grafts --allow-downgrades \
	${HOME_CONFIG}

guix/image:
	RDE_TARGET=live-system guix system vm -t qcow2 --volatile \
	${HOME_CONFIG}

guix-image.qcow2: config.scm
	./make-image.sh

ovmf_x64.bin:
	./make-firmware.sh

.PHONY: guix/run
guix/run: guix-image.qcow2 ovmf_x64.bin
	qemu-system-x86_64 -hda guix-image.qcow2 -bios ovmf_x64.bin -m 1000

# end
