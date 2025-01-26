# Local File Paths
SRC_DIR=./src
CONFIG=${SRC_DIR}/home.scm
CHANNEL_FILE=./channels.scm
CHANNEL_LOCK_FILE=./channels-lock.scm

# Executable
GUIXTM=GUILE_LOAD_PATH="${SRC_DIR}" guix time-machine -C "${CHANNEL_LOCK_FILE}"

.PHONY=all
all: apply

$(CHANNEL_LOCK_FILE): $(CHANNEL_FILE)
	echo -e "(use-modules (guix channels))\n" > channels-lock-tmp.scm
	guix time-machine -C channels.scm -- \
	describe -f channels >> channels-lock-tmp.scm
	mv channels-lock-tmp.scm channels-lock.scm

.PHONY=upgrade
upgrade: $(CHANNEL_FILE)
	rm -v $(CHANNEL_LOCK_FILE)
	make $(CHANNEL_LOCK_FILE)

.PHONY=pull
pull: $(CHANNEL_LOCK_FILE)
	guix pull -C channels-lock.scm --allow-downgrades

.guix-time-marker: $(CHANNEL_LOCK_FILE)
	make pull
	touch $@

guix: .guix-time-marker

.PHONY=build
build: guix $(CONFIG)
	$(GUIXTM) -- home build --allow-downgrades $(CONFIG)

.PHONY=apply
apply: guix $(CONFIG)
	$(GUIXTM) -- home reconfigure --allow-downgrades $(CONFIG)

.PHONY=repl
repl:
	$(GUIXTM) -- shell \
	guile-next guile-ares-rs guile-gnutls guile-avahi guile-gcrypt \
	guile-json guile-lib guile-semver guile-sqlite3 guile-ssh guile-git \
	guile-zlib guile-lzlib guile-zstd \
	-- \
	guile \
	--no-auto-compile \
	-L $(SRC_DIR) \
	-L ~/.config/guix/current/share/guile/site/3.0/ \
	-c \
"((@ (ares server) run-nrepl-server) #:nrepl-port-path \"./.nrepl-port\")"
