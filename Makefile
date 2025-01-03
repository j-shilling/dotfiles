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
	$(GUIXTM) -- home build $(CONFIG)

.PHONY=apply
apply: guix $(CONFIG)
	$(GUIXTM) -- home reconfigure $(CONFIG)

.PHONY=repl
repl:
	$(GUIXTM) -- shell guile-next guile-ares-rs \
	-- guile \
	-L $(SRC_DIR) \
	-c \
"((@ (ares server) run-nrepl-server) #:nrepl-port-path \"./.nrepl-port\")"
