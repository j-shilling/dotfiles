SRC_DIR=./src
GUIX_PROFILE=target/profiles/guix
GUIX=GUILE_LOAD_PATH="./src:${GUIX_PROFILE}/share/guile/site/3.0" GUIX_LOAD_COMPILED_PATH="" GUIX_PROFILE=${GUIX_PROFILE} ${GUIX_PROFILE}/bin/guix
GUIX_PULL_EXTRA_OPTIONS=--allow-downgrades
CONFIG=${SRC_DIR}/home.scm

all: apply

.PHONY=upgrade
upgrade:
	echo -e "(use-modules (guix channels))\n" > channels-lock-tmp.scm
	guix time-machine -C channels.scm -- \
	describe -f channels >> channels-lock-tmp.scm
	mv channels-lock-tmp.scm channels-lock.scm

channels-lock.scm: channels.scm
	echo -e "(use-modules (guix channels))\n" > channels-lock-tmp.scm
	guix time-machine -C channels.scm -- \
	describe -f channels >> channels-lock-tmp.scm
	mv channels-lock-tmp.scm channels-lock.scm

target/profiles:
	mkdir -p target/profiles

target/profiles/guix: target/profiles channels-lock.scm
	guix pull -C channels-lock.scm -p ${GUIX_PROFILE} \
	${GUIX_PULL_EXTRA_OPTIONS}

target/guix-time-marker: channels-lock.scm
	make target/profiles/guix
	touch $@

guix: target/guix-time-marker

build: guix ${CONFIG}
	${GUIX} home build ${CONFIG}

apply: guix ${CONFIG}
	${GUIX} home reconfigure ${CONFIG}

repl:
	${GUIX} shell guile-next guile-ares-rs \
	-- guile \
        -L ${GUIX_PROFILE}/share/guile/site/3.0 \
	-L ${SRC_DIR} \
	-c \
"(begin (use-modules (guix gexp))) \
((@ (ares server) run-nrepl-server) #:nrepl-port-path \"./.nrepl-port\")"

clean-target:
	rm -rfv ./target

clean: clean-target
