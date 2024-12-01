SRC_DIR=./src
GUIX_PROFILE=target/profiles/guix
GUIX=GUILE_LOAD_PATH="./src" GUIX_LOAD_COMPILED_PATH="" ${GUIX_PROFILE}/bin/guix
PULL_EXTRA_OPTIONS=
CONFIG=${SRC_DIR}/config.scm

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
	-L ${SRC_DIR}

clean-target:
	rm -rfv ./target

clean: clean-target
