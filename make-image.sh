#!/usr/bin/env bash

dest=guix-image.qcow2
image_file=$(RDE_TARGET=live-system guix system image -t qcow2 --volatile ./config.scm)
cp -v ${image_file} ${dest}
chmod +w ${dest}
