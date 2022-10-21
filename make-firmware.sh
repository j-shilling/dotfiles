#!/usr/bin/env bash

build_dir=$(guix build ovmf)
cp -v ${build_dir}/share/firmware/ovmf_x64.bin .
