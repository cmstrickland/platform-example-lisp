#!/usr/bin/env bash

set -e

cd "$PLATFORM_CACHE_DIR"

function build {
    sh bootstrap
    ./configure --prefix ~/.global
    make
}

if [ ! -d roswell ]; then
    git clone --branch release https://github.com/roswell/roswell.git
    cd roswell
    build
else
    cd roswell
    old_head=$(git rev-parse HEAD)
    git pull origin release
    new_head=$(git rev-parse HEAD)
    if [ ! "$new_head" = "$old_head" ]; then
	git clean -xdf
	build
    fi
fi

make install
ros setup
