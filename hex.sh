#!/usr/bin/env bash

echo '{username,<<"'${HEX_USERNAME}'">>}.' > ~/.hex/hex.config
echo '{key,<<"'${HEX_KEY}'">>}.' >> ~/.hex/hex.config

rebar3 hex publish <<EOF
y
EOF
