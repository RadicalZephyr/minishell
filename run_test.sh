#!/usr/bin/env bash

eval `opam config env`
corebuild minishell.d.byte

BINARY=$(pwd)/minishell.d.byte

urchin test
