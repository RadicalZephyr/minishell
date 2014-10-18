#!/usr/bin/env bash


opam config exec 'corebuild minishell.d.byte'

export BINARY=$(pwd)/minishell.d.byte

urchin test
