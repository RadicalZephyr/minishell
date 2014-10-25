#!/usr/bin/env bash


opam config exec 'corebuild minishell.d.byte'

if [[ $? != 0 ]]
then
    echo "Compilation failed!!!"
    exit 1
fi

export BINARY=$(pwd)/minishell.d.byte

urchin test
