#!/usr/bin/env bash

BINARY=minishell.d.byte
corebuild $BINARY

if [[ $? != 0 ]]
then
    echo "Compilation failed!!!"
    exit 1
fi

export BINARY=$PWD/$BINARY

urchin test
