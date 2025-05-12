#!/bin/sh

# Determine path separator based on OS
if [ "$(uname)" = "Linux" ] || [ "$(uname)" = "Darwin" ]; then
    SEP=":"
else
    SEP=";"
fi

# This script runs the Prolog program
# It assumes that the Prolog interpreter is installed and available in the PATH

# Runs the program
swipl -q -s othello.pl -g "play, halt."