#!/bin/sh

# Determine path separator based on OS
if [ "$(uname)" = "Linux" ] || [ "$(uname)" = "Darwin" ]; then
    SEP=":"
else
    SEP=";"
fi

# This script runs the Prolog program
# It assumes that the Prolog interpreter is installed and available in the PATH

# Runs the given program from argument
# "./play.sh" ttt or "./play.sh othello"
swipl -q -s $1.pl -g "initBoard(B), showState(B), moves(1, B, MvList)."