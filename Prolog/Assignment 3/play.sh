#!/bin/sh

# This script runs the Prolog program
# It assumes that the Prolog interpreter is installed and available in the PATH

# Runs the given program from argument
# "./play.sh" ttt or "./play.sh othello"
swipl -q -s $1.pl -g "play."