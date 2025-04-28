#!/bin/sh

# This script runs the Prolog program
# It assumes that the Prolog interpreter is installed and available in the PATH

# Load the Prolog program
swipl -q -s smallest_k_sets -g "test, halt."