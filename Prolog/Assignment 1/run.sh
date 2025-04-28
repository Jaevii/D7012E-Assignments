#!/bin/sh

# This script runs the Prolog program
# It assumes that the Prolog interpreter is installed and available in the PATH

# Load the Prolog program
swipl -q -s package_delivery -g "test, halt."