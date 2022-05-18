#!/bin/bash

source /afs/umbc.edu/software/cadence/etc/setup_2017/cadence6.bashrc

# -------------------
# Launch Virtuoso
# -------------------

LD_PRELOAD=/usr/lib64/libstdc++.so.6 virtuoso &
