#!/bin/bash

# call from workspace root

cargo build -p red_moon_cli --profile performance

MEASUREMENTS=30

hyperfine --warmup 5 --min-runs $MEASUREMENTS "./target/performance/red_moon_cli $*" "lua $*"
