#!/bin/sh

set -eu

aflfuzz=afl-fuzz
executable=_build/default/main.exe

## parallelization
cmd1="$aflfuzz -i input/ -o sync_dir/ -M fuzzer01 $executable"
cmd2="$aflfuzz -i input/ -o sync_dir/ -S fuzzer02 $executable"
cmd3="$aflfuzz -i input/ -o sync_dir/ -S fuzzer03 $executable"
cmd4="$aflfuzz -i input/ -o sync_dir/ -S fuzzer04 $executable"

tmux new -s afl_fuzz -d
tmux bind-key -n C-c send-keys C-c "tmux kill-session -t afl_fuzz" Enter
tmux split-window -v -t afl_fuzz:0.0
tmux split-window -h -t afl_fuzz:0.0
tmux split-window -h -t afl_fuzz:0.2
# Fuzzers
tmux send-keys -t afl_fuzz:0.0 "$cmd1" C-m
tmux send-keys -t afl_fuzz:0.1 "$cmd2" C-m
tmux send-keys -t afl_fuzz:0.2 "$cmd3" C-m
tmux send-keys -t afl_fuzz:0.3 "$cmd4" C-m
tmux attach -t afl_fuzz
