#!/bin/sh

set -eu

aflfuzz=afl-fuzz
executable=_build/default/main.exe

# echo $core_pattern_backup >/proc/sys/kernel/core_pattern
# echo $scaling_governor_backup | tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor

# Reset on exit
exit_tmux="on_exit() {
  tmux kill-session -t afl_fuzz
  exit
}

trap 'on_exit' INT
"

## parallelization
cmd1="$aflfuzz -i input/ -o sync_dir/ -M fuzzer01 $executable"
cmd2="$aflfuzz -i input/ -o sync_dir/ -S fuzzer02 $executable"
cmd3="$aflfuzz -i input/ -o sync_dir/ -S fuzzer03 $executable"
cmd4="$aflfuzz -i input/ -o sync_dir/ -S fuzzer04 $executable"

tmux new -s afl_fuzz -d
tmux split-window -v -t afl_fuzz:0.0
tmux split-window -h -t afl_fuzz:0.0
tmux split-window -h -t afl_fuzz:0.2
# Fuzzers
tmux send-keys -t afl_fuzz:0.0 "$exit_tmux $cmd1" C-m
tmux send-keys -t afl_fuzz:0.1 "$exit_tmux $cmd2" C-m
tmux send-keys -t afl_fuzz:0.2 "$exit_tmux $cmd3" C-m
tmux send-keys -t afl_fuzz:0.3 "$exit_tmux $cmd4" C-m
tmux attach -t afl_fuzz
