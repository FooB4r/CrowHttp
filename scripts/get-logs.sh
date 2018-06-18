#!/bin/sh

set -eux

output_dir=output
output_log_dir="$output_dir/log"
crash_dir="$output_dir/crashes"  # where do i get the crashes
hang_dir="$output_dir/hangs"     # where do i get te hangs
executable='_build/default/src/main.exe -v'

if [ ! -d "$output_dir" ]; then
  echo "Error: No output directory in $(pwd)"
  echo "This script needs at least output/crashes or output/hangs"
  exit 1
fi

if [ ! -d "$output_log_dir" ]; then
  mkdir "$output_log_dir"
fi


if [ ! -d "$crash_dir" ]; then
  echo "Warning: No crash_dir in $(pwd), not logging hangs"
else
  for filename in $crash_dir/*; do
    filename=$(basename "$filename")
    output_file="$output_log_dir/$filename"
    echo "$filename" > "$output_file"
    printf "CRASH\\n" >> "$output_file"
    eval "$executable $crash_dir/$filename" 2>> "$output_file" || true
  done
fi

if [ ! -d "$hang_dir" ]; then
  echo "Warning: No output/hangs in $(pwd), not logging hangs"
else
  for filename in $hang_dir/*; do
    filename=$(basename "$filename")
    output_file="$output_log_dir/$filename"
    echo "$filename" > "$output_file"
    printf "HANG\\n" >> "$output_file"
    eval "$executable $hang_dir/$filename" 2>> "$output_file" || true
  done
fi
