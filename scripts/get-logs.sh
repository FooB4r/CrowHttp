#!/bin/sh

set -eux

# Launch that script from your project directory containing the AFL output folder
output_dir=output
output_log_dir="$output_dir/log"  # where do i log
crash_dir="$output_dir/crashes"   # where do i get crashes
hang_dir="$output_dir/hangs"      # where do i get hangs
build_dir='_build/default/src'
executable="$build_dir/main.exe -v"
printexec="$build_dir/printer.exe" # Only prints to avoid hanging
server="$build_dir/cohttp_server.exe"

# building the executables
opam config exec -- jbuilder build src/main.exe
opam config exec -- jbuilder build src/cohttp_server.exe
opam config exec -- jbuilder build src/printer.exe

if [ ! -d "$output_dir" ]; then
  echo "Error: No output directory in $(pwd)"
  echo "This script needs at least output/crashes or output/hangs"
  exit 1
fi

if [ ! -d "$output_log_dir" ]; then
  mkdir "$output_log_dir"
fi

$server &
pid="$!"

# Logging / pretty printing the crashes from $executable stderr
if [ ! -d "$crash_dir" ]; then
  echo "Warning: No crash_dir in $(pwd), not logging crashes"
else
  for filename in $crash_dir/*; do
    filename=$(basename "$filename")
    output_file="$output_log_dir/$filename"
    echo "$filename" > "$output_file"
    printf "CRASH\\n" >> "$output_file"
    eval "$executable $crash_dir/$filename" 2>> "$output_file" || true
  done
fi

kill -15 "$pid"

# Logging / pretty printing the hangs from $executable stderr
if [ ! -d "$hang_dir" ]; then
  echo "Warning: No output/hangs in $(pwd), not logging hangs"
else
  for filename in $hang_dir/*; do
    filename=$(basename "$filename")
    output_file="$output_log_dir/$filename"
    echo "$filename" > "$output_file"
    printf "HANG\\n" >> "$output_file"
    eval "$printexec $hang_dir/$filename" 2>> "$output_file" || true
  done
fi
