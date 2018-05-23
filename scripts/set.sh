#!/bin/sh

set -eu

if [ "$EUID" -ne 0 ]
  then echo "Please run as root"
  exit
fi

# Save
reset_script=scripts/reset.sh
core_pattern_backup=$(cat /proc/sys/kernel/core_pattern)
scaling_governor_backup=$(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_governor)

# create reset script accoring to the default config
if [ ! -e "$reset_script" ]
then
  su "$USER" sh -c "cat > $reset_script <<-EOF
#!/bin/sh
set -eu
echo '$core_pattern_backup' >/proc/sys/kernel/core_pattern
echo $scaling_governor_backup | tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor
EOF
"
  chmod +x "$reset_script"
else
  echo "$reset_script script already done"
fi

# Other Performance factors improvement
#   - Transparent huge pages. Some allocators, such as jemalloc, can incur a
#     heavy fuzzing penalty when transparent huge pages (THP) are enabled in the
#     kernel. You can disable this via:
#
#     echo never > /sys/kernel/mm/transparent_hugepage/enabled
#
#   - Suboptimal scheduling strategies. The significance of this will vary from
#     one target to another, but on Linux, you may want to make sure that the
#     following options are set:
#
#     echo 1 >/proc/sys/kernel/sched_child_runs_first
#     echo 1 >/proc/sys/kernel/sched_autogroup_enabled
