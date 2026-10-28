#!/usr/bin/env bash
set -euo pipefail

# --- network ---
net_tooltip="Disconnected"
net_wifi=false
if ip route 2>/dev/null | grep -q "^default"; then
  iface=$(ip route | awk '/^default/ {print $5; exit}')
  ip_addr=$(ip -4 addr show "$iface" 2>/dev/null | awk '/inet / {print $2; exit}')
  if [ -d "/sys/class/net/$iface/wireless" ]; then
    net_wifi=true
    ssid=$(iwgetid -r 2>/dev/null || true)
    net_tooltip="${ssid:-$iface} · ${ip_addr:-no ip}"
  else
    net_tooltip="$iface · ${ip_addr:-no ip}"
  fi
fi

# --- disk (root filesystem) ---
disk_pct=$(df -P / 2>/dev/null | awk 'NR==2 {gsub(/%/,"",$5); print $5}')
disk_used=$(df -h / 2>/dev/null | awk 'NR==2 {print $3}')
disk_total=$(df -h / 2>/dev/null | awk 'NR==2 {print $2}')
disk_tooltip="Used: ${disk_used:-?} / ${disk_total:-?}"

# --- load average ---
load1=$(awk '{print $1}' /proc/loadavg)

# --- temperature (CPU package) ---
temp_c=""
for z in /sys/class/thermal/thermal_zone*; do
  [ -e "$z" ] || continue
  t=$(cat "$z/type" 2>/dev/null || true)
  if [ "$t" = "x86_pkg_temp" ] || [ "$t" = "acpitz" ] || [ "$t" = "Tctl" ]; then
    temp_c=$(( $(cat "$z/temp" 2>/dev/null) / 1000 ))
    break
  fi
done
if [ -z "$temp_c" ] && [ -r /sys/class/thermal/thermal_zone0/temp ]; then
  temp_c=$(( $(cat /sys/class/thermal/thermal_zone0/temp) / 1000 ))
fi

# --- CPU usage (delta of two /proc/stat samples, cached between runs) ---
cpu_sample() {
  awk '/^cpu / {printf "%d %d\n", $2+$3+$4+$5+$6+$7+$8+$9+$10, $5+$6}' /proc/stat
}
cpu_cache="${XDG_RUNTIME_DIR:-/tmp}/quickshell-cpu-stat"
cpu=0
ptotal=0; pidle=0; ctotal=0; cidle=0
if [ -f "$cpu_cache" ]; then
  read -r ptotal pidle < "$cpu_cache" 2>/dev/null || true
  read -r ctotal cidle <<< "$(cpu_sample)" 2>/dev/null || true
  dtotal=$(( ctotal - ptotal ))
  didle=$(( cidle - pidle ))
  if [ "$dtotal" -gt 0 ]; then
    cpu=$(( 100 - (didle * 100) / dtotal ))
  fi
fi
cpu_sample > "$cpu_cache"

# --- memory ---
mem_total=$(awk '/MemTotal/ {print $2}' /proc/meminfo)
mem_avail=$(awk '/MemAvailable/ {print $2}' /proc/meminfo)
if [ -n "${mem_total:-}" ] && [ "$mem_total" -gt 0 ]; then
  mem_pct=$(( (mem_total - mem_avail) * 100 / mem_total ))
else
  mem_pct=0
fi
mem_used_gb=$(awk -v t="$mem_total" -v a="$mem_avail" 'BEGIN {if (t>0) printf "%.1f", (t-a)/1024/1024; else print "0.0"}')
mem_total_gb=$(awk -v t="$mem_total" 'BEGIN {printf "%.1f", t/1024/1024}')
mem_tooltip="Used: ${mem_used_gb}G / ${mem_total_gb}G"

jq -n \
  --arg net_tooltip "$net_tooltip" \
  --argjson net_wifi "$net_wifi" \
  --arg disk_pct "${disk_pct:-?}" \
  --arg disk_tooltip "$disk_tooltip" \
  --arg load1 "${load1:-?}" \
  --arg temp_c "${temp_c:-?}" \
  --argjson cpu "$cpu" \
  --arg mem_pct "$mem_pct" \
  --arg mem_tooltip "$mem_tooltip" \
  '{
    netTooltip: $net_tooltip,
    netWifi: $net_wifi,
    diskPct: $disk_pct,
    diskTooltip: $disk_tooltip,
    load: $load1,
    temp: $temp_c,
    cpu: $cpu,
    memPct: $mem_pct,
    memTooltip: $mem_tooltip
  }'
