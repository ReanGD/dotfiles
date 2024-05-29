#!/bin/bash

output=$(nvidia-smi --query-gpu=temperature.gpu,fan.speed,memory.total,memory.used --format=csv,noheader,nounits)

IFS=',' read -r temperature fan_speed memory_total memory_used <<< "$output"

echo "{\"text\":\"󰢮 ${temperature}°C 󰈐 ${fan_speed}%\",\"tooltip\":\"GPU Temperature: ${temperature}°C\nFan Speed: ${fan_speed}%\nMemory: ${memory_used} / ${memory_total} MiB\"}"
