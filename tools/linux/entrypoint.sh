#!/bin/bash
# The container's entry point (tools/linux/Dockerfile): an X server for the
# windows (as CI's xvfb-run, on DISPLAY=:99), unless DISPLAY is another's
# (XQuartz's: linux_docker.py --xquartz), then the command.
if [ "$DISPLAY" = ":99" ]; then
    Xvfb :99 -screen 0 1280x1024x24 -nolisten tcp >/dev/null 2>&1 &
    for _ in $(seq 50); do [ -e /tmp/.X11-unix/X99 ] && break; sleep 0.1; done
fi
exec "$@"
