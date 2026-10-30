# A Linux like CI's (ubuntu-latest, x86_64) for testing vkframes from a Mac:
# see tools/linux_docker.py (`pixi run linux`), which builds and runs it.
#
# As the CI runner: Mesa's CPU Vulkan driver (lavapipe), for the conda Vulkan
# loader (VK_DRIVER_FILES), and an X server (Xvfb) for windows. binutils:
# fpm calls `ar` itself (the runner has it; the base image doesn't).

FROM ubuntu:24.04

RUN apt-get update \
    && DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends \
        ca-certificates curl git binutils mesa-vulkan-drivers xvfb xauth \
    && rm -rf /var/lib/apt/lists/*

RUN curl -fsSL https://pixi.sh/install.sh | PIXI_HOME=/usr/local PIXI_NO_PATH_UPDATE=1 bash

ENV VK_DRIVER_FILES=/usr/share/vulkan/icd.d/lvp_icd.json \
    DISPLAY=:99

COPY entrypoint.sh /usr/local/bin/vkframes-entrypoint
RUN chmod +x /usr/local/bin/vkframes-entrypoint
ENTRYPOINT ["/usr/local/bin/vkframes-entrypoint"]
CMD ["bash"]
