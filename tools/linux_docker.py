"""Run vkframes in a Linux Docker container, like CI's Linux (Ubuntu 24.04,
x86_64, lavapipe and Xvfb): for testing Linux from a Mac (`pixi run linux`).

Usage:
    pixi run linux                          a shell in the container, in the repository
    pixi run linux pixi run build           a command (here, as CI's build step)
    pixi run linux pixi run test
    pixi run linux pixi run -e qt qt-demo --capture captures/qt.png --capture-frame 30 --quit-after 10
    pixi run linux --xquartz pixi run example orbit_demo    windows shown on the Mac, in XQuartz
    pixi run linux --clean                  remove the container's environments and builds
    pixi run linux --rebuild-image          make the image again (e.g. after editing tools/linux/)

The repository is mounted at /src, so edits on the Mac are seen at once and
files written (captures) appear on the Mac. Its pixi environments (.pixi)
and fpm build directories (build/ of each fpm project) are the Mac's, so
in the container each is covered by a Docker volume of its own: Linux's,
kept between runs (the first run installs the environments, a few minutes).
pixi's package cache is a volume too.

Windows open on the container's own X server (Xvfb, DISPLAY=:99), which
isn't shown: capture them to see them. With --xquartz they're shown on the
Mac by XQuartz instead (its Settings > Security: "Allow connections from
network clients", then restart it; the container is allowed with `xhost
+localhost`). They're drawn by Mesa's CPU Vulkan driver, emulated: slow, a
few frames a second, but enough to try things out.

Needs Docker (Docker Desktop: started if it isn't running). On Apple
silicon the x86_64 image runs emulated; Docker Desktop's Rosetta setting
("Use Rosetta for x86_64/amd64 emulation") makes it much faster.
"""

import os
import re
import shutil
import subprocess
import sys
import time

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
IMAGE = "vkframes-linux"
PREFIX = "vkframes-linux-"          # (the volumes' names)


def docker(*args, **kwargs):
    return subprocess.run(["docker", *args], **kwargs)


def ensure_docker():
    if not shutil.which("docker"):
        sys.exit("linux_docker: docker not found (install Docker Desktop)")
    if docker("info", capture_output=True).returncode == 0:
        return
    if sys.platform == "darwin":
        print("starting Docker Desktop...", flush=True)
        subprocess.run(["open", "-a", "Docker"])
        for _ in range(90):
            time.sleep(2)
            if docker("info", capture_output=True).returncode == 0:
                return
    sys.exit("linux_docker: the Docker daemon isn't running")


def build_image(force):
    if not force and docker("image", "inspect", IMAGE, capture_output=True).returncode == 0:
        return
    print(f"building the {IMAGE} image...", flush=True)
    context = os.path.join(ROOT, "tools", "linux")
    if docker("build", "--platform", "linux/amd64", "-t", IMAGE, context).returncode != 0:
        sys.exit("linux_docker: the image failed to build")


def xquartz_display():
    """docker run's options for showing windows in XQuartz (its display :0,
    reached from the container at host.docker.internal), once it's checked
    that XQuartz takes network clients and allowed this Mac's."""
    xhost = "/opt/X11/bin/xhost"
    if not os.path.exists(xhost):
        sys.exit("linux_docker: XQuartz not found (https://www.xquartz.org)")
    listening = subprocess.run(["defaults", "read", "org.xquartz.X11", "nolisten_tcp"],
                               capture_output=True, text=True).stdout.strip() == "0"
    if not listening:
        sys.exit("linux_docker: in XQuartz's Settings > Security, allow connections from network clients, "
                 "then restart XQuartz")
    if subprocess.run([xhost, "+localhost"], env=dict(os.environ, DISPLAY=":0"), capture_output=True).returncode != 0:
        sys.exit("linux_docker: xhost failed: is XQuartz running?")
    return ["-e", "DISPLAY=host.docker.internal:0"]


def covered_dirs():
    """The Mac's directories covered in the container (relative to ROOT): the
    pixi environments, and each fpm project's build directory."""
    dirs = [".pixi"]
    for top, subdirs, files in os.walk(ROOT):
        subdirs[:] = [d for d in subdirs if d not in (".pixi", "build", ".git", "node_modules")]
        if "fpm.toml" in files:
            dirs.append(os.path.relpath(os.path.join(top, "build"), ROOT))
    return sorted(dirs)


def volume_name(rel):
    return PREFIX + re.sub(r"[^A-Za-z0-9_.-]", "_", rel.replace(os.sep, "__"))


def clean():
    names = docker("volume", "ls", "-q", "--filter", f"name={PREFIX}", capture_output=True, text=True).stdout.split()
    if names:
        docker("volume", "rm", *names)
    print(f"removed {len(names)} volumes")


def main(argv):
    if argv[:1] in (["-h"], ["--help"]):
        sys.exit(__doc__)
    ensure_docker()
    if argv[:1] == ["--clean"]:
        return clean()
    force = argv[:1] == ["--rebuild-image"]
    if force:
        argv = argv[1:]
    build_image(force)
    display = []
    if argv[:1] == ["--xquartz"]:
        argv = argv[1:]
        display = xquartz_display()

    mounts = ["-v", f"{ROOT}:/src", "-v", f"{PREFIX}rattler-cache:/root/.cache/rattler"]
    for rel in covered_dirs():
        mounts += ["-v", f"{volume_name(rel)}:/src/{rel.replace(os.sep, '/')}"]
    tty = ["-it"] if sys.stdin.isatty() and sys.stdout.isatty() else ["-i"]
    command = argv or ["bash"]
    return docker("run", "--rm", *tty, "--platform", "linux/amd64", *mounts, *display, "-w", "/src",
                  IMAGE, *command).returncode


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
