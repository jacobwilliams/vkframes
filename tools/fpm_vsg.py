"""Run fpm with the C++ compiler, VSG and linker settings that vkframes needs.

Run inside the pixi environment (e.g. `pixi run build`), from the project root.

Usage:
    python tools/fpm_vsg.py <fpm command> [fpm args...]   e.g. build, test, run --example hello_sphere
    python tools/fpm_vsg.py --env                         print `export ...` lines; then plain
                                                          `fpm build` works in that shell

fpm compiles the C++ layer (src/vsg/*.cpp) as part of the vkframes library and
links the libraries declared in fpm.toml. This script supplies the settings that
depend on the environment, through fpm's standard environment variables:
    FPM_CXX       clang++ (macOS) / g++
    FPM_CXXFLAGS  -std=c++17 -fPIC, VSG's compile definitions, the env's include dir
    FPM_LDFLAGS   the env's library directory and rpath

VSG ships only CMake package files (no pkg-config), so the flags its CMake
targets carry are reproduced here: VSG_SHARED_LIBRARY, VSGXCHANGE_SHARED_LIBRARY,
C++17 and <prefix>/include.

FPM_CXXFLAGS and FPM_LDFLAGS already set in the environment are kept (appended).
"""

import os
import shlex
import shutil
import subprocess
import sys

WINDOWS = os.name == "nt"
MACOS = sys.platform == "darwin"


def which(*names):
    for n in names:
        if shutil.which(n):
            return n
    sys.exit(f"fpm_vsg: none of {names} found on PATH")


def prefix():
    p = os.environ.get("CONDA_PREFIX")
    if not p:
        sys.exit("fpm_vsg: CONDA_PREFIX is not set; run inside the pixi environment (pixi run ...)")
    return os.path.join(p, "Library") if WINDOWS else p


def fpm_settings():
    """FPM_CXX, FPM_CXXFLAGS and FPM_LDFLAGS for building the C++ layer with fpm."""
    if WINDOWS:
        sys.exit("fpm_vsg: Windows (MSVC-built VSG) is not supported yet")
    pre = prefix()
    inc = os.path.join(pre, "include")
    lib = os.path.join(pre, "lib")
    cxx = os.environ.get("FPM_CXX") or (which("clang++") if MACOS else which("g++", "c++"))
    cxxflags = ["-std=c++17", "-fPIC", "-DVSG_SHARED_LIBRARY", "-DVSGXCHANGE_SHARED_LIBRARY", f"-I{inc}"]
    ldflags = [f"-L{lib}", f"-Wl,-rpath,{lib}"]
    if not MACOS:
        ldflags.append(f"-Wl,-rpath-link,{lib}")
    settings = {"FPM_CXX": cxx, "FPM_CXXFLAGS": " ".join(cxxflags), "FPM_LDFLAGS": " ".join(ldflags)}
    for key in ("FPM_CXXFLAGS", "FPM_LDFLAGS"):
        extra = os.environ.get(key, "").strip()
        if extra:
            settings[key] = f"{settings[key]} {extra}"
    return settings


def main(argv):
    if not argv:
        sys.exit(__doc__)
    settings = fpm_settings()
    if argv == ["--env"]:
        for k, v in settings.items():
            print(f"export {k}={shlex.quote(v)}")
        return 0
    env = dict(os.environ, **settings)
    cmd = [which("fpm"), *argv]
    print("+", " ".join(cmd), flush=True)
    return subprocess.call(cmd, env=env)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
