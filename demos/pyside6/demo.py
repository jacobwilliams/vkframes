"""Build and run the vkframes PySide6 demo. Run in the pixi environment, e.g.
`pixi run pyside6-demo` from the vkframes root.

Usage:
    python demo.py build
    python demo.py run [app options...]     (see app.py)

fpm builds the demo's Fortran (src/vkdemo.f90) and vkframes as shared
libraries (fpm.toml: [library] type = "shared"), with vkframes' C++ layer
settings from ../../tools/fpm_vsg.py. The app loads the library with ctypes:
VKDEMO_LIB names it, and the build directory goes on the library search path
(for vkframes' library, which it needs). VKFRAMES_DATA_PATH gets vkframes'
data/ (the star catalog).
"""

import glob
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
sys.path.insert(0, os.path.join(ROOT, "tools"))
import fpm_vsg  # noqa: E402

LIBRARY = {"win32": "vkdemo.dll", "darwin": "libvkdemo.dylib"}.get(sys.platform, "libvkdemo.so")


def build(env):
    return subprocess.call(["fpm", "build", "--compiler", "gfortran"], cwd=HERE, env=env)


def library_path():
    """The most recently built demo library."""
    found = glob.glob(os.path.join(HERE, "build", "gfortran_*", LIBRARY))
    if not found:
        sys.exit("demo.py: the library was not built")
    return max(found, key=os.path.getmtime)


def main(argv):
    if not argv or argv[0] not in ("build", "run"):
        sys.exit(__doc__)
    settings, dll_dir = fpm_vsg.settings()
    env = dict(os.environ, **settings)
    status = build(env)
    if status != 0 or argv[0] == "build":
        return status

    lib = library_path()
    lib_dir = os.path.dirname(lib)
    search = {"win32": "PATH", "darwin": "DYLD_LIBRARY_PATH"}.get(sys.platform, "LD_LIBRARY_PATH")
    dirs = [lib_dir] + ([dll_dir] if dll_dir else []) + [env.get(search, "")]
    env[search] = os.pathsep.join(d for d in dirs if d)
    env["VKDEMO_LIB"] = lib
    env["VKFRAMES_DATA_PATH"] = os.pathsep.join(
        d for d in (os.path.join(ROOT, "data"), env.get("VKFRAMES_DATA_PATH")) if d)
    return subprocess.call([sys.executable, os.path.join(HERE, "app.py"), *argv[1:]], env=env)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
