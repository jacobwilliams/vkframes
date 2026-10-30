"""Build and run the vkframes Qt demo. Run in the pixi `qt` environment, e.g.
`pixi run -e qt qt-demo` from the vkframes root.

Usage:
    python demo.py build
    python demo.py run [demo options...]

fpm needs the settings of both C++ layers: vkframes' (VSG flags and the
compiled shaders, from ../../tools/fpm_vsg.py) and qt-fortran's (Qt flags,
from its tools/fpm_qt.py, which keeps FPM_CXXFLAGS/FPM_LDFLAGS already set).
"""

import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
sys.path.insert(0, os.path.join(ROOT, "tools"))
import fpm_vsg  # noqa: E402

QT_TOOL = os.path.join(HERE, "build", "dependencies", "qt-fortran", "tools", "fpm_qt.py")


def main(argv):
    if not argv or argv[0] not in ("build", "run"):
        sys.exit(__doc__)
    os.chdir(HERE)
    fpm_vsg.update_dependencies(HERE)                    # fetch qt-fortran
    settings, dll_dir = fpm_vsg.settings()
    env = dict(os.environ, **settings)
    # vkframes' data files (the star catalog), found by name
    env["VKFRAMES_DATA_PATH"] = os.pathsep.join(p for p in (os.path.join(ROOT, "data"), env.get("VKFRAMES_DATA_PATH")) if p)
    if dll_dir:   # Windows: vkf.dll
        env["PATH"] = dll_dir + os.pathsep + env.get("PATH", "")
    args = ["build"] if argv[0] == "build" else ["run", "--", *argv[1:]]
    return subprocess.call([sys.executable, QT_TOOL, *args], env=env)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
