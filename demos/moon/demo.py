"""Build and run the vkframes lunar demo (Rocky terrain in a qt-fortran GUI).
Run in the pixi `rocky` environment after `pixi run -e rocky rocky-build`
(and `pixi run -e rocky moon-data` for the Moon's elevation), e.g.
`pixi run -e rocky moon-demo` from the vkframes root.

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
    if not os.path.exists(QT_TOOL):
        subprocess.check_call(["fpm", "update"])          # fetch qt-fortran
    settings, dll_dir = fpm_vsg.settings()
    env = dict(os.environ, **settings)
    # vkframes' data files (the star catalog, the Moon's elevation), found by name
    env["VKFRAMES_DATA_PATH"] = os.pathsep.join(p for p in (os.path.join(ROOT, "data"), env.get("VKFRAMES_DATA_PATH")) if p)
    if dll_dir:   # Windows: vkf.dll
        env["PATH"] = dll_dir + os.pathsep + env.get("PATH", "")
    args = ["build"] if argv[0] == "build" else ["run", "--", *argv[1:]]
    return subprocess.call([sys.executable, QT_TOOL, *args], env=env)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
