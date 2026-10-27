"""Build and run the vkframes benchmark app (bench/README.md). Run in the pixi
`qt` environment, e.g. `pixi run -e qt bench-vkframes -- --scenario grow`
from the vkframes root.

Usage:
    python demo.py build
    python demo.py run [benchmark options...]

As the Qt demo's demo.py: fpm gets vkframes' C++ layer settings (from
../../tools/fpm_vsg.py) and qt-fortran's (its tools/fpm_qt.py). Built in
release mode, as the OpenFrames app's library is.
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
    env["FPM_CXXFLAGS"] = env.get("FPM_CXXFLAGS", "") + " -O2"   # the C++ layer optimized too (OpenFrames: a Release build)
    env["VKFRAMES_DATA_PATH"] = os.pathsep.join(p for p in (os.path.join(ROOT, "data"), env.get("VKFRAMES_DATA_PATH")) if p)
    if dll_dir:   # Windows: vkf.dll
        env["PATH"] = dll_dir + os.pathsep + env.get("PATH", "")
    # a relative --out or --capture: from where pixi was run (it runs this in bench/vkframes)
    start = os.environ.get("INIT_CWD", HERE)
    args = list(argv[1:])
    for k, a in enumerate(args):
        if k > 0 and args[k - 1] in ("--out", "--capture") and not os.path.isabs(a):
            args[k] = os.path.join(start, a)
    fpm = ["build", "--profile", "release"] if argv[0] == "build" else ["run", "--profile", "release", "--", *args]
    return subprocess.call([sys.executable, QT_TOOL, *fpm], env=env)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
