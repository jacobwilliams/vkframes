"""Build and run the vkframes halo demo (an Earth-Moon L1 halo orbit's
invariant manifolds and zero-velocity curves, CR3BP). Run with pixi from
the vkframes root: `pixi run halo-demo`.

Usage:
    python demo.py build
    python demo.py run [demo options...]

fpm needs vkframes' C++ layer settings (VSG flags and the compiled shaders,
from ../../tools/fpm_vsg.py).
"""

import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
sys.path.insert(0, os.path.join(ROOT, "tools"))
import fpm_vsg  # noqa: E402


def main(argv):
    if not argv or argv[0] not in ("build", "run"):
        sys.exit(__doc__)
    os.chdir(HERE)
    settings, dll_dir = fpm_vsg.settings()
    env = dict(os.environ, **settings)
    if dll_dir:   # Windows: vkf.dll
        env["PATH"] = dll_dir + os.pathsep + env.get("PATH", "")
    # a relative file name: from where pixi was run (it runs this in demos/halo)
    start = os.environ.get("INIT_CWD", HERE)
    demo_args = [a if a.startswith("-") or os.path.isabs(a) or a.isdigit() else os.path.join(start, a)
                 for a in argv[1:]]
    fc = os.environ.get("FPM_FC") or fpm_vsg.which("gfortran")
    cmd = [fpm_vsg.which("fpm"), argv[0], "--compiler", fc, "--profile", "release"]
    if argv[0] == "run":
        cmd += ["--", *demo_args]
    return subprocess.call(cmd, env=env)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
