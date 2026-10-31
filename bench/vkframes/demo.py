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
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(HERE)), "tools"))
import fpm_vsg  # noqa: E402

if __name__ == "__main__":
    sys.exit(fpm_vsg.run_qt_project(HERE, sys.argv[1:], __doc__, release=True, cxxflags="-O2"))
