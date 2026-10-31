"""Build and run the vkframes trajectory design demo (initial position and
velocity arrows dragged with the mouse, the orbit following, in a qt-fortran
GUI). Run in the pixi `qt` environment, e.g. `pixi run -e qt design-demo`
from the vkframes root.

Usage:
    python demo.py build
    python demo.py run [demo options...]

fpm needs the settings of both C++ layers: vkframes' (VSG flags and the
compiled shaders, from ../../tools/fpm_vsg.py) and qt-fortran's (Qt flags,
from its tools/fpm_qt.py, which keeps FPM_CXXFLAGS/FPM_LDFLAGS already set).
"""

import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, os.path.join(os.path.dirname(os.path.dirname(HERE)), "tools"))
import fpm_vsg  # noqa: E402

if __name__ == "__main__":
    sys.exit(fpm_vsg.run_qt_project(HERE, sys.argv[1:], __doc__))
