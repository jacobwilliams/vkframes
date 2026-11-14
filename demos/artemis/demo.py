"""Build and run the vkframes Artemis I demo (NASA's Artemis I mission from its
trajectory, in a qt-fortran GUI). Run in the pixi `qt` environment, e.g.
`pixi run -e qt artemis-demo` from the vkframes root.

Usage:
    python demo.py build
    python demo.py run [demo options...]

The trajectory data (data/artemis1/, from JPL Horizons) are downloaded first
if missing (tools/get_artemis1.py, `pixi run artemis-data`).
"""

import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(os.path.dirname(HERE))
sys.path.insert(0, os.path.join(ROOT, "tools"))
import fpm_vsg  # noqa: E402

if __name__ == "__main__":
    if sys.argv[1:2] == ["run"] and not os.path.exists(os.path.join(ROOT, "data", "artemis1", "events.txt")):
        subprocess.run([sys.executable, os.path.join(ROOT, "tools", "get_artemis1.py")], check=True)
    sys.exit(fpm_vsg.run_qt_project(HERE, sys.argv[1:], __doc__))
