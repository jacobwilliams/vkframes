"""Build OpenFrames from source, and build/run the OpenFrames benchmark app
(bench/README.md). Run in the pixi `openframes` environment, from the vkframes
root:

    pixi run -e openframes openframes-build      once: clone and build OpenFrames
    pixi run -e openframes bench-openframes -- --scenario grow ...

Usage:
    python demo.py openframes        clone OpenFrames (pinned commit), build and install it in build/
    python demo.py build             build the app with fpm
    python demo.py run [options...]  run it (benchmark options: see bench_common)

As qt-fortran's OpenFrames demo (demos/openframes there): OpenFrames' libraries
are built with CMake (Release) against conda-forge's OpenSceneGraph; the app
is built with qt-fortran's tools/fpm_qt.py, plus OpenFrames' library
directory and an rpath to it.

OpenFrames is stock OpenFrames (github.com/ravidavi/OpenFrames), pinned to the
commit compared in plans/openframes-compare.md (the same as qt-fortran's
OpenFrames demo).
"""

import glob
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
BUILD = os.path.join(HERE, "build")
OF_REPO = "https://github.com/ravidavi/OpenFrames.git"
OF_COMMIT = "798dc4a"                       # master (2026-03-02)
OF_SRC = os.path.join(BUILD, "OpenFrames")
OF_BUILD = os.path.join(BUILD, "openframes-build")
OF_PREFIX = os.path.join(BUILD, "openframes-install")
QT_TOOL = os.path.join(BUILD, "dependencies", "qt-fortran", "tools", "fpm_qt.py")


def run(cmd, env=None, cwd=None):
    print("+", " ".join(cmd), flush=True)
    if subprocess.call(cmd, env=env, cwd=cwd) != 0:
        sys.exit(f"demo.py: failed: {' '.join(cmd)}")


def openframes():
    """Clone, configure, build and install OpenFrames (only the libraries)."""
    if not os.path.isdir(OF_SRC):
        run(["git", "clone", OF_REPO, OF_SRC])
    run(["git", "-C", OF_SRC, "checkout", "-q", OF_COMMIT])
    prefix = os.environ["CONDA_PREFIX"]
    run(["cmake", "-S", OF_SRC, "-B", OF_BUILD, "-G", "Ninja",
         "-DCMAKE_BUILD_TYPE=Release", f"-DCMAKE_INSTALL_PREFIX={OF_PREFIX}",
         f"-DOSG_DIR={prefix}", f"-DCMAKE_PREFIX_PATH={prefix}",
         "-DOF_BUILD_DEMOS=OFF", "-DOF_PYTHON_MODULE=OFF", "-DOF_FORTRAN_MODULE=OFF", "-DOF_QT_SUPPORT=OFF"])
    run(["cmake", "--build", OF_BUILD])
    run(["cmake", "--install", OF_BUILD])


def libdir():
    for d in ("lib", "lib64"):
        if glob.glob(os.path.join(OF_PREFIX, d, "*OpenFrames_Interface_C*")):
            return os.path.join(OF_PREFIX, d)
    sys.exit("demo.py: OpenFrames is not built yet: run `pixi run -e openframes openframes-build` first")


def fpm(command, args):
    """Run fpm (release) through qt-fortran's fpm_qt.py, adding the OpenFrames library."""
    lib = libdir()
    os.chdir(HERE)
    if not os.path.exists(QT_TOOL):
        run(["fpm", "update"])                              # fetch qt-fortran and openframes-fortran
    env = dict(os.environ)
    env["FPM_LDFLAGS"] = f"{env.get('FPM_LDFLAGS', '')} -L{lib} -Wl,-rpath,{lib}".strip()
    env["FPM_CXXFLAGS"] = f"{env.get('FPM_CXXFLAGS', '')} -O2".strip()
    # a relative --out or --capture: from where pixi was run (it runs this in bench/openframes)
    start = os.environ.get("INIT_CWD", HERE)
    args = list(args)
    for k, a in enumerate(args):
        if k > 0 and args[k - 1] in ("--out", "--capture") and not os.path.isabs(a):
            args[k] = os.path.join(start, a)
    fpm_args = [command, "--profile", "release"] + (["--"] + args if command == "run" else [])
    run([sys.executable, QT_TOOL, *fpm_args], env)


def main():
    if len(sys.argv) < 2 or sys.argv[1] not in ("openframes", "build", "run"):
        sys.exit(__doc__)
    if sys.argv[1] == "openframes":
        openframes()
    else:
        fpm(sys.argv[1], sys.argv[2:])


if __name__ == "__main__":
    main()
