"""Build Rocky (https://github.com/pelicanmapping/rocky, maps and terrain on
VSG) from source and install it into the active pixi environment, for the
optional Rocky support (`pixi run -e rocky rocky-build`).

Rocky is not on conda-forge; its dependencies are (see pixi.toml's rocky
feature). The source is a pinned commit, cloned into build/rocky-src.
Options: --clean (configure from scratch).
"""

import os
import re
import shutil
import subprocess
import sys
from pathlib import Path

ROCKY_URL = "https://github.com/pelicanmapping/rocky.git"
ROCKY_COMMIT = "a792a5f49378cae8f5c2572def7cf2c7b54d8e7e"   # main, 2026-10-21 (version 1.1.0)

root = Path(__file__).resolve().parent.parent
src = root / "build" / "rocky-src"
build = root / "build" / "rocky-build"
prefix = Path(os.environ["CONDA_PREFIX"])
if sys.platform == "win32":
    prefix = prefix / "Library"   # conda's Windows layout (where VSG is, and where tools/fpm_vsg.py looks)


def run(*cmd, cwd=None):
    print("+", " ".join(str(c) for c in cmd), flush=True)
    subprocess.run([str(c) for c in cmd], cwd=cwd, check=True)


def patch():
    """Fixes to Rocky's C++ for conda-forge's compilers and packages, and for
    vkframes. (vkframes' changes to Rocky's shaders are copies of them, in
    shaders/rocky/shaders, which Rocky finds first: see tools/fpm_vsg.py.)"""
    # Rocky is compiled as C++20 (EnTT 4.0 requires it), where
    # unordered_map::contains, in SRSFactory (an unordered_map), hides
    # SRS.cpp's own contains(string, pattern)
    srs = src / "src" / "rocky" / "SRS.cpp"
    text = srs.read_text()
    fixed = re.sub(r"(?<![:\w.])contains\((ndef|new_entry)", r"::contains(\1", text)
    if fixed != text:
        srs.write_text(fixed)
    elif "::contains(ndef" not in text:
        sys.exit("build_rocky: src/rocky/SRS.cpp: the contains() calls to qualify are not there (Rocky changed)")
    # only the library and rocky_simple: the tests (Catch2's RNG fails
    # libc++'s checks) and rocky_demo (u8 strings as std::string, C++17
    # style) don't build with libc++ in C++20
    replace(src / "src" / "CMakeLists.txt", "add_subdirectory(tests)", "# add_subdirectory(tests)")
    replace(src / "src" / "apps" / "CMakeLists.txt", "        add_subdirectory(rocky_demo)", "        # add_subdirectory(rocky_demo)")
    # vsgXchange's readers come before Rocky's GDAL reader (meant for what
    # vsgXchange can't read, TIFF and WebP): JPEG and PNG are decoded by stb
    # as sRGB RGBA (GDAL decodes a greyscale JPEG as one-channel 0-255
    # floats, which the terrain draws red)
    replace(src / "src" / "rocky" / "vsg" / "VSGContext.cpp",
            "#ifdef ROCKY_HAS_GDAL\n    readerWriterOptions->add(GDAL_VSG_ReaderWriter::create());\n#endif\n\n"
            "#ifdef ROCKY_HAS_VSGXCHANGE\n    // Adds all the readerwriters in vsgxchange to the options data.\n"
            "    readerWriterOptions->add(vsgXchange::all::create());\n#endif\n",
            "#ifdef ROCKY_HAS_VSGXCHANGE\n    // Adds all the readerwriters in vsgxchange to the options data.\n"
            "    readerWriterOptions->add(vsgXchange::all::create());\n#endif\n\n"
            "#ifdef ROCKY_HAS_GDAL\n    readerWriterOptions->add(GDAL_VSG_ReaderWriter::create()); // (vkframes: after vsgXchange's)\n#endif\n")


def replace(path, old, new):
    """Replace `old` by `new` in the file at `path` (once applied, nothing to
    do). Stops the build if neither is there: Rocky's source has changed (a
    new commit), and the fix must be redone."""
    text = path.read_text()
    if new in text:
        return
    if old not in text:
        sys.exit(f"build_rocky: {path.relative_to(src)}: the text to replace is not there "
                 f"(Rocky changed; update tools/build_rocky.py):\n{old}")
    path.write_text(text.replace(old, new))


def main():
    if "--clean" in sys.argv and build.exists():
        shutil.rmtree(build)
    if not src.exists():
        run("git", "clone", "--filter=blob:none", ROCKY_URL, src)
    run("git", "fetch", "--quiet", "origin", ROCKY_COMMIT, cwd=src)
    run("git", "checkout", "--quiet", "--force", ROCKY_COMMIT, cwd=src)   # (drops earlier patches)
    patch()

    options = {
        "CMAKE_BUILD_TYPE": "Release",
        "CMAKE_INSTALL_PREFIX": prefix,
        "CMAKE_PREFIX_PATH": prefix,
        "ROCKY_SUPPORTS_IMGUI": "ON",                 # (needed: Application.cpp uses it even when off)
        "ROCKY_SUPPORTS_SLUGHORN": "OFF",
        "ROCKY_SUPPORTS_AZURE": "OFF",
        "ROCKY_SUPPORTS_QT": "OFF",
        "ROCKY_SUPPORTS_CURL": "OFF",
        "ROCKY_SUPPORTS_HTTPLIB": "ON",              # (conda-forge's libcurl has no CMake config)
        "ROCKY_SUPPORTS_HTTPS": "ON",
        "ROCKY_SUPPORTS_GDAL": "ON",
        "ROCKY_SUPPORTS_MBTILES": "ON",
    }
    if sys.platform == "darwin":
        # conda-forge's libc++ (which the build links) has floating-point
        # std::from_chars; its availability markup is for the system's
        options["CMAKE_CXX_FLAGS"] = "-D_LIBCPP_DISABLE_AVAILABILITY"
        # cpp-httplib reads the system's certificates (HTTPS)
        frameworks = "-framework Security -framework CoreFoundation"
        options["CMAKE_SHARED_LINKER_FLAGS"] = frameworks
        options["CMAKE_EXE_LINKER_FLAGS"] = frameworks
    run("cmake", "-S", src, "-B", build, "-G", "Ninja", *[f"-D{k}={v}" for k, v in options.items()])
    run("cmake", "--build", build)
    run("cmake", "--install", build)


if __name__ == "__main__":
    main()
