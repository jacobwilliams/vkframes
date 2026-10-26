"""Run fpm with the C++ compiler, VSG and linker settings that vkframes needs.

Run inside the pixi environment (e.g. `pixi run build`), from the project root.

Usage:
    python tools/fpm_vsg.py <fpm command> [fpm args...]   e.g. build, test, run --example hello_sphere
    python tools/fpm_vsg.py --env                         print `export ...` lines; then plain
                                                          `fpm build` works in that shell

Linux and macOS: fpm compiles the C++ layer (src/vsg/*.cpp) as part of the
vkframes library. This script supplies the settings that depend on the
environment, through fpm's standard environment variables:
    FPM_CXX       clang++ (macOS) / g++
    FPM_CXXFLAGS  -std=c++17 -fPIC, VSG's compile definitions, the env's include dir
    FPM_LDFLAGS   the env's library directory and rpath, VSG, Vulkan and the C++
                  runtime (and the Objective-C runtime on macOS)

Windows: conda-forge's VSG is MSVC-built, so the layer is built separately with
clang++ (MSVC target) into build/vkf/vkf.dll, which fpm links (see
build_layer_dll); the DLL's directory is put on PATH for `run` and `test`.

VSG ships only CMake package files (no pkg-config), so the flags its CMake
targets carry are reproduced here: VSG_SHARED_LIBRARY, VSGXCHANGE_SHARED_LIBRARY,
C++17 and <prefix>/include.

Rocky (maps and terrain, optional): when Rocky is installed in the
environment (the `rocky` pixi environment, `pixi run -e rocky rocky-build`),
the layer is built with it (VKF_ROCKY, C++20 as Rocky's headers need, Rocky's
spdlog/fmt definitions, -lrocky -lgdal), and ROCKY_FILE_PATH points Rocky at its
shaders; otherwise src/vsg/vkf_rocky.cpp only reports Rocky as unavailable.
The two builds go to separate build directories (build/ and build/rocky/:
fpm does not rebuild C++ sources when only FPM_CXXFLAGS change).

Before running fpm, the shaders in shaders/ are compiled to SPIR-V with
glslangValidator and written into src/vsg/vkf_shaders.cpp (generated, not
committed; declared in src/vsg/vkf_shaders.h). The file is rewritten only when
its contents change, so fpm recompiles it only then.

FPM_CXXFLAGS and FPM_LDFLAGS already set in the environment are kept (appended).
"""

import glob
import os
import shlex
import shutil
import struct
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SHADER_DIR = os.path.join(ROOT, "shaders")
SHADER_CPP = os.path.join(ROOT, "src", "vsg", "vkf_shaders.cpp")
WINDOWS = os.name == "nt"
MACOS = sys.platform == "darwin"


def which(*names):
    for n in names:
        if shutil.which(n):
            return n
    sys.exit(f"fpm_vsg: none of {names} found on PATH")


def prefix():
    p = os.environ.get("CONDA_PREFIX")
    if not p:
        sys.exit("fpm_vsg: CONDA_PREFIX is not set; run inside the pixi environment (pixi run ...)")
    return os.path.join(p, "Library") if WINDOWS else p


LIBS = ["vsgXchange", "vsg"]                  # plus Vulkan: libvulkan, or vulkan-1 on Windows
VSG_DEFINES = ["-DVSG_SHARED_LIBRARY", "-DVSGXCHANGE_SHARED_LIBRARY"]
LAYER_SRC = os.path.join(ROOT, "src", "vsg")


def rocky_installed(pre):
    """Whether Rocky is installed in the environment prefix pre."""
    names = ["rocky.lib"] if WINDOWS else ["librocky.dylib", "librocky.so"]
    return (os.path.exists(os.path.join(pre, "include", "rocky", "rocky.h"))
            and any(os.path.exists(os.path.join(pre, "lib", n)) for n in names))


# Rocky's compile definitions (its CMake target's: spdlog and fmt as shared libraries)
ROCKY_DEFINES = ["-DVKF_ROCKY", "-DFMT_SHARED", "-DSPDLOG_COMPILED_LIB", "-DSPDLOG_FMT_EXTERNAL", "-DSPDLOG_SHARED_LIB"]


def keep_user_flags(settings):
    """Append FPM_CXXFLAGS / FPM_LDFLAGS already set in the environment."""
    for key in ("FPM_CXXFLAGS", "FPM_LDFLAGS"):
        extra = os.environ.get(key, "").strip()
        if extra:
            settings[key] = f"{settings[key]} {extra}"
    return settings


# ---------------------------------------------------------------------------
# Linux and macOS: fpm builds the layer

def unix_settings():
    """FPM_CXX, FPM_CXXFLAGS and FPM_LDFLAGS for building the C++ layer with fpm."""
    pre = prefix()
    inc = os.path.join(pre, "include")
    lib = os.path.join(pre, "lib")
    cxx = os.environ.get("FPM_CXX") or (which("clang++") if MACOS else which("g++", "c++"))
    cxxflags = ["-std=c++17", "-fPIC", *VSG_DEFINES, f"-I{inc}"]
    libs = [f"-l{n}" for n in LIBS] + ["-lvulkan"]
    if rocky_installed(pre):
        cxxflags[0] = "-std=c++20"
        cxxflags += ROCKY_DEFINES
        if MACOS:
            cxxflags.append("-D_LIBCPP_DISABLE_AVAILABILITY")   # (as Rocky is built: tools/build_rocky.py)
        libs[:0] = ["-lrocky", "-lgdal"]          # (GDAL: the layer samples elevation files itself)
    if MACOS:
        # C++ runtime, and the Objective-C runtime (the layer's child NSView for embedding)
        ldflags = [f"-L{lib}", f"-Wl,-rpath,{lib}", *libs, "-lc++", "-lobjc"]
    else:
        # --no-as-needed: these come before the objects on fpm's link line
        ldflags = [f"-L{lib}", f"-Wl,-rpath,{lib}", f"-Wl,-rpath-link,{lib}", "-Wl,--no-as-needed", *libs, "-lstdc++"]
    return {"FPM_CXX": cxx, "FPM_CXXFLAGS": " ".join(cxxflags), "FPM_LDFLAGS": " ".join(ldflags)}, None


# ---------------------------------------------------------------------------
# Windows: conda-forge's VSG is built with MSVC, which gfortran (MinGW) cannot
# link against, so the layer is built with clang++ (MSVC target) into
# build/vkf/vkf.dll, which has a plain C interface (qt-fortran does the same
# for its Qt shim). fpm compiles the layer's sources as empty files
# (-DVKF_EXTERNAL_LAYER) and links the DLL; the DLL and VSG are found through
# PATH at run time.

def build_dir():
    """fpm's build directory: build, or build/rocky for the build with Rocky."""
    return os.path.join("build", "rocky") if rocky_installed(prefix()) else "build"


def build_layer_dll(lib):
    """Build <build dir>/vkf/vkf.dll from src/vsg/*.cpp if any source is newer."""
    out_dir = os.path.join(os.getcwd(), build_dir(), "vkf")
    dll = os.path.join(out_dir, "vkf.dll")
    sources = sorted(glob.glob(os.path.join(LAYER_SRC, "*.cpp")))
    deps = sources + glob.glob(os.path.join(LAYER_SRC, "*.h"))
    if os.path.exists(dll) and all(os.path.getmtime(d) <= os.path.getmtime(dll) for d in deps):
        return dll, False
    os.makedirs(out_dir, exist_ok=True)
    cxx = os.environ.get("VKF_CXX") or which("clang++")
    rocky = rocky_installed(lib)
    cmd = [cxx, "--target=x86_64-pc-windows-msvc", "-std=c++20" if rocky else "-std=c++17", "-O2",
           # VSG's DLLs use the dynamic CRT (/MD); keep the static one out of the link
           "-fms-runtime-lib=dll", "-shared", "-o", dll,
           *(f"-Wl,/NODEFAULTLIB:{crt}" for crt in ("libcmt", "libucrt", "libvcruntime", "libcpmt")),
           "-DNOMINMAX", *VSG_DEFINES, *(ROCKY_DEFINES if rocky else []), f"-I{os.path.join(lib, 'include')}",
           *sources,
           f"-L{os.path.join(lib, 'lib')}", *(["-lrocky", "-lgdal"] if rocky else []), *(f"-l{n}" for n in LIBS), "-lvulkan-1"]
    print("+", " ".join(cmd), flush=True)
    if subprocess.call(cmd) != 0:
        sys.exit("fpm_vsg: building build/vkf/vkf.dll failed")
    return dll, True


def windows_settings():
    lib = prefix()
    dll, rebuilt = build_layer_dll(lib)
    if rebuilt:
        # fpm does not relink executables when only the DLL changed
        for d in glob.glob(os.path.join(os.getcwd(), build_dir(), "gfortran_*")):
            shutil.rmtree(d, ignore_errors=True)
    # The DLL is passed as a linker input: ld resolves its C symbols directly.
    settings = {
        "FPM_CXX": os.environ.get("FPM_CXX") or which("clang++", "g++"),
        "FPM_CXXFLAGS": "-DVKF_EXTERNAL_LAYER",
        "FPM_LDFLAGS": dll,
    }
    return settings, os.path.dirname(dll)


def settings():
    """Compile the shaders, then return (fpm environment settings, directory to
    put on PATH or None)."""
    compile_shaders()
    s, dll_dir = windows_settings() if WINDOWS else unix_settings()
    # vkframes' data files (star catalog, Earth textures) found by name at run time
    s["VKFRAMES_DATA_PATH"] = os.pathsep.join(
        d for d in (os.path.join(ROOT, "data"), os.environ.get("VKFRAMES_DATA_PATH")) if d)
    if rocky_installed(prefix()):
        # Rocky's shaders and data (installed in <prefix>/share/rocky)
        s["ROCKY_FILE_PATH"] = os.pathsep.join(
            d for d in (os.path.join(prefix(), "share", "rocky"), os.environ.get("ROCKY_FILE_PATH")) if d)
    return keep_user_flags(s), dll_dir


def spirv(path, tmpdir):
    """SPIR-V words of the GLSL shader at path."""
    out = os.path.join(tmpdir, os.path.basename(path) + ".spv")
    cmd = [which("glslangValidator"), "-V", "--target-env", "vulkan1.0", "-o", out, path]
    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.returncode != 0:
        sys.exit(f"fpm_vsg: shader compilation failed:\n{result.stdout}{result.stderr}")
    with open(out, "rb") as f:
        code = f.read()
    return struct.unpack(f"<{len(code) // 4}I", code)


def compile_shaders():
    """Write src/vsg/vkf_shaders.cpp with the SPIR-V of shaders/*.vert|frag."""
    shaders = sorted(glob.glob(os.path.join(SHADER_DIR, "*.vert")) + glob.glob(os.path.join(SHADER_DIR, "*.frag")))
    lines = ["// Generated by tools/fpm_vsg.py from shaders/; do not edit.", "",
             "#ifndef VKF_EXTERNAL_LAYER  // Windows: built into vkf.dll instead (see fpm_vsg.py)", "",
             '#include "vkf_shaders.h"', "", "namespace vkf_shaders", "{"]
    with tempfile.TemporaryDirectory() as tmpdir:
        for path in shaders:
            name = os.path.basename(path).replace(".", "_")
            words = spirv(path, tmpdir)
            lines.append(f"    const std::vector<uint32_t> {name} = {{")
            for i in range(0, len(words), 8):
                lines.append("        " + ", ".join(f"0x{w:08x}" for w in words[i:i + 8]) + ",")
            lines.append("    };")
    lines += ["} // namespace vkf_shaders", "", "#endif // VKF_EXTERNAL_LAYER", ""]
    text = "\n".join(lines)
    old = None
    if os.path.exists(SHADER_CPP):
        with open(SHADER_CPP) as f:
            old = f.read()
    if text != old:
        with open(SHADER_CPP, "w") as f:
            f.write(text)
        print(f"fpm_vsg: compiled {len(shaders)} shaders into {os.path.relpath(SHADER_CPP, ROOT)}", file=sys.stderr)


def main(argv):
    if not argv:
        sys.exit(__doc__)
    env_settings, dll_dir = settings()
    if argv == ["--env"]:
        for k, v in env_settings.items():
            print(f"export {k}={shlex.quote(v)}")
        if dll_dir:
            print(f"export PATH={shlex.quote(dll_dir)}{os.pathsep}\"$PATH\"")
        return 0
    env = dict(os.environ, **env_settings)
    if dll_dir:
        env["PATH"] = dll_dir + os.pathsep + env.get("PATH", "")
    fc = os.environ.get("FPM_FC") or which("gfortran")
    cmd = [which("fpm"), argv[0], "--compiler", fc, "--build-dir", build_dir(), *argv[1:]]
    print("+", " ".join(cmd), flush=True)
    return subprocess.call(cmd, env=env)


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
