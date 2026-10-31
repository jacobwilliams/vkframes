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
shaders (vkframes' copies of the two it changes first: shaders/rocky);
otherwise src/vsg/vkf_rocky.cpp only reports Rocky as unavailable.
The two builds go to separate build directories (build/ and build/rocky/:
fpm does not rebuild C++ sources when only FPM_CXXFLAGS change).

Before running fpm, the shaders in shaders/ are compiled to SPIR-V with
glslangValidator and written into src/vsg/vkf_shaders.cpp (generated, not
committed), with the GLSL source of vkframes' copies of VSG's shaders
(shaders/vsg/), which VSG compiles itself; src/vsg/vkf_shaders.h, which
declares them, is generated too (and committed). Only when a shader file is
newer than vkf_shaders.cpp, or a shader was added or removed. The file is rewritten only when
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
SHADER_H = os.path.join(ROOT, "src", "vsg", "vkf_shaders.h")
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


LIBS = ["vsgImGui", "imgui", "vsgXchange", "vsg"]   # plus Vulkan: libvulkan, or vulkan-1 on Windows
VSG_DEFINES = ["-DVSG_SHARED_LIBRARY", "-DVSGXCHANGE_SHARED_LIBRARY"]
LAYER_SRC = os.path.join(ROOT, "src", "vsg")


def rocky_installed(pre):
    """Whether Rocky is installed in the environment prefix pre."""
    names = ["rocky.lib"] if WINDOWS else ["librocky.dylib", "librocky.so"]
    return (os.path.exists(os.path.join(pre, "include", "rocky", "rocky.h"))
            and any(os.path.exists(os.path.join(pre, "lib", n)) for n in names))


# Rocky's compile definitions (its CMake target's: spdlog and fmt as shared libraries)
ROCKY_DEFINES = ["-DVKF_ROCKY", "-DFMT_SHARED", "-DSPDLOG_COMPILED_LIB", "-DSPDLOG_FMT_EXTERNAL", "-DSPDLOG_SHARED_LIB"]


def update_dependencies(project_dir):
    """Run `fpm update` in project_dir if a git dependency is missing or not at the rev in fpm.toml.

    fpm would fetch it itself during the build, but only after qt-fortran's fpm_qt.py has
    decided whether its shim DLL (Windows) is up to date, so the DLL would be stale.
    """
    import tomllib
    with open(os.path.join(project_dir, "fpm.toml"), "rb") as f:
        deps = tomllib.load(f).get("dependencies", {})
    cache = {}
    cache_file = os.path.join(project_dir, "build", "cache.toml")
    if os.path.exists(cache_file):
        with open(cache_file, "rb") as f:
            cache = tomllib.load(f)
    stale = [name for name, d in deps.items()
             if isinstance(d, dict) and "git" in d
             and (not os.path.isdir(os.path.join(project_dir, "build", "dependencies", name))
                  or ("rev" in d and cache.get(name, {}).get("obj") != d["rev"]))]
    if stale:
        subprocess.check_call(["fpm", "update"], cwd=project_dir)


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
        # --no-as-needed: these come before the objects on fpm's link line; -lpthread: the
        # layer's std::thread (conda's sysroot has a glibc older than 2.34, its own libpthread)
        ldflags = [f"-L{lib}", f"-Wl,-rpath,{lib}", f"-Wl,-rpath-link,{lib}", "-Wl,--no-as-needed", *libs, "-lstdc++",
                   "-lpthread"]
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
    # (forward slashes: fpm drops backslashes from the link flags of some projects, e.g. demos/cr3bp)
    cxx = os.environ.get("FPM_CXX") or which("clang++", "g++")
    cxxflags = ["-DVKF_EXTERNAL_LAYER"]
    if "clang" in os.path.basename(cxx):
        # the stubs are empty; MinGW target as fpm's release profile adds -fPIC, an error for MSVC's
        cxxflags.insert(0, "--target=x86_64-w64-windows-gnu")
    settings = {
        "FPM_CXX": cxx,
        "FPM_CXXFLAGS": " ".join(cxxflags),
        "FPM_LDFLAGS": dll.replace("\\", "/"),
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
        # Rocky's shaders and data: vkframes' copies of the shaders it changes
        # first (shaders/rocky/shaders, found as Rocky's "shaders/<name>"),
        # then Rocky's (installed in <prefix>/share/rocky)
        s["ROCKY_FILE_PATH"] = os.pathsep.join(
            d for d in (os.path.join(SHADER_DIR, "rocky"), os.path.join(prefix(), "share", "rocky"),
                        os.environ.get("ROCKY_FILE_PATH")) if d)
    return keep_user_flags(s), dll_dir


# Shaders also compiled as variants: shaders/<file> as vkf_shaders::<name> with
# the preprocessor definition given (lines without dashes, highlights or
# trails, and dashed lines without highlights or trails: leaner shaders, much
# faster where vertices dominate).
SHADER_VARIANTS = {
    "line.vert": [("line_plain_vert", "VKF_LINE_PLAIN"), ("line_dashed_vert", "VKF_LINE_DASHED")],
    "line.frag": [("line_plain_frag", "VKF_LINE_PLAIN"), ("line_dashed_frag", "VKF_LINE_DASHED")],
}


def spirv(path, tmpdir, define=None):
    """SPIR-V words of the GLSL shader at path (with the preprocessor
    definition `define`, if given)."""
    out = os.path.join(tmpdir, os.path.basename(path) + (f".{define}" if define else "") + ".spv")
    cmd = [which("glslangValidator"), "-V", "--target-env", "vulkan1.0", *([f"-D{define}"] if define else []), "-o", out, path]
    result = subprocess.run(cmd, capture_output=True, text=True)
    if result.returncode != 0:
        sys.exit(f"fpm_vsg: shader compilation failed:\n{result.stdout}{result.stderr}")
    with open(out, "rb") as f:
        code = f.read()
    return struct.unpack(f"<{len(code) // 4}I", code)


def write_if_changed(path, text):
    """Write text to path unless it already holds it (so fpm recompiles only
    what changed); True if written."""
    old = None
    if os.path.exists(path):
        with open(path) as f:
            old = f.read()
    if text == old:
        return False
    with open(path, "w") as f:
        f.write(text)
    return True


def compile_shaders():
    """Write src/vsg/vkf_shaders.cpp with the SPIR-V of shaders/*.vert|frag,
    and the GLSL source of shaders/vsg/*.frag (vkframes' copies of VSG's
    shaders, which VSG compiles itself, a variant per set of #defines), and
    src/vsg/vkf_shaders.h declaring them; only when a shader file is newer
    than vkf_shaders.cpp (or one was added or removed: the header changes)."""
    shaders = sorted(glob.glob(os.path.join(SHADER_DIR, "*.vert")) + glob.glob(os.path.join(SHADER_DIR, "*.frag")))
    sources = sorted(glob.glob(os.path.join(SHADER_DIR, "vsg", "*.frag")))
    spirv_names = [os.path.basename(p).replace(".", "_") for p in shaders]
    spirv_names += [name for p in shaders for name, _ in SHADER_VARIANTS.get(os.path.basename(p), [])]
    source_names = [os.path.basename(p).replace(".", "_") + "_glsl" for p in sources]
    header = "\n".join([
        "// Generated by tools/fpm_vsg.py from shaders/; do not edit.",
        "//",
        "// SPIR-V of the shaders in shaders/ (compiled at build time with",
        "// glslangValidator: shaders/name.ext becomes vkf_shaders::name_ext), and the",
        "// GLSL source of vkframes' copies of VSG's shaders (shaders/vsg/), which VSG",
        "// compiles itself (a variant per set of #defines). The definitions are in",
        "// vkf_shaders.cpp.",
        "",
        "#pragma once",
        "",
        "#include <cstdint>",
        "#include <vector>",
        "",
        "namespace vkf_shaders",
        "{",
        *[f"    extern const std::vector<uint32_t> {n};" for n in spirv_names],
        "",
        *[f"    extern const char* const {n};" for n in source_names],
        "} // namespace vkf_shaders",
        ""])
    header_changed = write_if_changed(SHADER_H, header)
    newest = max((os.path.getmtime(p) for p in shaders + sources), default=0.0)
    if not header_changed and os.path.exists(SHADER_CPP) and os.path.getmtime(SHADER_CPP) >= newest:
        return
    lines = ["// Generated by tools/fpm_vsg.py from shaders/; do not edit.", "",
             "#ifndef VKF_EXTERNAL_LAYER  // Windows: built into vkf.dll instead (see fpm_vsg.py)", "",
             '#include "vkf_shaders.h"', "", "namespace vkf_shaders", "{"]
    with tempfile.TemporaryDirectory() as tmpdir:
        for path in shaders:
            variants = [(os.path.basename(path).replace(".", "_"), None)] + SHADER_VARIANTS.get(os.path.basename(path), [])
            for name, define in variants:
                words = spirv(path, tmpdir, define)
                lines.append(f"    const std::vector<uint32_t> {name} = {{")
                for i in range(0, len(words), 8):
                    lines.append("        " + ", ".join(f"0x{w:08x}" for w in words[i:i + 8]) + ",")
                lines.append("    };")
    for path in sources:
        name = os.path.basename(path).replace(".", "_") + "_glsl"
        with open(path) as f:
            glsl = f.read()
        if ")glsl" in glsl:
            sys.exit(f"fpm_vsg: {path} contains the raw string delimiter )glsl")
        lines.append(f'    const char* const {name} = R"glsl({glsl})glsl";')
    lines += ["} // namespace vkf_shaders", "", "#endif // VKF_EXTERNAL_LAYER", ""]
    if write_if_changed(SHADER_CPP, "\n".join(lines)):
        print(f"fpm_vsg: compiled {len(shaders)} shaders into {os.path.relpath(SHADER_CPP, ROOT)}", file=sys.stderr)
    else:
        os.utime(SHADER_CPP)                  # (up to date: newer than the shaders, so not compiled again)


# Options of the demos and the benchmark whose values are files or folders
PATH_OPTIONS = ("--capture", "--out", "--data", "--save-as")


def demo_arguments(args, start):
    """A demo's arguments, relative paths made absolute from `start` (where
    pixi was run; pixi runs a demo in its own folder): the values of
    PATH_OPTIONS always (e.g. a capture still to be written), other
    arguments if they name a file or folder there (e.g. the trajectory
    demo's file)."""
    out = []
    for k, a in enumerate(args):
        option = args[k - 1] if k > 0 else None
        if not (a.startswith("-") or os.path.isabs(a)) and \
                (option in PATH_OPTIONS or os.path.exists(os.path.join(start, a))):
            a = os.path.join(start, a)
        out.append(a)
    return out


def run_qt_project(here, argv, usage, release=False, cxxflags=""):
    """The main() of a qt-fortran project's demo.py (the demos, the
    benchmark's vkframes app): `build` it, or `run` it with the arguments
    after `run` (demo_arguments). fpm runs through qt-fortran's
    tools/fpm_qt.py (Qt's settings, keeping vkframes' from settings());
    `release`: fpm's release profile; `cxxflags`: added to the C++ flags."""
    if not argv or argv[0] not in ("build", "run"):
        sys.exit(usage)
    os.chdir(here)
    update_dependencies(here)                         # (qt-fortran fetched: its fpm_qt.py)
    env_settings, dll_dir = settings()
    env = dict(os.environ, **env_settings)
    if cxxflags:
        env["FPM_CXXFLAGS"] = (env.get("FPM_CXXFLAGS", "") + " " + cxxflags).strip()
    if dll_dir:                                       # Windows: vkf.dll
        env["PATH"] = dll_dir + os.pathsep + env.get("PATH", "")
    profile = ["--profile", "release"] if release else []
    if argv[0] == "build":
        fpm = ["build", *profile]
    else:
        fpm = ["run", *profile, "--", *demo_arguments(argv[1:], os.environ.get("INIT_CWD", here))]
    qt_tool = os.path.join(here, "build", "dependencies", "qt-fortran", "tools", "fpm_qt.py")
    return subprocess.call([sys.executable, qt_tool, *fpm], env=env)


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
