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


def run(*cmd, cwd=None):
    print("+", " ".join(str(c) for c in cmd), flush=True)
    subprocess.run([str(c) for c in cmd], cwd=cwd, check=True)


def patch():
    """Fixes for building with conda-forge's compilers and packages."""
    # libc++ has unordered_map::contains also in C++17: in SRSFactory (an
    # unordered_map) it hides SRS.cpp's own contains(string, pattern)
    srs = src / "src" / "rocky" / "SRS.cpp"
    text = srs.read_text()
    fixed = re.sub(r"(?<![:\w.])contains\((ndef|new_entry)", r"::contains(\1", text)
    if fixed != text:
        srs.write_text(fixed)
    # only the library and rocky_simple: the tests (Catch2's RNG fails
    # libc++'s checks) and rocky_demo (u8 strings as std::string, C++17
    # style) don't build with libc++ in C++20
    replace(src / "src" / "CMakeLists.txt", "add_subdirectory(tests)", "# add_subdirectory(tests)")
    replace(src / "src" / "apps" / "CMakeLists.txt", "        add_subdirectory(rocky_demo)", "        # add_subdirectory(rocky_demo)")
    # vkframes draws into UNORM framebuffers (colors in display values, as in
    # OpenGL); Rocky lights in linear color for sRGB framebuffers: with
    # VKF_UNORM_FRAMEBUFFER defined (src/vsg/vkf_rocky.cpp), the terrain
    # encodes its color to sRGB itself
    replace(src / "src" / "rocky" / "vsg" / "shaders" / "rocky.terrain.frag",
            "    applyDebugTriangles(outColor, u_terrain.debugTriangles);\n}",
            "    applyDebugTriangles(outColor, u_terrain.debugTriangles);\n"
            "\n#ifdef VKF_UNORM_FRAMEBUFFER\n"
            "    // (vkframes) encode to sRGB: the framebuffer does not\n"
            "    vec3 c = clamp(outColor.rgb, 0.0, 1.0);\n"
            "    outColor.rgb = mix(12.92 * c, 1.055 * pow(c, vec3(1.0 / 2.4)) - 0.055, step(0.0031308, c));\n"
            "#endif\n}")
    # an image stream is read by the first reader for its type (as vsg::read
    # does; Rocky's search kept the last, and a nested composite reader
    # without one reset it), and vsgXchange's readers come before Rocky's
    # GDAL fallback (meant for what vsgXchange can't stream, TIFF and WebP):
    # JPEG and PNG are decoded by stb, as sRGB (GDAL decodes greyscale JPEG
    # as 0-255 floats)
    replace(src / "src" / "rocky" / "vsg" / "VSGContext.cpp",
            "#ifdef ROCKY_HAS_GDAL\n    readerWriterOptions->add(GDAL_VSG_ReaderWriter::create());\n#endif\n\n"
            "#ifdef ROCKY_HAS_VSGXCHANGE\n    // Adds all the readerwriters in vsgxchange to the options data.\n"
            "    readerWriterOptions->add(vsgXchange::all::create());\n#endif\n",
            "#ifdef ROCKY_HAS_VSGXCHANGE\n    // Adds all the readerwriters in vsgxchange to the options data.\n"
            "    readerWriterOptions->add(vsgXchange::all::create());\n#endif\n\n"
            "#ifdef ROCKY_HAS_GDAL\n    readerWriterOptions->add(GDAL_VSG_ReaderWriter::create()); // (vkframes: after vsgXchange's)\n#endif\n")
    replace(src / "src" / "rocky" / "vsg" / "VSGContext.cpp",
            "                output = findReaderWriter(extension, crw->readerWriters);\n",
            "                output = findReaderWriter(extension, crw->readerWriters);\n"
            "                if (output) return output; // (vkframes) the first reader\n")
    replace(src / "src" / "rocky" / "vsg" / "VSGContext.cpp",
            "                        output = rw;\n",
            "                        return rw; // (vkframes) the first reader\n")
    # 8-bit one- and two-channel imagery (greyscale) as RGBA: the terrain
    # samples color
    replace(src / "src" / "rocky" / "ImageLayer.cpp",
            "                return Failure_ResourceUnavailable;\n            }\n        }\n    }\n\n    return r;\n}",
            "                return Failure_ResourceUnavailable;\n            }\n        }\n    }\n\n"
            "    // (vkframes) one- and two-channel imagery (greyscale) as RGBA: the terrain samples color\n"
            "    if (r.ok() && r.value().image() && r.value().image()->pixelFormat() <= Image::R8G8_SRGB)\n"
            "    {\n"
            "        auto in = r.value().image();\n"
            "        // 8-bit greyscale as sRGB, as color images (JPEG and PNG pixels are display values)\n"
            "        const bool unorm8 = in->pixelFormat() == Image::R8_UNORM || in->pixelFormat() == Image::R8G8_UNORM;\n"
            "        const bool srgb = unorm8 || in->pixelFormat() == Image::R8_SRGB || in->pixelFormat() == Image::R8G8_SRGB;\n"
            "        const bool alpha = in->numComponents() == 2;\n"
            "        auto out = Image::create(srgb ? Image::R8G8B8A8_SRGB : Image::R8G8B8A8_UNORM, in->width(), in->height(), in->depth());\n"
            "        for (unsigned l = 0; l < in->depth(); ++l)\n"
            "            for (unsigned t = 0; t < in->height(); ++t)\n"
            "                for (unsigned s = 0; s < in->width(); ++s)\n"
            "                {\n"
            "                    auto p = in->read(s, t, l);\n"
            "                    float v = p.r;\n"
            "                    if (unorm8) v = (v <= 0.04045f) ? v / 12.92f : std::pow((v + 0.055f) / 1.055f, 2.4f); // (write encodes)\n"
            "                    out->write(Image::Pixel(v, v, v, alpha ? p.g : 1.0f), s, t, l);\n"
            "                }\n"
            "        r = GeoImage(out, r.value().extent());\n"
            "    }\n\n"
            "    return r;\n}")
    # vkframes' lights (src/vsg/vkf_layer.cpp, "Lighting"): a light's diffuse
    # part is a VSG light, its specular part another of negative color; Rocky
    # applied both parts of every light (the negative ones darkening)
    lighting = src / "src" / "rocky" / "vsg" / "shaders" / "rocky.lighting.glsl"
    replace(lighting, "        vec3 radiance = light_color * intensity;\n",
            "        vec3 radiance = abs(light_color) * intensity;\n"
            "        bool specularOnly = any(lessThan(light_color, vec3(0.0))); // (vkframes) a specular part\n")
    replace(lighting, "        vec3 radiance = light_color;\n",
            "        vec3 radiance = abs(light_color);\n"
            "        bool specularOnly = any(lessThan(light_color, vec3(0.0))); // (vkframes) a specular part\n")
    replace(lighting, "        Lo += (diffuse + specular) * radiance * NdotL;\n",
            "        Lo += (specularOnly ? specular : diffuse) * radiance * NdotL; // (vkframes) diffuse, or specular\n")
    replace(src / "src" / "rocky" / "vsg" / "shaders" / "rocky.terrain.frag",
            "#pragma import_defines(ROCKY_HAS_ATMOSPHERE, ROCKY_HAS_SLUGHORN)",
            "#pragma import_defines(ROCKY_HAS_ATMOSPHERE, ROCKY_HAS_SLUGHORN, VKF_UNORM_FRAMEBUFFER)")


def replace(path, old, new):
    text = path.read_text()
    if new not in text:
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
