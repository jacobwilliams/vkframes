// vkframes C++ layer: the cache (vkf_cache.cpp) of what is slow to make
// again at every launch: converted fonts (their glyphs' signed distance
// fields: seconds to compute) and compiled shaders (SPIR-V of the stages VSG
// compiles from GLSL: about 45 ms each). The cache directory is
// VKFRAMES_CACHE_DIR, or the user's cache directory (cache_dir); without one,
// nothing is cached. vkf_cache_directory and vkf_cache_clear (Fortran:
// vkf_cache's cache_directory and clear_cache) say where it is and delete it.
#pragma once

#include <filesystem>

#include <vsg/core/Object.h>
#include <vsg/core/ref_ptr.h>
#include <vsg/io/Options.h>
#include <vsg/text/Font.h>

namespace vkf
{
    // The cache directory: VKFRAMES_CACHE_DIR, or the user's cache directory; empty if none.
    std::filesystem::path cache_dir();

    // The cache file of a font file (named for its contents); empty without a cache directory.
    std::filesystem::path font_cache_file(const std::filesystem::path& font);

    // Save a converted font in the cache (written with `options`), the font's older entries removed.
    void write_font_cache(const vsg::ref_ptr<vsg::Font>& font, const std::filesystem::path& cached,
                          const vsg::ref_ptr<const vsg::Options>& options);

    // The stages of the pipelines under `root` compiled from GLSL given their SPIR-V from the cache
    // (or compiled, and saved).
    void use_cached_shaders(vsg::Object& root);
} // namespace vkf
