// SPIR-V of the shaders in shaders/, compiled at build time.
//
// The definitions are in vkf_shaders.cpp, which tools/fpm_vsg.py generates
// from shaders/*.vert and shaders/*.frag with glslangValidator (a shader
// shaders/name.ext becomes vkf_shaders::name_ext).

#pragma once

#include <cstdint>
#include <vector>

namespace vkf_shaders
{
    extern const std::vector<uint32_t> line_vert;
    extern const std::vector<uint32_t> line_frag;
    extern const std::vector<uint32_t> label_vert;
    extern const std::vector<uint32_t> label_frag;
    extern const std::vector<uint32_t> marker_vert;
    extern const std::vector<uint32_t> marker_frag;
    extern const std::vector<uint32_t> sky_vert;
    extern const std::vector<uint32_t> sky_frag;
    extern const std::vector<uint32_t> stars_vert;
    extern const std::vector<uint32_t> stars_frag;
} // namespace vkf_shaders
