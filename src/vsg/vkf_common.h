// vkframes C++ layer: what its source files (vkf_layer.cpp, vkf_rocky.cpp,
// vkf_gui.cpp) share.
#pragma once

#include <string>

// the layer's C functions, called from Fortran (exported from vkf.dll on Windows)
#if defined(_WIN32)
#    define VKF_API __declspec(dllexport)
#else
#    define VKF_API
#endif

namespace vkf
{
    // A Fortran string (its characters and length) as a std::string: empty for
    // none (a null pointer, or no length).
    inline std::string to_string(const char* s, int len)
    {
        return (s && len > 0) ? std::string(s, static_cast<size_t>(len)) : std::string();
    }
} // namespace vkf
