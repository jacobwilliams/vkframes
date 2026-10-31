// vkframes C++ layer: the cache of converted fonts and compiled shaders (see
// vkf_cache.h).

#ifndef VKF_EXTERNAL_LAYER  // Windows: built into vkf.dll instead (see fpm_vsg.py)

#include "vkf_cache.h"
#include "vkf_common.h"

#include <vsg/all.h>

#include <chrono>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <fstream>
#include <iostream>
#include <iterator>
#include <sstream>
#include <string>
#include <vector>

#if defined(_WIN32)
#    include <process.h>    // _getpid
#else
#    include <unistd.h>     // getpid
#endif

namespace vkf
{
    // An environment variable's value ('' if unset).
    std::string env_var(const char* name)
    {
#if defined(_WIN32)
        char* value = nullptr;
        size_t len = 0;
        if (_dupenv_s(&value, &len, name) != 0 || !value) return {};
        std::string result(value);
        std::free(value);
        return result;
#else
        const char* value = std::getenv(name);
        return value ? std::string(value) : std::string();
#endif
    }

    // The directory for converted fonts and compiled shaders (vkf_cache_clear
    // deletes them), without creating it: VKFRAMES_CACHE_DIR, or the user's
    // cache directory; empty if none.
    std::filesystem::path cache_path()
    {
        namespace fs = std::filesystem;
        if (auto d = env_var("VKFRAMES_CACHE_DIR"); !d.empty()) return fs::path(d);
#if defined(_WIN32)
        if (auto d = env_var("LOCALAPPDATA"); !d.empty()) return fs::path(d) / "vkframes" / "cache";
#elif defined(__APPLE__)
        if (auto h = env_var("HOME"); !h.empty()) return fs::path(h) / "Library" / "Caches" / "vkframes";
#else
        if (auto d = env_var("XDG_CACHE_HOME"); !d.empty()) return fs::path(d) / "vkframes";
        if (auto h = env_var("HOME"); !h.empty()) return fs::path(h) / ".cache" / "vkframes";
#endif
        return {};
    }

    // The cache directory (cache_path), created if it doesn't exist (with its
    // parents: e.g. a VKFRAMES_CACHE_DIR not made yet). Empty if there is none,
    // or it can't be created (said once): then nothing is cached.
    std::filesystem::path cache_dir()
    {
        namespace fs = std::filesystem;
        auto dir = cache_path();
        if (dir.empty()) return dir;
        std::error_code ec;
        if (fs::is_directory(dir, ec)) return dir;
        fs::create_directories(dir, ec);
        if (fs::is_directory(dir, ec)) return dir;
        static bool warned = false;
        if (!warned) std::cerr << "vkframes: no cache: could not create " << dir.string() << std::endl;
        warned = true;
        return {};
    }

    // A 64-bit FNV-1a hash of `bytes` (continuing from `h`), as 16 hex digits
    // by cache_key: the cache's file names, the same with any compiler and
    // standard library (std::hash's are not).
    uint64_t fnv1a(const std::string& bytes, uint64_t h = 14695981039346656037ull)
    {
        for (unsigned char c : bytes)
        {
            h ^= c;
            h *= 1099511628211ull;
        }
        return h;
    }

    std::string cache_key(const std::string& bytes)
    {
        char hex[17];
        std::snprintf(hex, sizeof hex, "%016llx", static_cast<unsigned long long>(fnv1a(bytes)));
        return hex;
    }

    // The cache file of a font file: its name, and a hash of its contents and
    // VSG's version (copies of the same font, e.g. in several environments,
    // share it; a changed font, or another VSG, is converted again). Empty if
    // there is no cache directory, or the font can't be read.
    std::filesystem::path font_cache_file(const std::filesystem::path& font)
    {
        const auto dir = cache_dir();
        if (dir.empty()) return {};
        std::ifstream in(font, std::ios::binary);
        if (!in) return {};
        std::string bytes((std::istreambuf_iterator<char>(in)), std::istreambuf_iterator<char>());
        return dir / "fonts" / (font.stem().string() + "-" + cache_key(bytes + "|" + VSG_VERSION_STRING) + ".vsgb");
    }

    // Remove the other cache files of the font that `kept` (in the cache's
    // fonts directory) is a cache file of: the font's older conversions (the
    // font changed, or VSG), in the fonts directory, and those from before
    // fonts had their directory, in the cache directory (from before fonts
    // were named by their contents, one per copy of the font). They are the
    // files named <the font's name>-<key>.vsgb, the key without a '-' (so
    // DejaVuSans-Bold's are not DejaVuSans's).
    void prune_font_cache(const std::filesystem::path& kept)
    {
        namespace fs = std::filesystem;
        const auto name = kept.stem().string();
        const auto font = name.substr(0, name.rfind('-'));
        for (const auto& dir : {kept.parent_path(), kept.parent_path().parent_path()})
        {
            std::error_code ec;
            for (const auto& entry : fs::directory_iterator(dir, ec))
            {
                const auto& path = entry.path();
                const auto stem = path.stem().string();
                if (path.extension() != ".vsgb" || path == kept) continue;
                if (stem.size() <= font.size() + 1 || stem.compare(0, font.size() + 1, font + "-") != 0) continue;
                if (stem.find('-', font.size() + 1) != std::string::npos) continue;   // (another font's: font-Bold-...)
                fs::remove(path, ec);
            }
        }
    }

    // Save a file in the cache, safely with other programs using it: written
    // (by `write`, given the path to write) to a temporary file of this
    // process's own (its id and the time in its name), then renamed into place,
    // which replaces a file in one step (readers see the old file or the new
    // one, never part of one). If the rename fails (on Windows while another
    // program reads the file) or anything else does, the temporary file is
    // removed and the cache left as it is.
    template <class Write>
    void write_cache_file(const std::filesystem::path& cached, Write write)
    {
        namespace fs = std::filesystem;
        std::error_code ec;
        fs::create_directories(cached.parent_path(), ec);
    #if defined(_WIN32)
        const auto pid = _getpid();
    #else
        const auto pid = getpid();
    #endif
        auto tmp = cached;
        tmp.replace_extension(".tmp" + std::to_string(pid) + "-" +
                              std::to_string(std::chrono::steady_clock::now().time_since_epoch().count()) +
                              cached.extension().string());
        const bool written = write(tmp) && fs::file_size(tmp, ec) > 0 && !ec;
        if (written) fs::rename(tmp, cached, ec);
        if (!written || ec) fs::remove(tmp, ec);
    }

    // Save a converted font in the cache (written with `options`: vsgXchange's
    // writers), and remove the font's other cache files (prune_font_cache).
    void write_font_cache(const vsg::ref_ptr<vsg::Font>& font, const std::filesystem::path& cached,
                          const vsg::ref_ptr<const vsg::Options>& options)
    {
        write_cache_file(cached, [&](const std::filesystem::path& tmp) { return vsg::write(font, tmp.string(), options); });
        if (std::filesystem::exists(cached)) prune_font_cache(cached);
    }

    // Shader stages compiled from GLSL (vkframes' Phong and PBR fragment
    // stages, see use_fragment_source, and VSG's vertex stage with them): VSG
    // compiles them with glslang when their pipeline is first compiled, at
    // every launch (about 45 ms for a Phong variant, on the first frame).
    // Instead, their SPIR-V is kept in the cache (cache_dir), named for the
    // stage, its source and settings and VSG's version, and read from there.
    // The stages of each pipeline under `root` that have no SPIR-V get it from
    // the cache, or are compiled here (for Vulkan 1.0, VSG's default: valid
    // with any instance) and saved. Without a cache directory, VSG compiles
    // them as before.
    void use_cached_shaders(vsg::Object& root)
    {
        namespace fs = std::filesystem;
        const auto dir = cache_dir();
        if (dir.empty()) return;
        struct Stages : vsg::Visitor
        {
            std::vector<vsg::ShaderStages> pipelines;
            void apply(vsg::Node& node) override { node.traverse(*this); }
            void apply(vsg::StateGroup& group) override
            {
                for (auto& command : group.stateCommands) command->accept(*this);
                group.traverse(*this);
            }
            void apply(vsg::BindGraphicsPipeline& bind) override
            {
                if (bind.pipeline) pipelines.push_back(bind.pipeline->stages);
            }
        } stages;
        root.accept(stages);

        auto file_of = [&](const vsg::ShaderStage& stage) {
            const auto& module = *stage.module;
            std::ostringstream key;
            key << stage.stage << '|' << stage.entryPointName << '|' << module.source << '|' << VSG_VERSION_STRING;
            if (const auto& h = module.hints)
            {
                key << '|' << h->vulkanVersion << ',' << h->clientInputVersion << ',' << h->language << ',' << h->defaultVersion
                    << ',' << h->target << ',' << h->forwardCompatible << ',' << h->generateDebugInfo << ',' << h->optimize;
                for (const auto& define : h->defines) key << ',' << define;
            }
            return dir / "shaders" / (cache_key(key.str()) + ".spv");
        };
        for (auto& pipeline : stages.pipelines)
        {
            vsg::ShaderStages uncached;
            for (auto& stage : pipeline)
            {
                if (!stage || !stage->module || !stage->module->code.empty() || stage->module->source.empty()) continue;
                std::ifstream in(file_of(*stage), std::ios::binary | std::ios::ate);
                const auto bytes = in ? static_cast<std::size_t>(in.tellg()) : 0;
                vsg::ShaderModule::SPIRV code(bytes / 4);
                in.seekg(0);
                if (bytes >= 20 && bytes % 4 == 0 && in.read(reinterpret_cast<char*>(code.data()), bytes) && code[0] == 0x07230203u)
                    stage->module->code = std::move(code);
                else
                    uncached.push_back(stage);
            }
            if (uncached.empty()) continue;
            vsg::ShaderCompiler compiler;
            if (!compiler.compile(uncached)) continue;  // (VSG tries again, and reports the errors)
            for (auto& stage : uncached)
            {
                const auto& code = stage->module->code;
                if (code.empty()) continue;
                write_cache_file(file_of(*stage), [&](const fs::path& tmp) {
                    std::ofstream out(tmp, std::ios::binary);
                    return static_cast<bool>(out.write(reinterpret_cast<const char*>(code.data()), code.size() * 4));
                });
            }
        }
    }
} // namespace vkf

extern "C" {

// The cache directory (cache_dir: VKFRAMES_CACHE_DIR, or the user's cache
// directory; created if it doesn't exist) into `buf` (not null-terminated);
// returns its length (0: there is none, or it can't be created), or -1 if
// `buf` is too short.
VKF_API int vkf_cache_directory(char* buf, int buflen)
{
    const auto dir = vkf::cache_dir().string();
    if (static_cast<int>(dir.size()) > buflen) return -1;
    std::memcpy(buf, dir.data(), dir.size());
    return static_cast<int>(dir.size());
}

// Delete the cache's files: the converted fonts (fonts/*.vsgb, and *.vsgb from
// before fonts had their directory), the compiled shaders (shaders/*.spv) and
// temporary files left by a program stopped while
// writing one; then the fonts and shaders directories, if left empty. Nothing
// else is removed, the cache directory itself kept (VKFRAMES_CACHE_DIR may be
// a directory of the user's, shared with other files). They are
// made again when next needed. Returns the number of files removed, or -1 if
// some couldn't be.
VKF_API int vkf_cache_clear()
{
    try
    {
        namespace fs = std::filesystem;
        const auto dir = vkf::cache_path();                 // (not created: nothing to clear then)
        if (dir.empty()) return 0;
        int removed = 0;
        bool failed = false;
        auto clear = [&](const fs::path& d, const std::string& extension) {
            std::error_code ec;
            for (const auto& entry : fs::directory_iterator(d, ec))
            {
                // (temporary files too: name.tmp<pid>-<time>.vsgb, see write_cache_file)
                if (entry.path().extension() != extension || !entry.is_regular_file(ec)) continue;
                if (fs::remove(entry.path(), ec)) ++removed;
                else failed = true;
            }
        };
        clear(dir / "fonts", ".vsgb");
        clear(dir / "shaders", ".spv");
        clear(dir, ".vsgb");                                // (fonts from before they had their directory)
        std::error_code ec;
        if (fs::is_empty(dir / "fonts", ec)) fs::remove(dir / "fonts", ec);
        if (fs::is_empty(dir / "shaders", ec)) fs::remove(dir / "shaders", ec);
        return failed ? -1 : removed;
    }
    catch (const std::exception& e)
    {
        std::cerr << "vkf_cache_clear: " << e.what() << std::endl;
        return -1;
    }
}

} // extern "C"

#endif // VKF_EXTERNAL_LAYER
