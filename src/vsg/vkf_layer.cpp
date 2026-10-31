// vkframes C++ VSG layer: generic rendering primitives behind an extern "C"
// interface, called from Fortran (src/vsg/vkf_layer.f90).
//
// Conventions:
//   - objects cross the boundary as opaque pointers; the layer owns them and
//     Fortran releases them with the matching *_delete call;
//   - strings cross as (pointer, length), not NUL-terminated;
//   - matrices are 4x4 doubles, column-major (Fortran's real(c_double) :: m(4,4));
//   - no C++ exception crosses the boundary: each entry point catches and
//     reports failure through its return value.

// On Windows this file is built into vkf.dll by tools/fpm_vsg.py, and fpm
// compiles it as an empty file (VKF_EXTERNAL_LAYER).
#ifndef VKF_EXTERNAL_LAYER

#include "vkf_common.h"

using vkf::to_string;

#if defined(_WIN32)
#    include <windows.h>
#endif
#if defined(__APPLE__)
#    define VK_USE_PLATFORM_METAL_EXT    // VK_EXT_metal_surface (see "macOS windows")
#endif

#include "vkf_shaders.h"

#include <vsg/all.h>
#include <vsgXchange/all.h>

#include "vkf_cache.h"
#if defined(_WIN32)
#    include <process.h>    // _getpid
#    include <ole2.h>
#    include <vsg/platform/win32/Win32_Window.h>
#    pragma comment(lib, "ole32.lib")   // RevokeDragDrop
#endif

#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <csignal>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <deque>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <memory>
#include <mutex>
#include <set>
#include <sstream>
#include <string>
#include <thread>
#include <typeinfo>

#if !defined(_WIN32)
#    include <unistd.h>     // getpid
#endif
#if defined(__APPLE__)
#    include <CoreGraphics/CGGeometry.h>
#    include <cxxabi.h>
#    include <objc/message.h>
#    include <objc/runtime.h>
#endif

namespace vkf
{
    // optional parts of the layer (vkf_rocky.cpp): release their state for a
    // viewer being deleted (after vkframes dropped its own references to it)
    void release_viewer(vsg::Viewer* viewer);
    // ... and the device features they need, and their state for a view (before the first compile)
    void device_features(vsg::WindowTraits& traits);
    bool prepare_view(vsg::Viewer* viewer, vsg::View& view, vsg::ref_ptr<vsg::Device> device);

    // GUI panels (vkf_gui.cpp): made for a viewer on first use, attached once
    // its command graph exists, cleared after each draw, deleted with it
    struct Gui;
    Gui* gui_create(float pixel_ratio, const std::string& font, float size);
    void gui_attach(Gui* gui, vsg::ref_ptr<vsg::Window> window, vsg::Viewer& viewer, vsg::CommandGraph& commandGraph);
    bool gui_attached(const Gui* gui);
    void gui_frame_done(Gui* gui);
    void gui_delete(Gui* gui, vsg::Viewer& viewer);
} // namespace vkf

namespace
{
    // Copy s into a Fortran character buffer (blank padded); returns the length of s.
    int to_fortran(const std::string& s, char* buf, int buflen)
    {
        if (buf && buflen > 0)
        {
            const size_t n = std::min(s.size(), static_cast<size_t>(buflen));
            std::memcpy(buf, s.data(), n);
            std::memset(buf + n, ' ', static_cast<size_t>(buflen) - n);
        }
        return static_cast<int>(s.size());
    }

    // The message of the vsg::Exception being handled (by its type name, see guarded).
    std::string current_vsg_exception_message()
    {
#if defined(__APPLE__)
        const std::type_info* type = abi::__cxa_current_exception_type();
        if (type && std::strcmp(type->name(), typeid(vsg::Exception).name()) == 0)
        {
            void* object = abi::__cxa_current_primary_exception();
            const auto* e = static_cast<const vsg::Exception*>(object);
            std::string message = e->message + " (VkResult " + std::to_string(e->result) + ")";
            abi::__cxa_decrement_exception_refcount(object);
            return message;
        }
        if (type) return std::string("exception of type ") + type->name();
#endif
        return "unknown exception";
    }

    // Seconds a deferred resize waits for the size to stop changing (macOS,
    // embedded windows: see disable_layer_animations).
    constexpr double resize_settle = 0.1;

#if defined(__APPLE__)
    // The class of the view VSG's own macOS windows render into (VSG 1.1's
    // vsg_MacOS_NSView, which is private to VSG: see accept_first_mouse_in_vsg_windows
    // and new_window_layer).
    constexpr const char* vsg_view_class = "vsg_MacOS_NSView";

    // The view VSG renders into: an NSView that is invisible to hit-testing
    // (hitTest: returns nil), so the window gives every mouse event straight to
    // the host's view under it. As a plain NSView, the host's view would only
    // get them through the responder chain, where a left mouse-down is caught
    // up in window activation and first-responder handling (Qt then sees no
    // left drags).
    id render_view_hit_test(id, SEL, CGPoint) { return nil; }

    Class render_view_class()
    {
        static Class cls = [] {
            Class c = objc_allocateClassPair(objc_getClass("NSView"), "VkfRenderView", 0);
            if (!c) return objc_getClass("VkfRenderView");     // already registered
            class_addMethod(c, sel_registerName("hitTest:"), reinterpret_cast<IMP>(render_view_hit_test), "@@:{CGPoint=dd}");
            objc_registerClassPair(c);
            return c;
        }();
        return cls;
    }

    // A view filling `parent` (and resizing with it), for VSG to render into
    // (render_view_class). A host's view may keep its own layer tree (Qt 6's
    // QNSView has a container layer), where Vulkan finds no CAMetalLayer; a
    // plain NSView takes VSG's CAMetalLayer as its layer. Mouse and keys go to
    // `parent` (the host). Objective-C runtime calls, so this stays plain C++.
    void* create_child_view(void* parent)
    {
        auto send = [](auto f, auto... args) { return reinterpret_cast<decltype(f)>(objc_msgSend)(args...); };
        using IdSel = id (*)(id, SEL);
        id p = static_cast<id>(parent);
        CGRect bounds = send(static_cast<CGRect (*)(id, SEL)>(nullptr), p, sel_registerName("bounds"));   // (arm64: no _stret)
        id view = send(static_cast<IdSel>(nullptr), reinterpret_cast<id>(render_view_class()), sel_registerName("alloc"));
        view = send(static_cast<id (*)(id, SEL, CGRect)>(nullptr), view, sel_registerName("initWithFrame:"), bounds);
        // NSViewWidthSizable | NSViewHeightSizable
        send(static_cast<void (*)(id, SEL, unsigned long)>(nullptr), view, sel_registerName("setAutoresizingMask:"), 2ul | 16ul);
        send(static_cast<void (*)(id, SEL, id)>(nullptr), p, sel_registerName("addSubview:"), view);
        return view;
    }

    // VSG's own macOS windows: let their view take the click that activates
    // the window (acceptsFirstMouse:). A program started from a terminal may
    // not be the active application; a click on its window then only activates
    // it and the view never gets that mouse-down, so the trackball, which
    // starts a drag on the button press, ignores the drag (a Cmd-click, which
    // macOS passes to inactive windows, works). VSG's view class does not
    // implement the method, so it is added (once).
    BOOL accepts_first_mouse(id, SEL, id) { return YES; }

    void accept_first_mouse_in_vsg_windows()
    {
        static const bool done = [] {
            Class c = objc_getClass(vsg_view_class);
            SEL selector = sel_registerName("acceptsFirstMouse:");
            if (c && class_getInstanceMethod(c, selector) && class_getMethodImplementation(c, selector) !=
                                                                  class_getMethodImplementation(objc_getClass("NSView"), selector))
                return true;                                    // (VSG's view implements it itself)
            if (c && class_addMethod(c, selector, reinterpret_cast<IMP>(accepts_first_mouse), "B@:@"))   // (arm64: BOOL is bool)
                return true;
            std::cerr << "vkframes: VSG's macOS view class (" << vsg_view_class << ") is not as expected: "
                         "a click that activates a window will not start a drag" << std::endl;
            return false;
        }();
        (void)done;
    }

    // Resizing a window embedded in a host (macOS): rebuilding the swapchain
    // changes the Metal layer's drawable size, which clears what it shows, and
    // during a drag the next frame is shown too late: the window stays black
    // (or flickers). So while the size keeps changing, the swapchain is kept
    // (frames at the old size, with the new aspect ratio, which Core Animation
    // scales to the layer's new size: right proportions, slightly blurred),
    // and rebuilt once the size has stayed the same for resize_settle seconds
    // (vkf_viewer_frame). The perspective's aspect ratio is set for the new
    // size at once (VSG changes it only when the swapchain is rebuilt, by the
    // change of size: it is set back to the old size's just before). The layer also gets no implicit animations (its
    // actions are null). (presentsWithTransaction, which would present with
    // the window's resize, shows nothing with KosmicKrisp.)

    void disable_layer_animations(void* layer)
    {
        id l = static_cast<id>(layer);
        auto cls = [](const char* name) { return reinterpret_cast<id>(objc_getClass(name)); };
        auto str = [&](const char* s) {
            return reinterpret_cast<id (*)(id, SEL, const char*)>(objc_msgSend)(cls("NSString"), sel_registerName("stringWithUTF8String:"), s);
        };
        id null = reinterpret_cast<id (*)(id, SEL)>(objc_msgSend)(cls("NSNull"), sel_registerName("null"));
        const char* keys[] = {"bounds", "position", "frame", "contents", "anchorPoint", "transform"};
        id objects[6], names[6];
        for (int i = 0; i < 6; ++i) { objects[i] = null; names[i] = str(keys[i]); }
        id actions = reinterpret_cast<id (*)(id, SEL, const id*, const id*, unsigned long)>(objc_msgSend)(
            cls("NSDictionary"), sel_registerName("dictionaryWithObjects:forKeys:count:"), objects, names, 6ul);
        reinterpret_cast<void (*)(id, SEL, id)>(objc_msgSend)(l, sel_registerName("setActions:"), actions);
    }

    void delete_child_view(void* view)
    {
        if (!view) return;
        id v = static_cast<id>(view);
        reinterpret_cast<void (*)(id, SEL)>(objc_msgSend)(v, sel_registerName("removeFromSuperview"));
        reinterpret_cast<void (*)(id, SEL)>(objc_msgSend)(v, sel_registerName("release"));
    }

    // macOS windows: vkframes makes their Vulkan instance and surface itself,
    // with the standard VK_EXT_metal_surface: VSG's macOS window asks for
    // MoltenVK's own VK_MVK_macos_surface, which KosmicKrisp (Mesa's Vulkan
    // on Metal, the driver Khronos recommends for Apple silicon) has not. The
    // window is given the instance (setInstance) or the shared device
    // (setDevice: not through traits->device, with which VSG's window makes its
    // own surface at once) and a Metal surface on its view's CAMetalLayer
    // (setSurface).

    // A VkSurfaceKHR on a CAMetalLayer.
    class MetalSurface : public vsg::Surface
    {
    public:
        MetalSurface(vsg::Instance* instance, void* layer) :
            vsg::Surface(VK_NULL_HANDLE, instance)
        {
            VkMetalSurfaceCreateInfoEXT info{};
            info.sType = VK_STRUCTURE_TYPE_METAL_SURFACE_CREATE_INFO_EXT;
            info.pLayer = static_cast<const CAMetalLayer*>(layer);
            auto create = reinterpret_cast<PFN_vkCreateMetalSurfaceEXT>(vkGetInstanceProcAddr(*instance, "vkCreateMetalSurfaceEXT"));
            if (!create || create(*instance, &info, instance->getAllocationCallbacks(), &_surface) != VK_SUCCESS)
                throw vsg::Exception{"Error: vkframes could not create a Metal surface", VK_ERROR_INITIALIZATION_FAILED};
        }
    };

    // The instance of the first window (later ones share its device): as VSG's
    // window makes it, with VK_EXT_metal_surface.
    vsg::ref_ptr<vsg::Instance> metal_instance(vsg::WindowTraits& traits)
    {
        traits.validate();
        vsg::Names extensions = traits.instanceExtensionNames;
        extensions.push_back(VK_KHR_SURFACE_EXTENSION_NAME);
        extensions.push_back(VK_EXT_METAL_SURFACE_EXTENSION_NAME);
        return vsg::Instance::create(extensions, traits.requestedLayers, traits.vulkanVersion);
    }

    // Access to a step of vsg::Window's setup that its share() does (the
    // surface's formats), for a window given a shared device and our surface.
    struct WindowSetup : vsg::Window
    {
        static void init_formats(vsg::Window& window) { (window.*&WindowSetup::_initFormats)(); }
    };

    // The CAMetalLayers given to windows so far (to tell a new window's).
    std::set<void*>& metal_layers()
    {
        static std::set<void*> layers;
        return layers;
    }

    // The CAMetalLayer of the VSG window just made: the layer of the view it
    // renders into, `childView` (an embedded window), or of the content view of
    // the window VSG made (the one with a VSG view whose layer is not taken yet).
    // A minimum content size for the VSG window whose view's layer is
    // `layer`: a window dragged to no height (its bottom edge to the title bar)
    // would make the layer's drawables empty, and Metal neither makes nor
    // gives them (the swapchain can't be built, and acquiring an image never
    // returns).
    void limit_window_size(void* layer)
    {
        auto send = [](id object, const char* selector) {
            return reinterpret_cast<id (*)(id, SEL)>(objc_msgSend)(object, sel_registerName(selector));
        };
        id app = send(reinterpret_cast<id>(objc_getClass("NSApplication")), "sharedApplication");
        id windows = send(app, "windows");
        auto count = reinterpret_cast<unsigned long (*)(id, SEL)>(objc_msgSend)(windows, sel_registerName("count"));
        for (unsigned long i = 0; i < count; ++i)
        {
            id window = reinterpret_cast<id (*)(id, SEL, unsigned long)>(objc_msgSend)(windows, sel_registerName("objectAtIndex:"), i);
            id view = send(window, "contentView");
            if (!view || send(view, "layer") != static_cast<id>(layer)) continue;
            reinterpret_cast<void (*)(id, SEL, CGSize)>(objc_msgSend)(window, sel_registerName("setContentMinSize:"), CGSizeMake(64.0, 48.0));
            return;
        }
    }

    void* new_window_layer(void* childView)
    {
        auto send = [](id object, const char* selector) {
            return reinterpret_cast<id (*)(id, SEL)>(objc_msgSend)(object, sel_registerName(selector));
        };
        id layer = nil;
        if (childView)
        {
            layer = send(static_cast<id>(childView), "layer");
        }
        else
        {
            id app = send(reinterpret_cast<id>(objc_getClass("NSApplication")), "sharedApplication");
            id windows = send(app, "windows");
            auto count = reinterpret_cast<unsigned long (*)(id, SEL)>(objc_msgSend)(windows, sel_registerName("count"));
            for (unsigned long i = count; i-- > 0 && !layer;)
            {
                id window = reinterpret_cast<id (*)(id, SEL, unsigned long)>(objc_msgSend)(windows, sel_registerName("objectAtIndex:"), i);
                id view = send(window, "contentView");
                if (!view || std::strcmp(object_getClassName(view), vsg_view_class) != 0) continue;
                id l = send(view, "layer");
                if (l && !metal_layers().count(l)) layer = l;
            }
        }
        if (!layer) throw vsg::Exception{"Error: vkframes found no CAMetalLayer for the window", VK_ERROR_INITIALIZATION_FAILED};
        metal_layers().insert(layer);
        return layer;
    }
#endif

    // Run f, reporting any exception (vsg::Exception is not a std::exception)
    // and returning fallback instead, so that no exception reaches Fortran.
    template<typename R, typename F>
    R guarded(const char* name, R fallback, F f)
    {
        try
        {
            return f();
        }
        catch (const vsg::Exception& e)
        {
            std::cerr << name << ": " << e.message << " (VkResult " << e.result << ")" << std::endl;
        }
        catch (const std::exception& e)
        {
            std::cerr << name << ": " << e.what() << std::endl;
        }
        catch (...)
        {
            // libvsg exports no type_info for vsg::Exception, so on macOS one thrown
            // inside libvsg doesn't match the catch above; recognize it by name.
            std::cerr << name << ": " << current_vsg_exception_message() << std::endl;
        }
        return fallback;
    }

    // A pipeline drawing from a storage buffer of points (no vertex buffers),
    // shared by all nodes of a kind, in all windows: line strips and segments
    // (shaders/line.vert, line.frag), markers (marker.vert, marker.frag, with a
    // marker image), stars (stars.vert, stars.frag) and the sky texture
    // (sky.vert, sky.frag: no points), ribbons (ribbon.vert, ribbon.frag:
    // translucent surfaces between pairs of points). Bindings: 0 the points, 1
    // the style (or the sky's attitude), 2 an image, 3 the points' colors. The
    // sky kinds are drawn at the far plane without writing depth, stars blended
    // by the brighter color; ribbons are depth tested but write no depth (as
    // the transparent surfaces of vkf_mesh_create).
    enum class PointsKind { lines, markers, stars, skyTexture, ribbons };

    struct PointsPipeline
    {
        vsg::ref_ptr<vsg::DescriptorSetLayout> descriptorSetLayout;
        vsg::ref_ptr<vsg::DescriptorSetLayout> arcLayout;   // lines: set 1, the screen lengths (see ScreenArcs)
        vsg::ref_ptr<vsg::PipelineLayout> pipelineLayout;
        vsg::ref_ptr<vsg::BindGraphicsPipeline> bindPipeline;
        // lines: a highlighted line's glow and arrows (its second pass, see
        // make_line): blended, depth tested but writing no depth (where the
        // glow is faint it doesn't hide what's drawn after it: translucent
        // surfaces), at the line's own depth too (its arrows over it)
        vsg::ref_ptr<vsg::BindGraphicsPipeline> bindGlowPipeline;
        // lines: the leaner variants of the shaders, the same layout and
        // states (see line_variant): plain lines (no dashes, highlight or
        // trail: line_plain_vert, _frag) and dashed lines (no highlight or
        // trail: line_dashed_vert, _frag)
        vsg::ref_ptr<vsg::BindGraphicsPipeline> bindPlainPipeline;
        vsg::ref_ptr<vsg::BindGraphicsPipeline> bindDashedPipeline;

        PointsPipeline(const std::vector<uint32_t>& vert, const std::vector<uint32_t>& frag, PointsKind kind,
                       const std::vector<uint32_t>* plainVert = nullptr, const std::vector<uint32_t>* plainFrag = nullptr,
                       const std::vector<uint32_t>* dashedVert = nullptr, const std::vector<uint32_t>* dashedFrag = nullptr)
        {
            vsg::DescriptorSetLayoutBindings bindings;
            if (kind != PointsKind::skyTexture)
                bindings.push_back({0, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, VK_SHADER_STAGE_VERTEX_BIT, nullptr});
            bindings.push_back({1, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1, VK_SHADER_STAGE_VERTEX_BIT | VK_SHADER_STAGE_FRAGMENT_BIT, nullptr});
            if (kind == PointsKind::markers || kind == PointsKind::skyTexture)
                bindings.push_back({2, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 1, VK_SHADER_STAGE_FRAGMENT_BIT, nullptr});
            if (kind == PointsKind::lines || kind == PointsKind::ribbons || kind == PointsKind::markers)   // the points' colors
                bindings.push_back({3, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, VK_SHADER_STAGE_VERTEX_BIT, nullptr});
            descriptorSetLayout = vsg::DescriptorSetLayout::create(bindings);
            // VSG pushes the projection and model-view matrices (2 x mat4, offset 0);
            // EyeRelative pushes the eye position, the viewport, a line's level of
            // detail and its dash sizes (7 x vec4, offset 128)
            vsg::DescriptorSetLayouts layouts{descriptorSetLayout};
            if (kind == PointsKind::lines)        // set 1: a line's screen lengths, per view (EyeRelative)
            {
                arcLayout = vsg::DescriptorSetLayout::create(vsg::DescriptorSetLayoutBindings{
                    {0, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, VK_SHADER_STAGE_VERTEX_BIT, nullptr}});
                layouts.push_back(arcLayout);
            }
            pipelineLayout = vsg::PipelineLayout::create(layouts, vsg::PushConstantRanges{{VK_SHADER_STAGE_VERTEX_BIT, 0, 240}});
            vsg::ShaderStages stages{
                vsg::ShaderStage::create(VK_SHADER_STAGE_VERTEX_BIT, "main", vsg::ShaderModule::create(vert)),
                vsg::ShaderStage::create(VK_SHADER_STAGE_FRAGMENT_BIT, "main", vsg::ShaderModule::create(frag)),
            };
            auto rasterization = vsg::RasterizationState::create();
            rasterization->cullMode = VK_CULL_MODE_NONE;
            auto colorBlend = vsg::ColorBlendState::create();
            colorBlend->configureAttachments(kind != PointsKind::skyTexture);
            if (kind == PointsKind::stars)
            {
                for (auto& attachment : colorBlend->attachments)
                    attachment.colorBlendOp = attachment.alphaBlendOp = VK_BLEND_OP_MAX;
            }
            auto depth = vsg::DepthStencilState::create();
            if (kind == PointsKind::stars || kind == PointsKind::skyTexture)
            {
                depth->depthCompareOp = VK_COMPARE_OP_GREATER_OR_EQUAL; // at depth 0, where nothing was drawn
                depth->depthWriteEnable = VK_FALSE;
            }
            if (kind == PointsKind::ribbons) depth->depthWriteEnable = VK_FALSE;
            vsg::GraphicsPipelineStates states{
                vsg::VertexInputState::create(), // no vertex buffers: points come from the storage buffer
                vsg::InputAssemblyState::create(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST),
                rasterization,
                vsg::MultisampleState::create(),
                colorBlend,
                depth,
            };
            bindPipeline = vsg::BindGraphicsPipeline::create(vsg::GraphicsPipeline::create(pipelineLayout, stages, states));
            if (kind == PointsKind::lines)
            {
                auto glowDepth = vsg::DepthStencilState::create();
                glowDepth->depthCompareOp = VK_COMPARE_OP_GREATER_OR_EQUAL;   // (reversed depth: and equal, the line's own)
                glowDepth->depthWriteEnable = VK_FALSE;
                vsg::GraphicsPipelineStates glowStates{
                    vsg::VertexInputState::create(), vsg::InputAssemblyState::create(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST),
                    rasterization, vsg::MultisampleState::create(), colorBlend, glowDepth,
                };
                bindGlowPipeline = vsg::BindGraphicsPipeline::create(vsg::GraphicsPipeline::create(pipelineLayout, stages, glowStates));
            }
            auto variant = [&](const std::vector<uint32_t>* v, const std::vector<uint32_t>* f) {
                if (!v || !f) return vsg::ref_ptr<vsg::BindGraphicsPipeline>();
                vsg::ShaderStages variantStages{
                    vsg::ShaderStage::create(VK_SHADER_STAGE_VERTEX_BIT, "main", vsg::ShaderModule::create(*v)),
                    vsg::ShaderStage::create(VK_SHADER_STAGE_FRAGMENT_BIT, "main", vsg::ShaderModule::create(*f)),
                };
                return vsg::BindGraphicsPipeline::create(vsg::GraphicsPipeline::create(pipelineLayout, variantStages, states));
            };
            bindPlainPipeline = variant(plainVert, plainFrag);
            bindDashedPipeline = variant(dashedVert, dashedFrag);
        }
    };

    // The line pipeline, created on first use and kept for the whole run (VSG
    // compiles it once per device; not destroyed at exit, to avoid depending
    // on the order of static destruction).
    PointsPipeline& line_pipeline()
    {
        static PointsPipeline* pipeline = new PointsPipeline(vkf_shaders::line_vert, vkf_shaders::line_frag, PointsKind::lines,
                                                             &vkf_shaders::line_plain_vert, &vkf_shaders::line_plain_frag,
                                                             &vkf_shaders::line_dashed_vert, &vkf_shaders::line_dashed_frag);
        return *pipeline;
    }

    PointsPipeline& marker_pipeline()
    {
        static PointsPipeline* pipeline = new PointsPipeline(vkf_shaders::marker_vert, vkf_shaders::marker_frag, PointsKind::markers);
        return *pipeline;
    }

    PointsPipeline& ribbon_pipeline()
    {
        static PointsPipeline* pipeline = new PointsPipeline(vkf_shaders::ribbon_vert, vkf_shaders::ribbon_frag, PointsKind::ribbons);
        return *pipeline;
    }

    PointsPipeline& stars_pipeline()
    {
        static PointsPipeline* pipeline = new PointsPipeline(vkf_shaders::stars_vert, vkf_shaders::stars_frag, PointsKind::stars);
        return *pipeline;
    }

    PointsPipeline& sky_pipeline()
    {
        static PointsPipeline* pipeline = new PointsPipeline(vkf_shaders::sky_vert, vkf_shaders::sky_frag, PointsKind::skyTexture);
        return *pipeline;
    }

    // Seconds since the first call, for animated markers (as osg_FrameTime;
    // wrapped every hour to keep float precision).
    float frame_time()
    {
        using clock = std::chrono::steady_clock;
        static const auto start = clock::now();
        return static_cast<float>(std::fmod(std::chrono::duration<double>(clock::now() - start).count(), 3600.0));
    }

    // The time of animated shaders (pulsing markers, highlighted lines), stored
    // on each vsg::View while its window records a video (vkf_viewer_record_start):
    // the video's time, so that they play at its speed however long a frame
    // takes; negative (or not set): frame_time().
    constexpr const char* animation_time_key = "vkf_animation_time";

    // The animation time for the view being recorded (see animation_time_key).
    float animation_time(vsg::RecordTraversal& visitor)
    {
        float t = -1.0f;
        if (visitor.viewDependentState && visitor.viewDependentState->view)
            visitor.viewDependentState->view->getValue(animation_time_key, t);
        return (t >= 0.0f) ? t : frame_time();
    }

    // Framebuffer pixels per window unit, stored on each vsg::View (2 on Retina).
    constexpr const char* pixel_ratio_key = "vkf_pixel_ratio";
    // A view's size as displayed when it differs from its render area: while
    // a resize is deferred (see resize_settle), the frame is drawn at the old
    // size and scaled to the new one, so sizes in pixels are worked out for
    // the new size (set_display_size; (0, 0): the render area's).
    constexpr const char* display_size_key = "vkf_display_size";

    // The viewport of the view being recorded: (width, height, pixel ratio, 0),
    // from the record traversal (a node can be shown in several windows).
    void recorded_viewport(vsg::RecordTraversal& visitor, float viewport[4])
    {
        viewport[0] = viewport[1] = viewport[2] = 1.0f;
        viewport[3] = 0.0f;
        if (!visitor.viewDependentState || !visitor.viewDependentState->view) return;
        const auto* view = visitor.viewDependentState->view;
        if (view->camera)
        {
            const auto area = view->camera->getRenderArea();
            viewport[0] = static_cast<float>(area.extent.width);
            viewport[1] = static_cast<float>(area.extent.height);
        }
        vsg::vec2 displayed;
        if (view->getValue(display_size_key, displayed) && displayed.x > 0.0f && displayed.y > 0.0f)
        {
            viewport[0] = displayed.x;
            viewport[1] = displayed.y;
        }
        float ratio = 1.0f;
        if (view->getValue(pixel_ratio_key, ratio)) viewport[2] = ratio;
    }

    // Labels: vsg::Text (GPU layout, so the text can change after it is shown)
    // with VSG's text shader set, except for its billboard GPU-layout variant,
    // which uses shaders/label.vert and label.frag (pixel-sized glyphs; the
    // viewport pushed at offset 128).
    struct LabelShaders
    {
        vsg::ref_ptr<vsg::ShaderSet> shaderSet;
        VkShaderStageFlags pushStages = VK_SHADER_STAGE_ALL;
        // per device: a pipeline layout compatible for push constants with the
        // text pipelines (same push constant range), to push the viewport
        std::map<const vsg::Device*, VkPipelineLayout> pushLayouts;

        VkPipelineLayout pushLayout(const vsg::Device* device)
        {
            auto& layout = pushLayouts[device];
            if (layout == VK_NULL_HANDLE)
            {
                VkPushConstantRange range{pushStages, 0, 144};
                VkPipelineLayoutCreateInfo info{};
                info.sType = VK_STRUCTURE_TYPE_PIPELINE_LAYOUT_CREATE_INFO;
                info.pushConstantRangeCount = 1;
                info.pPushConstantRanges = &range;
                if (vkCreatePipelineLayout(*device, &info, nullptr, &layout) != VK_SUCCESS) layout = VK_NULL_HANDLE;
            }
            return layout;
        }

        LabelShaders()
        {
            shaderSet = vsg::createTextShaderSet();
            // the variant labels use (billboard, GPU layout): looked up by
            // vsg::Text under these hints (its pipeline configurator's: the
            // shader set's default hints and these defines), whether or not
            // VSG has precompiled a variant of its own for them
            auto hints = shaderSet->defaultShaderHints ? vsg::ShaderCompileSettings::create(*shaderSet->defaultShaderHints)
                                                       : vsg::ShaderCompileSettings::create();
            hints->defines = {"BILLBOARD", "GPU_LAYOUT"};
            auto vert = vsg::ShaderModule::create(vkf_shaders::label_vert);
            auto frag = vsg::ShaderModule::create(vkf_shaders::label_frag);
            vert->hints = frag->hints = hints; // (SPIR-V: never recompiled)
            vsg::ShaderStages stages{vsg::ShaderStage::create(VK_SHADER_STAGE_VERTEX_BIT, "main", vert),
                                     vsg::ShaderStage::create(VK_SHADER_STAGE_FRAGMENT_BIT, "main", frag)};
            shaderSet->variants[hints] = stages;
            if (shaderSet->getShaderStages(hints) != stages)
                std::cerr << "vkframes: labels: VSG's text shaders are not as expected; labels use VSG's" << std::endl;
            if (shaderSet->pushConstantRanges.empty())
                std::cerr << "vkframes: labels: VSG's text shaders have no push constants; labels are not pixel-sized" << std::endl;
            for (auto& range : shaderSet->pushConstantRanges)
            {
                range.range.size = 144; // projection, model-view, viewport
                pushStages = range.range.stageFlags;
            }
        }
    };

    LabelShaders& label_shaders()
    {
        static LabelShaders* shaders = new LabelShaders; // kept for the whole run, as line_pipeline()
        return *shaders;
    }

    // Pushes the recorded view's viewport (offset 128) for its label children;
    // viewport[3] = 1 for HUD labels (drawn on top: label.vert puts them at the
    // near plane), 2 for labels at infinity (their position a direction: at the
    // far plane, as the sky's stars).
    class LabelGroup : public vsg::Inherit<vsg::Group, LabelGroup>
    {
    public:
        bool hud = false;
        bool atInfinity = false;

        void traverse(vsg::RecordTraversal& visitor) const override
        {
            float viewport[4];
            recorded_viewport(visitor, viewport);
            if (atInfinity) viewport[3] = 2.0f;
            else if (hud) viewport[3] = 1.0f;
            auto& commandBuffer = *visitor.getState()->_commandBuffer;
            auto& shaders = label_shaders();
            if (auto layout = shaders.pushLayout(commandBuffer.getDevice()))
                vkCmdPushConstants(commandBuffer, layout, shaders.pushStages, 128, sizeof(viewport), viewport);
            Group::traverse(visitor);
        }
    };

    struct Font
    {
        vsg::ref_ptr<vsg::Font> font;
    };

    // The HUD's projection: x and y from 0 to 1 across the viewport (OpenFrames'
    // ortho2D(0, 1, 0, 1)), unchanged when the window is resized.
    class HudProjection : public vsg::Inherit<vsg::ProjectionMatrix, HudProjection>
    {
    public:
        vsg::dmat4 transform() const override { return vsg::orthographic(0.0, 1.0, 0.0, 1.0, -1.0, 1.0); }
        void changeExtent(const VkExtent2D&, const VkExtent2D&) override {}
    };

    // Splits x into a float pair (high, low) with high + low == x to about 48 bits.
    void split(double x, float& high, float& low)
    {
        high = static_cast<float>(x);
        low = static_cast<float>(x - static_cast<double>(high));
    }

    // Records its children relative to the eye (RTE), for geometry whose
    // coordinates are too large for float precision (trajectories far from their
    // frame's origin). The vertex shader gets each point as a (high, low) float
    // pair. In the record traversal this node takes VSG's double model-view
    // matrix, inverts it to find the eye position in the local frame, pushes that
    // as a (high, low) pair (push constants at offset 128), and replaces the
    // model-view matrix with a copy whose translation is zeroed. The shader then
    // subtracts the eye from each point in double-single arithmetic. It also
    // pushes the size of the view being recorded (and its pixel ratio), for
    // pixel widths: a line can be shown in several windows.
    //
    // This is OpenFrames' method (DrawableTrajectory's UniformCallback and
    // TrajectoryArtist's vertex shader; Cozzi & Ring, "3D Engine Design for
    // Virtual Globes"), with push constants in place of uniforms.
    //
    // Level of detail (line strips): seen from far away, a dense trajectory is
    // mostly segments shorter than a pixel. Fortran gives each buffer of a strip
    // its bounding sphere and, for strides 2, 4, ... 2^levels, a bound on how far
    // the points skipped can be from the segments drawn (vkf_line_set_lod). Here,
    // for the view being recorded, the strip is drawn at the largest stride whose
    // bound is under lod_tolerance pixels at the sphere's nearest point: every
    // stride-th point, and the last (shaders/line.vert; the stride and the points
    // drawn are pushed, and the draw's vertex count set, per view).
    constexpr double lod_tolerance = 0.5;     // framebuffer pixels

    struct LineLod
    {
        vsg::ref_ptr<vsg::Draw> draw;         // the strip's draw: its vertex count is set per view
        uint32_t firstPoint = 0;              // points firstPoint + 1 .. drawnPoints are drawn (setRange)
        uint32_t drawnPoints = 0;
        bool strip = true;                    // (false for separate segments: no level of detail)
        vsg::dvec3 center;                    // bounding sphere of the points written
        double radius = 0.0;
        std::array<float, 16> error{};        // error[k]: the bound at stride 2^(k+1) (scene units)
        int levels = 0;
        bool hasBound = false;                // center and radius given (vkf_line_set_lod: strips)
        // One dash size per line and view (see dash_levels): for the dashes, the
        // trail's future's dashes and the highlight's arrows
        vsg::ref_ptr<vsg::vec4Array> style;   // the line's style (the patterns' factors, the arrows' spacing)
        struct DashLevel
        {
            int level = 0, previous = 0;      // the size shown (2^level scene units), and the one before
            double since = -1.0;              // when it changed (s; < 0: no change to morph)
            bool set = false;
        };
        std::map<const vsg::View*, std::array<DashLevel, 3>> dashLevels;
        // Screen-locked dashes (style count.z = 1: vkf_line_set_dash_mode): each
        // point's distance along the strip on screen, per view (see ScreenArcs)
        bool screenDashes = false;
        vsg::ref_ptr<vsg::floatArray> points;   // the strip's points (high/low pairs)
        struct ScreenArcs
        {
            static constexpr int slots = 4;                 // (a frame in flight's each)
            std::array<vsg::ref_ptr<vsg::Buffer>, slots> buffers;
            std::array<float*, slots> mapped{};
            std::array<VkDescriptorSet, slots> sets{};
            uint32_t capacity = 0;
        };
        std::map<const vsg::View*, ScreenArcs> screenArcs;
        // Fixed-length dashes (vkf_line_set_dash_mode 2: see fixed_dashes): the
        // pattern's length (scene units; 0: from the first view), and per view
        // the window units per scene unit it was given
        bool fixedDashes = false;
        double dashLength = 0.0;
        std::map<const vsg::View*, double> fixedBase;
    };

    // Seconds since the first call (for the dashes' changes of size).
    double steady_seconds()
    {
        static const auto start = std::chrono::steady_clock::now();
        return std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();
    }

    constexpr double dash_hysteresis = 0.6;   // a size kept until it is this many doublings off (> 0.5)
    constexpr double dash_morph = 0.3;        // seconds a change of size morphs over

    // One dash size per line and view (not per segment, so that a line's
    // dashes all have one length): for each of the line's patterns (its dashes,
    // its trail's future's, its highlight's arrows), the power of two of scene
    // units whose length on screen at the nearest point of the line's bounding
    // sphere is closest to the pattern's, kept until it is dash_hysteresis
    // doublings off; a change morphs from the old size to the new over
    // dash_morph seconds. Packed per pattern for line.vert: the old size + 128,
    // the new + 128 (8 bits each), the morph's progress (16 bits). False
    // (per-segment sizes) for a line without bounds, or with the eye among its
    // points.
    // Whether a line's style (11 vec4: see line.vert) draws a dash pattern (its
    // own, a trail's) or the highlight's arrows: else it needs no dash work
    // (dash_levels, fixed_dashes, screen_arcs).
    bool dashed(const vsg::vec4Array& st)
    {
        auto on = [](float pattern) {
            const uint32_t p = static_cast<uint32_t>(pattern) & 0xFFFFu;
            return p != 0xFFFFu && p != 0u;
        };
        if (on(st[1].z) || (static_cast<int>(st[4].x + 0.5f) & 2) != 0) return true;
        return st[6].z != 0.0f && (on(st[10].x) || on(st[10].z));
    }

    // The leanest variant of the line shaders a line's style can be drawn by:
    // 0 plain (no dash pattern, highlight or trail), 1 dashed (a dash pattern,
    // no highlight or trail), 2 full.
    int line_variant(const vsg::vec4Array& st)
    {
        if (static_cast<int>(st[4].x + 0.5f) != 0 || st[6].z != 0.0f) return 2;   // a highlight (pulse, arrows), a trail
        const uint32_t pattern = static_cast<uint32_t>(st[1].z) & 0xFFFFu;
        return (pattern != 0xFFFFu && pattern != 0u) ? 1 : 0;
    }

    bool dash_levels(LineLod& lod, const vsg::View* view, const vsg::dvec3& eye, const vsg::dmat4& projection,
                     const vsg::dmat4& modelView, const float viewport[4], uint32_t packed[3])
    {
        if (!lod.hasBound || !lod.style || lod.style->size() < 11 || !view || !dashed(*lod.style)) return false;
        const double fx = std::abs(projection[0][0]) * viewport[0], fy = std::abs(projection[1][1]) * viewport[1];
        double pixelsPerUnit;
        if (projection[3][3] == 1.0)
        {
            const double scale = vsg::length(vsg::dvec3(modelView[0][0], modelView[0][1], modelView[0][2]));
            pixelsPerUnit = 0.5 * std::max(fx, fy) * scale;
        }
        else
        {
            const double d = vsg::length(lod.center - eye) - lod.radius;
            if (!(d > 0.0)) return false;
            pixelsPerUnit = 0.5 * std::max(fx, fy) / d;
        }
        if (!(pixelsPerUnit > 0.0)) return false;
        const auto& st = *lod.style;
        const bool trail = st[6].z != 0.0f;
        const double ratio = viewport[2];
        const double periods[3] = {16.0 * std::max(trail ? st[10].y : st[1].w, 1.0e-3f) * ratio,   // pixels
                                   16.0 * std::max(st[10].w, 1.0e-3f) * ratio,
                                   std::max(st[4].y, 1.0f) * ratio};
        auto& levels = lod.dashLevels[view];
        const double now = steady_seconds();
        for (int k = 0; k < 3; ++k)
        {
            const double lambda = std::clamp(std::log2(periods[k] / pixelsPerUnit), -125.0, 125.0);
            auto& dl = levels[k];
            if (!dl.set)
            {
                dl.level = dl.previous = static_cast<int>(std::lround(lambda));
                dl.since = -1.0;
                dl.set = true;
            }
            else if (std::abs(lambda - dl.level) > dash_hysteresis)
            {
                dl.previous = dl.level;
                dl.level = static_cast<int>(std::lround(lambda));
                dl.since = now;
            }
            const double w = (dl.since < 0.0) ? 1.0 : std::clamp((now - dl.since) / dash_morph, 0.0, 1.0);
            packed[k] = static_cast<uint32_t>(dl.previous + 128) | (static_cast<uint32_t>(dl.level + 128) << 8) |
                        (static_cast<uint32_t>(std::lround(w * 65535.0)) << 16);
        }
        return true;
    }

    // The stride to draw a strip with in the view being recorded: the largest
    // 2^k whose error bound projects under lod_tolerance pixels.
    uint32_t lod_stride(const LineLod& lod, const vsg::dvec3& eye, const vsg::dmat4& projection,
                        const vsg::dmat4& modelView, const float viewport[4])
    {
        const double fx = std::abs(projection[0][0]) * viewport[0], fy = std::abs(projection[1][1]) * viewport[1];
        double pixelsPerUnit;                 // at the nearest point of the bounding sphere
        if (projection[3][3] == 1.0)          // orthographic: the same everywhere
        {
            const double scale = vsg::length(vsg::dvec3(modelView[0][0], modelView[0][1], modelView[0][2]));
            pixelsPerUnit = 0.5 * std::max(fx, fy) * scale;
        }
        else
        {
            const double d = vsg::length(lod.center - eye) - lod.radius;
            if (!(d > 0.0)) return 1;         // the eye is among the points: all of them
            pixelsPerUnit = 0.5 * std::max(fx, fy) / d;
        }
        uint32_t stride = 1;
        for (int k = 0; k < lod.levels; ++k)
        {
            if (lod.error[k] * pixelsPerUnit > lod_tolerance) break;
            stride *= 2;
        }
        return stride;
    }

    // Screen-locked dashes (OpenGL's line stipple: a constant length in pixels,
    // following the line on screen): line.vert needs each point's distance
    // along the strip on screen from the first point drawn, which changes with
    // every view and frame. EyeRelative computes it while recording a view
    // (projecting the points drawn, at the level of detail's stride, each
    // segment clipped in front of the eye as line.vert does) into a buffer of
    // that line's and view's (one per frame in flight, mapped), bound as
    // descriptor set 1; lines without them bind a dummy set (set 1 is in the
    // line pipeline's layout). Their own descriptor pool, per device; raw
    // Vulkan, as they are made while recording.
    struct ArcResources
    {
        VkDescriptorPool pool = VK_NULL_HANDLE;
        vsg::ref_ptr<vsg::Buffer> dummyBuffer;
        VkDescriptorSet dummy = VK_NULL_HANDLE;
    };

    VkDescriptorSet allocate_arc_set(ArcResources& r, vsg::Device* device, const vsg::Buffer& buffer, VkDeviceSize size)
    {
        const uint32_t id = device->deviceID;
        VkDescriptorSetLayout layout = line_pipeline().arcLayout->vk(id);
        VkDescriptorSetAllocateInfo info{VK_STRUCTURE_TYPE_DESCRIPTOR_SET_ALLOCATE_INFO};
        info.descriptorPool = r.pool;
        info.descriptorSetCount = 1;
        info.pSetLayouts = &layout;
        VkDescriptorSet set = VK_NULL_HANDLE;
        if (vkAllocateDescriptorSets(device->vk(), &info, &set) != VK_SUCCESS) return VK_NULL_HANDLE;
        VkDescriptorBufferInfo bufferInfo{buffer.vk(id), 0, size};
        VkWriteDescriptorSet write{VK_STRUCTURE_TYPE_WRITE_DESCRIPTOR_SET};
        write.dstSet = set;
        write.dstBinding = 0;
        write.descriptorCount = 1;
        write.descriptorType = VK_DESCRIPTOR_TYPE_STORAGE_BUFFER;
        write.pBufferInfo = &bufferInfo;
        vkUpdateDescriptorSets(device->vk(), 1, &write, 0, nullptr);
        return set;
    }

    // A host-visible, coherent storage buffer of n floats, mapped (null if it can't be).
    vsg::ref_ptr<vsg::Buffer> mapped_floats(vsg::Device* device, uint32_t n, float** mapped)
    {
        const VkDeviceSize size = VkDeviceSize(std::max(n, 1u)) * sizeof(float);
        auto buffer = vsg::createBufferAndMemory(vsg::ref_ptr<vsg::Device>(device), size, VK_BUFFER_USAGE_STORAGE_BUFFER_BIT,
                                                 VK_SHARING_MODE_EXCLUSIVE,
                                                 VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
        void* data = nullptr;
        auto memory = buffer->getDeviceMemory(device->deviceID);
        if (!memory || memory->map(buffer->getMemoryOffset(device->deviceID), size, 0, &data) != VK_SUCCESS) return {};
        *mapped = static_cast<float*>(data);
        return buffer;
    }

    ArcResources& arc_resources(vsg::Device* device)
    {
        static std::map<vsg::Device*, ArcResources> resources;   // (kept for the whole run, as the pipelines)
        auto& r = resources[device];
        if (r.pool == VK_NULL_HANDLE)
        {
            VkDescriptorPoolSize size{VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 4096};
            VkDescriptorPoolCreateInfo info{VK_STRUCTURE_TYPE_DESCRIPTOR_POOL_CREATE_INFO};
            info.maxSets = 4096;
            info.poolSizeCount = 1;
            info.pPoolSizes = &size;
            vkCreateDescriptorPool(device->vk(), &info, nullptr, &r.pool);
            float* mapped = nullptr;
            r.dummyBuffer = mapped_floats(device, 1, &mapped);
            if (mapped) mapped[0] = 0.0f;
            if (r.dummyBuffer) r.dummy = allocate_arc_set(r, device, *r.dummyBuffer, sizeof(float));
        }
        return r;
    }

    // The screen lengths of strip `lod` for the view being recorded (see
    // ScreenArcs): arc[i] at each segment's first point drawn (from `first`,
    // at `stride`, to point `drawn` - 1), in framebuffer pixels. The set to
    // bind (null if it can't be made).
    VkDescriptorSet screen_arcs(LineLod& lod, vsg::RecordTraversal& visitor, vsg::Device* device, const vsg::dvec3& eye,
                                const vsg::dmat4& projection, const vsg::dmat4& modelView, const float viewport[4],
                                uint32_t first, uint32_t drawn, uint32_t stride)
    {
        if (!lod.points || !visitor.viewDependentState || !visitor.getFrameStamp()) return VK_NULL_HANDLE;
        const uint32_t capacity = static_cast<uint32_t>(lod.points->size() / 8);
        auto& r = arc_resources(device);
        auto& arcs = lod.screenArcs[visitor.viewDependentState->view];
        const int slot = static_cast<int>(visitor.getFrameStamp()->frameCount % LineLod::ScreenArcs::slots);
        if (arcs.capacity != capacity)                       // (made for this strip's buffer)
        {
            for (int k = 0; k < LineLod::ScreenArcs::slots; ++k)
            {
                arcs.buffers[k] = mapped_floats(device, capacity, &arcs.mapped[k]);
                arcs.sets[k] = arcs.buffers[k] ? allocate_arc_set(r, device, *arcs.buffers[k], VkDeviceSize(capacity) * sizeof(float))
                                               : VK_NULL_HANDLE;
            }
            arcs.capacity = capacity;
        }
        float* arc = arcs.mapped[slot];
        if (!arc || arcs.sets[slot] == VK_NULL_HANDLE) return VK_NULL_HANDLE;
        vsg::dmat4 rotation = modelView;
        rotation[3][0] = rotation[3][1] = rotation[3][2] = 0.0;
        const vsg::dmat4 mvp = projection * rotation;
        const float* p = lod.points->data();
        auto clip = [&](uint32_t i) {
            const vsg::dvec3 v(double(p[8 * i]) + double(p[8 * i + 4]) - eye.x, double(p[8 * i + 1]) + double(p[8 * i + 5]) - eye.y,
                               double(p[8 * i + 2]) + double(p[8 * i + 6]) - eye.z);
            return mvp * vsg::dvec4(v, 1.0);
        };
        constexpr double w_min = 1.0e-6;                     // (line.vert's W_MIN)
        const double hx = 0.5 * viewport[0], hy = 0.5 * viewport[1];
        const uint32_t last = drawn - 1, q = first / stride;
        // the pattern's origin: the first point drawn, or with a trail the point
        // at the current time (its distance along the strip: the trail's), so
        // the dashes don't jump as the trail's first point changes
        const auto& st = *lod.style;
        const bool trail = lod.style->size() >= 11 && st[6].z != 0.0f;
        const double s_now = trail ? double(st[6].x) + double(st[6].y) : 0.0;
        auto along = [&](uint32_t i) { return double(p[8 * i + 3]) + double(p[8 * i + 7]); };   // distance along the strip
        double total = 0.0, origin = 0.0;
        bool found = false;
        for (uint32_t s = 0;; ++s)                           // the segments line.vert draws
        {
            const uint32_t a = (s == 0) ? first : (q + s) * stride;
            if (a >= last) break;
            const uint32_t b = std::min((q + s + 1) * stride, last);
            arc[a] = static_cast<float>(total);
            vsg::dvec4 ca = clip(a), cb = clip(b);
            double length = 0.0;
            if (!(ca.w < w_min && cb.w < w_min))             // (else behind the eye: not drawn)
            {
                if (ca.w < w_min) ca = ca + (cb - ca) * ((w_min - ca.w) / (cb.w - ca.w));
                if (cb.w < w_min) cb = cb + (ca - cb) * ((w_min - cb.w) / (ca.w - cb.w));
                length = std::hypot((cb.x / cb.w - ca.x / ca.w) * hx, (cb.y / cb.w - ca.y / ca.w) * hy);
            }
            if (trail && !found)                             // the current time in this segment (or before the first)
            {
                const double sa = along(a), sb = along(b);
                if (s_now <= sb || b >= last)
                {
                    const double f = (sb > sa) ? std::clamp((s_now - sa) / (sb - sa), 0.0, 1.0) : 0.0;
                    origin = total + f * length;
                    found = true;
                }
            }
            total += length;
        }
        if (origin != 0.0)                                   // (the pattern from the current time's point)
            for (uint32_t s = 0;; ++s)
            {
                const uint32_t a = (s == 0) ? first : (q + s) * stride;
                if (a >= last) break;
                arc[a] = static_cast<float>(double(arc[a]) - origin);
            }
        return arcs.sets[slot];
    }

    // Fixed-length dashes (vkf_line_set_dash_mode 2): the pattern a fixed
    // length of the line, as paint on it: d(s) = s * perUnit for the distance
    // s along the strip, perUnit the pattern's periods per scene unit, never
    // changing with the view (zooming in, the dashes grow on screen). Its
    // length: lod.dashLength (scene units), or if 0 the pattern's length on
    // screen at the point nearest the view's center, the first time the line
    // is drawn in this view. For precision (long trajectories, close up) d is
    // taken about a pivot near the view's center: d = phase + (s - pivot) *
    // perUnit, phase = fract(pivot * perUnit) in double, the same pattern for
    // any pivot. Packed for line.vert: (pivot high, pivot low, phase 0,
    // perUnit 0), (phase 1, perUnit 1, phase 2, perUnit 2) for the dashes, the
    // trail's future's and the arrows; false if not used here.
    bool fixed_dashes(LineLod& lod, const vsg::View* view, const vsg::dvec3& eye, const vsg::dmat4& projection,
                      const vsg::dmat4& modelView, const float viewport[4], uint32_t first, uint32_t drawn, float out[8])
    {
        if (!lod.fixedDashes || !lod.points || !lod.style || lod.style->size() < 11 || !view || drawn < first + 2 ||
            !dashed(*lod.style))
            return false;
        const float* p = lod.points->data();
        vsg::dmat4 rotation = modelView;
        rotation[3][0] = rotation[3][1] = rotation[3][2] = 0.0;
        const vsg::dmat4 mvp = projection * rotation;
        // the pivot: a drawn point near the view's center, coarse to fine (64
        // samples, then 64 about the nearest, ...: a few hundred projections
        // for a million points; any point will do, only nearer is more precise)
        double pivot = 0.0, depth = -1.0, best = std::numeric_limits<double>::max();
        const uint32_t last = drawn - 1;
        uint32_t lo = first, hi = last, nearest = first;
        for (;;)
        {
            const uint32_t step = std::max((hi - lo) / 64u, 1u);
            for (uint32_t i = lo;; i = std::min(i + step, hi))
            {
                const vsg::dvec3 v(double(p[8 * i]) + double(p[8 * i + 4]) - eye.x,
                                   double(p[8 * i + 1]) + double(p[8 * i + 5]) - eye.y,
                                   double(p[8 * i + 2]) + double(p[8 * i + 6]) - eye.z);
                const vsg::dvec4 c = mvp * vsg::dvec4(v, 1.0);
                const double r = (c.w > 1.0e-6) ? (c.x / c.w) * (c.x / c.w) + (c.y / c.w) * (c.y / c.w)
                                                : std::numeric_limits<double>::max() / 2;   // (behind: if nothing's in front)
                if (r < best)
                {
                    best = r;
                    nearest = i;
                    pivot = double(p[8 * i + 3]) + double(p[8 * i + 7]);
                    depth = c.w;
                }
                if (i >= hi) break;
            }
            if (step == 1) break;
            lo = (nearest > first + step) ? nearest - step : first;
            hi = std::min(nearest + step, last);
        }
        const auto& st = *lod.style;
        // each pattern's period (window units: 16 bits of `factor`; the arrows' spacing)
        const double periods[3] = {16.0 * std::max(st[6].z != 0.0f ? st[10].y : st[1].w, 1.0e-3f),
                                   16.0 * std::max(st[10].w, 1.0e-3f), std::max(st[4].y, 1.0f)};
        double& base = lod.fixedBase[view];                     // window units per scene unit, set once
        if (lod.dashLength > 0.0)
            base = periods[0] / lod.dashLength;
        else if (base <= 0.0)
        {
            const double ratio = std::max(double(viewport[2]), 1.0e-6);
            const double fx = std::abs(projection[0][0]) * viewport[0], fy = std::abs(projection[1][1]) * viewport[1];
            if (projection[3][3] == 1.0)
                base = 0.5 * std::max(fx, fy) * vsg::length(vsg::dvec3(modelView[0][0], modelView[0][1], modelView[0][2])) / ratio;
            else if (depth > 1.0e-6)
                base = 0.5 * std::max(fx, fy) / depth / ratio;
            if (!(base > 0.0) || !std::isfinite(base)) { base = 0.0; return false; }
        }
        float high, low;
        split(pivot, high, low);
        out[0] = high;
        out[1] = low;
        for (int k = 0; k < 3; ++k)
        {
            const double perUnit = base / periods[k];
            const double phase = pivot * perUnit;
            out[2 + 2 * k] = static_cast<float>(phase - std::floor(phase));
            out[3 + 2 * k] = static_cast<float>(perUnit);
        }
        return true;
    }

    class EyeRelative : public vsg::Inherit<vsg::Group, EyeRelative>
    {
    public:
        explicit EyeRelative(vsg::ref_ptr<vsg::PipelineLayout> layout) :
            pipelineLayout(layout) {}

        vsg::ref_ptr<vsg::PipelineLayout> pipelineLayout;
        std::shared_ptr<LineLod> lod;          // line strips (null for other geometry)
        bool linePipeline = false;             // drawn by the line pipeline: its set 1 bound (screen lengths)
        int32_t pass = 0;                      // lines: 0 the line, 1 a highlighted line's glow and arrows (pc.lod.w)
        bool variants = false;                 // lines: three children, the plain, dashed and full pipelines': one
                                               // drawn (as the style allows: line_variant); other traversals see all

        void traverse(vsg::RecordTraversal& visitor) const override
        {
            auto state = visitor.getState();
            const vsg::dmat4 modelView = state->modelviewMatrixStack.top();
            const vsg::dvec3 eye = vsg::inverse(modelView) * vsg::dvec3(0.0, 0.0, 0.0);

            // eye high, eye low, viewport (width, height, pixel ratio, time), level of
            // detail (stride, points drawn, the first point drawn (0-based): as integers),
            // the dash sizes (packed: see dash_levels; the last 1 if given, 2 for
            // fixed-length dashes: then fixed_dashes's 8 values after)
            float constants[28] = {};
            for (int i = 0; i < 3; ++i) split(eye[i], constants[i], constants[4 + i]);
            recorded_viewport(visitor, constants + 8);
            constants[11] = animation_time(visitor);
            uint32_t stride = 1, drawn = 0, first = 0;
            if (lod)
            {
                drawn = lod->drawnPoints;
                first = lod->firstPoint;
                if (lod->strip && lod->levels > 0 && drawn > first + 2)
                    stride = lod_stride(*lod, eye, state->projectionMatrixStack.top(), modelView, constants + 8);
                if (lod->strip && lod->draw)       // segments from `first`, then on multiples of the stride (line.vert)
                {
                    const uint32_t start = (first / stride) * stride;
                    lod->draw->vertexCount = (drawn >= first + 2) ? 6 * ((drawn - 1 - start + stride - 1) / stride) : 0;
                }
            }
            std::memcpy(constants + 12, &stride, sizeof(stride));
            std::memcpy(constants + 13, &drawn, sizeof(drawn));
            std::memcpy(constants + 14, &first, sizeof(first));
            std::memcpy(constants + 15, &pass, sizeof(pass));
            uint32_t packed[4] = {0, 0, 0, 0};
            if (lod && visitor.viewDependentState && lod->fixedDashes && lod->strip &&
                fixed_dashes(*lod, visitor.viewDependentState->view, eye, state->projectionMatrixStack.top(), modelView,
                             constants + 8, first, drawn, constants + 20))
                packed[3] = 2;
            else if (lod && visitor.viewDependentState &&
                     dash_levels(*lod, visitor.viewDependentState->view, eye, state->projectionMatrixStack.top(), modelView,
                                 constants + 8, packed))
                packed[3] = 1;
            std::memcpy(constants + 16, packed, sizeof(packed));
            auto& commandBuffer = *state->_commandBuffer;
            vkCmdPushConstants(commandBuffer, pipelineLayout->vk(commandBuffer.deviceID), VK_SHADER_STAGE_VERTEX_BIT,
                               128, sizeof(constants), constants);
            if (linePipeline)                  // set 1: the screen lengths (screen-locked dashes), or the dummy set
            {
                auto* device = commandBuffer.getDevice();
                VkDescriptorSet set = VK_NULL_HANDLE;
                if (lod && lod->screenDashes && lod->strip && drawn >= first + 2 && lod->style && lod->style->size() >= 11 &&
                    dashed(*lod->style))
                    set = screen_arcs(*lod, visitor, const_cast<vsg::Device*>(device), eye, state->projectionMatrixStack.top(),
                                      modelView, constants + 8, first, drawn, stride);
                if (set == VK_NULL_HANDLE) set = arc_resources(const_cast<vsg::Device*>(device)).dummy;
                if (set != VK_NULL_HANDLE)
                    vkCmdBindDescriptorSets(commandBuffer, VK_PIPELINE_BIND_POINT_GRAPHICS, pipelineLayout->vk(commandBuffer.deviceID),
                                            1, 1, &set, 0, nullptr);
            }

            vsg::dmat4 rotationOnly = modelView;
            rotationOnly[3][0] = rotationOnly[3][1] = rotationOnly[3][2] = 0.0;
            state->modelviewMatrixStack.push(rotationOnly);
            state->dirty = true;
            if (variants && children.size() == 3 && lod && lod->style && lod->style->size() >= 11)
                children[line_variant(*lod->style)]->accept(visitor);
            else
                Group::traverse(visitor);
            state->modelviewMatrixStack.pop();
            state->dirty = true;
        }
    };

    // A sphere's level of detail: meshes of the same body at several
    // resolutions (children, each with its segments around the equator), of
    // which the record traversal draws, for the view being recorded, the
    // coarsest whose facets stay within sphere_lod_tolerance pixels of the true
    // outline: a sphere of radius r pixels on screen needs pi * sqrt(r / (2 *
    // tolerance)) segments around (a facet of angle t departs from the circle
    // by r t^2 / 8). Without such a mesh yet, it draws the finest it has, and
    // records the segments wanted (`requested`, the most of any view), for
    // Fortran to build that mesh (vkf_sphere_lod_requested). Meshes are built
    // in Fortran (src/vkf_sphere.f90); this only chooses.
    constexpr double sphere_lod_tolerance = 0.5;   // framebuffer pixels

    class SphereLod : public vsg::Inherit<vsg::Group, SphereLod>
    {
    public:
        double radius = 1.0;                       // the bounding sphere (local coordinates)
        std::vector<int> segments;                 // of each child, increasing
        mutable int requested = 0;                 // segments wanted since the last request read

        void traverse(vsg::RecordTraversal& visitor) const override
        {
            if (children.empty()) return;
            auto state = visitor.getState();
            const vsg::dmat4 modelView = state->modelviewMatrixStack.top();
            const vsg::dmat4 projection = state->projectionMatrixStack.top();
            float viewport[4];
            recorded_viewport(visitor, viewport);
            const double f = 0.5 * std::max(std::abs(projection[0][0]) * viewport[0], std::abs(projection[1][1]) * viewport[1]);
            double pixels;                         // the sphere's radius on screen
            if (projection[3][3] == 1.0)           // orthographic
            {
                pixels = radius * f * vsg::length(vsg::dvec3(modelView[0][0], modelView[0][1], modelView[0][2]));
            }
            else
            {
                const vsg::dvec3 eye = vsg::inverse(modelView) * vsg::dvec3(0.0, 0.0, 0.0);
                const double d2 = vsg::length2(eye) - radius * radius;
                pixels = (d2 > 1.0e-12 * radius * radius) ? f * radius / std::sqrt(d2) : 1.0e12;   // (inside: the finest)
            }
            const int wanted = static_cast<int>(std::ceil(vsg::PI * std::sqrt(pixels / (2.0 * sphere_lod_tolerance))));
            requested = std::max(requested, wanted);
            std::size_t chosen = children.size() - 1;   // the finest, if none is fine enough
            for (std::size_t i = 0; i < children.size(); ++i)
                if (segments[i] >= wanted) { chosen = i; break; }
            children[chosen]->accept(visitor);
        }
    };

    // The camera's view: OpenFrames' FollowingTrackball. The trackball keeps
    // eye, center and up in a view frame's coordinates (it only moves those),
    // and the view matrix is lookAt * worldToView, with worldToView (world to
    // view-frame coordinates, in double) set from Fortran each frame.
    class ViewLookAt : public vsg::Inherit<vsg::LookAt, ViewLookAt>
    {
    public:
        using Inherit::Inherit;

        vsg::dmat4 worldToView; // identity

        vsg::dmat4 transform(const vsg::dvec3& offset = {}) const override
        {
            return vsg::LookAt::transform(offset) * worldToView;
        }
    };

    // Video recording (vkf_viewer_record_start): each frame drawn is copied, in
    // the frame's own commands after its render pass (GrabFrame, the window's
    // command graph's last command), from the swapchain image into one of two
    // persistently mapped host buffers; once the next frame has been
    // submitted, the frame's fence is waited for and its pixels are written
    // from the mapped buffer to an ffmpeg process's standard input (popen) as
    // raw video. So a frame is written while the next one renders, without
    // waiting for the whole device and without copies or image files. The
    // video's size is its first frame's: frames of another size (the window
    // resized) are cropped or padded with black.
    struct Recorder
    {
        struct Slot
        {
            vsg::ref_ptr<vsg::Buffer> buffer;      // (width x height x 4 bytes, host visible and coherent)
            void* mapped = nullptr;
            vsg::ref_ptr<vsg::Fence> fence;        // the frame's, once submitted
            bool pending = false;                  // copied to, not written yet
        };
        bool active = false;
        bool failed = false;                       // (an error: frames are no longer copied or written)
        std::string ffmpeg, output;                // the ffmpeg command, its output options (and file)
        double fps = 30.0;
        FILE* pipe = nullptr;                      // ffmpeg's standard input (opened at the first frame written)
        uint32_t width = 0, height = 0;            // the video's size (its first frame's)
        bool bgra = true;                          // the swapchain's byte order (else rgba)
        Slot slots[2];
        int current = -1;                          // the slot copied to in this frame (-1: none)
        int previous = -1;                         // the slot copied to in the frame before
        uint64_t copied = 0, written = 0;          // frames
        float animationStart = 0.0f;               // frame_time() at the start
    };

    // Copies the frame drawn to the recorder's next slot (see Recorder): the
    // swapchain image from presentation to transfer layout and back.
    class GrabFrame : public vsg::Inherit<vsg::Command, GrabFrame>
    {
    public:
        GrabFrame(vsg::ref_ptr<vsg::Window> w, Recorder* r) :
            window(w), recorder(r) {}

        vsg::observer_ptr<vsg::Window> window;
        Recorder* recorder;

        void record(vsg::CommandBuffer& commandBuffer) const override
        {
            auto& rec = *recorder;
            rec.current = -1;
            vsg::ref_ptr<vsg::Window> w = window;
            if (!rec.active || rec.failed || !w) return;
            auto swapchain = w->getSwapchain();
            const size_t index = w->imageIndex();
            if (!swapchain || index >= swapchain->getImageViews().size()) return;
            const VkFormat format = swapchain->getImageFormat();
            const bool bgra = (format == VK_FORMAT_B8G8R8A8_UNORM || format == VK_FORMAT_B8G8R8A8_SRGB);
            const bool rgba = (format == VK_FORMAT_R8G8B8A8_UNORM || format == VK_FORMAT_R8G8B8A8_SRGB);
            if (!bgra && !rgba)
            {
                std::cerr << "vkframes: recording: unsupported swapchain format " << format << std::endl;
                rec.failed = true;
                return;
            }
            const VkExtent2D extent = swapchain->getExtent();
            if (rec.width == 0)
            {
                rec.width = extent.width;
                rec.height = extent.height;
                rec.bgra = bgra;
            }
            const int s = (rec.previous == 0) ? 1 : 0;
            auto& slot = rec.slots[s];
            auto device = vsg::ref_ptr<vsg::Device>(commandBuffer.getDevice());
            const uint32_t deviceID = commandBuffer.deviceID;
            const VkDeviceSize size = VkDeviceSize(rec.width) * rec.height * 4;
            if (!slot.buffer)
            {
                slot.buffer = vsg::createBufferAndMemory(device, size, VK_BUFFER_USAGE_TRANSFER_DST_BIT, VK_SHARING_MODE_EXCLUSIVE,
                                                         VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);
                auto memory = slot.buffer->getDeviceMemory(deviceID);
                if (!memory || memory->map(slot.buffer->getMemoryOffset(deviceID), size, 0, &slot.mapped) != VK_SUCCESS)
                {
                    std::cerr << "vkframes: recording: could not map a readback buffer" << std::endl;
                    rec.failed = true;
                    return;
                }
                std::memset(slot.mapped, 0, static_cast<size_t>(size));
            }
            const uint32_t width = std::min(extent.width, rec.width), height = std::min(extent.height, rec.height);
            if (width != rec.width || height != rec.height)        // (the rest black)
                std::memset(slot.mapped, 0, static_cast<size_t>(size));

            VkImage image = swapchain->getImageViews()[index]->image->vk(deviceID);
            VkImageMemoryBarrier barrier{VK_STRUCTURE_TYPE_IMAGE_MEMORY_BARRIER};
            barrier.srcAccessMask = VK_ACCESS_COLOR_ATTACHMENT_WRITE_BIT;
            barrier.dstAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
            barrier.oldLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;     // (the render pass's final layout)
            barrier.newLayout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
            barrier.srcQueueFamilyIndex = barrier.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
            barrier.image = image;
            barrier.subresourceRange = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1};
            vkCmdPipelineBarrier(commandBuffer, VK_PIPELINE_STAGE_COLOR_ATTACHMENT_OUTPUT_BIT, VK_PIPELINE_STAGE_TRANSFER_BIT, 0,
                                 0, nullptr, 0, nullptr, 1, &barrier);
            VkBufferImageCopy region{};
            region.bufferRowLength = rec.width;
            region.bufferImageHeight = rec.height;
            region.imageSubresource = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1};
            region.imageExtent = {width, height, 1};
            vkCmdCopyImageToBuffer(commandBuffer, image, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, slot.buffer->vk(deviceID), 1, &region);
            barrier.srcAccessMask = VK_ACCESS_TRANSFER_READ_BIT;
            barrier.dstAccessMask = 0;
            barrier.oldLayout = VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL;
            barrier.newLayout = VK_IMAGE_LAYOUT_PRESENT_SRC_KHR;
            VkBufferMemoryBarrier toHost{VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER};
            toHost.srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT;
            toHost.dstAccessMask = VK_ACCESS_HOST_READ_BIT;
            toHost.srcQueueFamilyIndex = toHost.dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED;
            toHost.buffer = slot.buffer->vk(deviceID);
            toHost.size = VK_WHOLE_SIZE;
            vkCmdPipelineBarrier(commandBuffer, VK_PIPELINE_STAGE_TRANSFER_BIT,
                                 VK_PIPELINE_STAGE_BOTTOM_OF_PIPE_BIT | VK_PIPELINE_STAGE_HOST_BIT, 0,
                                 0, nullptr, 1, &toHost, 1, &barrier);
            slot.pending = true;
            slot.fence = nullptr;
            rec.current = s;
            ++rec.copied;
        }
    };

    struct Viewer
    {
        vsg::ref_ptr<vsg::Viewer> viewer;
        vsg::ref_ptr<vsg::Window> window;
        vsg::ref_ptr<vsg::Group> scene;
        vsg::ref_ptr<vsg::Camera> camera;
        vsg::ref_ptr<vsg::CommandGraph> commandGraph;
        vsg::ref_ptr<vsg::Group> hudScene;     // HUD text, drawn by a second view on top
        vsg::ref_ptr<vsg::Switch> headlight;   // the window's default light, in eye coordinates
        void* headlightHandle = nullptr;       // its LightNode (vkf_viewer_light)
        void* childView = nullptr;             // macOS: the view VSG renders into, in the host's view
        void* metalLayer = nullptr;            // macOS: the CAMetalLayer of its surface
        float pixelRatio = 1.0f;               // framebuffer pixels per window unit (2 on Retina)
        bool compiled = false;
        bool resizePending = false;            // resized before the first frame (no swapchain yet)
        bool resizeDeferred = false;           // macOS, embedded: the swapchain to rebuild once settled
        bool resizeOwn = false;                // macOS, VSG's own window: resized (see vkf_viewer_frame)
        bool empty = false;                    // a host's window of no width or height: not drawn
        bool skipped = false;                  // this frame not drawn (vkf_viewer_frame_draw does nothing)
        vsg::clock::time_point lastResize;     // (see resize_settle)
        std::vector<std::array<int32_t, 12>> events;   // keyboard and mouse events for Fortran (EventRecorder)
        vkf::Gui* gui = nullptr;               // GUI panels (vkf_gui.cpp), once used
        Recorder recorder;                     // video recording (vkf_viewer_record_start)
        vsg::ref_ptr<vsg::Trackball> trackball;
        float zoomSpeed = 1.0f;                // scroll steps scaled by it (vkf_viewer_set_camera_controls)
        bool grabArmed = false;                // a left press takes the mouse from the trackball (vkf_viewer_set_grab)
        bool grabbing = false;                 // taken: until the button is released
    };

    // Records the window's keyboard and mouse events for Fortran (its event
    // handler and built-in keys: vkf_viewer_events), before the trackball sees
    // them. Each: kind (1 key press, 2 key release, 3 pointer move, 4 button
    // press, 5 button release), key (modified: 'V' with shift), key base,
    // modifiers, button, buttons held (mask), x, y (framebuffer pixels from the
    // top left), and the render area (offset x, y, width, height). It also
    // scales scroll steps by the viewer's zoom speed, for the trackball, and
    // gives a button release soon after a drag's last move (under throw_gap:
    // released while moving) that move's time: VSG's trackball throws (keeps
    // the camera moving) only for a release in the same millisecond as the
    // last pointer event, which Windows' messages are, but not macOS's (nor
    // a host's events, forwarded one by one), so a flick never threw there.
    // A grab (vkf_viewer_set_grab: Fortran found a handle under the cursor): a
    // left button press then takes the mouse from the trackball until the
    // button is released, the press, the moves and the release recorded with
    // key 1 (a handle dragged, not the camera) and marked handled (the
    // trackball ignores them, and without the press it has no drag to follow).
    class EventRecorder : public vsg::Inherit<vsg::Visitor, EventRecorder>
    {
    public:
        explicit EventRecorder(Viewer* viewer) : v(viewer) {}

        void apply(vsg::KeyPressEvent& e) override { key(1, e); }
        void apply(vsg::KeyReleaseEvent& e) override { key(2, e); }
        void apply(vsg::MoveEvent& e) override
        {
            if (v->grabbing && !e.handled)
            {
                pointer(3, e, 0, 1);
                e.handled = true;
                return;
            }
            if ((e.mask & (vsg::BUTTON_MASK_1 | vsg::BUTTON_MASK_2 | vsg::BUTTON_MASK_3)) != 0)
            {
                lastDragMove = e.time;
                dragging = true;
            }
            pointer(3, e, 0);
        }
        void apply(vsg::ButtonPressEvent& e) override
        {
            dragging = false;
            if (v->grabArmed && !e.handled && (e.mask & vsg::BUTTON_MASK_1) != 0)
            {
                v->grabbing = true;
                pointer(4, e, e.button, 1);
                e.handled = true;
                return;
            }
            pointer(4, e, e.button);
        }
        void apply(vsg::ButtonReleaseEvent& e) override
        {
            if (v->grabbing && !e.handled)
            {
                v->grabbing = false;
                pointer(5, e, e.button, 1);
                e.handled = true;
                return;
            }
            if (dragging && e.time >= lastDragMove && e.time - lastDragMove < throw_gap) e.time = lastDragMove;
            dragging = false;
            pointer(5, e, e.button);
        }
        void apply(vsg::ScrollWheelEvent& e) override { e.delta *= v->zoomSpeed; }

    private:
        static constexpr size_t max_events = 4096;     // (kept while nothing reads them)
        static constexpr std::chrono::milliseconds throw_gap{40};  // (moves come every 8 to 16 ms while moving)
        Viewer* v;
        vsg::time_point lastDragMove;                  // the last move with a button held
        bool dragging = false;                         // a move with a button held since the last press

        void key(int32_t kind, vsg::KeyEvent& e)
        {
            if (e.handled) return;                     // (taken by a GUI panel)
            record({kind, int32_t(e.keyModified), int32_t(e.keyBase), int32_t(e.keyModifier), 0, 0, 0, 0, 0, 0, 0, 0});
        }
        void pointer(int32_t kind, vsg::PointerEvent& e, uint32_t button, int32_t grabbed = 0)
        {
            if (e.handled) return;                     // (over a GUI panel)
            const auto area = v->camera->getRenderArea();
            record({kind, grabbed, 0, 0, int32_t(button), int32_t(e.mask), e.x, e.y,
                    area.offset.x, area.offset.y, int32_t(area.extent.width), int32_t(area.extent.height)});
        }
        void record(const std::array<int32_t, 12>& event)
        {
            if (v->events.size() < max_events) v->events.push_back(event);
        }
    };

    // The viewers that exist (vkf_viewer_create to vkf_viewer_delete), oldest
    // first: Fortran keeps no viewer handle beyond a window's own (a frame tree
    // shown in several windows outlives any one of them).
    std::vector<Viewer*>& live_viewers()
    {
        static std::vector<Viewer*> viewers;
        return viewers;
    }

    // Compile a node for a viewer that is already running (before adding it to
    // its scene); nothing to do before the first frame, which compiles the
    // whole scene. False on failure.
    bool compile_for(Viewer& v, const vsg::ref_ptr<vsg::Node>& node)
    {
        if (!v.compiled) return true;
        auto result = v.viewer->compileManager->compile(node);
        if (!result) return false;
        vsg::updateViewer(*v.viewer, result);
        return true;
    }

    // The optional parts' state for the viewer's scene views (Rocky's, once
    // the viewer has maps: vkf::prepare_view), each frame; a view given it
    // while the viewer runs is compiled now.
    void prepare_views(Viewer& v)
    {
        if (!v.commandGraph) return;
        for (auto& child : v.commandGraph->children)
            if (auto renderGraph = child.cast<vsg::RenderGraph>(); renderGraph && !renderGraph->children.empty())
                if (auto view = renderGraph->children.front().cast<vsg::View>())   // (the scene's; not the HUD's)
                    if (vkf::prepare_view(v.viewer.get(), *view, v.window->getOrCreateDevice()) && v.compiled)
                    {
                        v.viewer->deviceWaitIdle();
                        compile_for(v, view);
                    }
    }

    // A driver's memory budget (VK_EXT_memory_budget), which VSG trusts when
    // allocating, ignored if it is implausible (under 10% of its heap; real
    // drivers report most of it): KosmicKrisp (Mesa 26.2) reports from 64 KiB
    // to about 300 MB of a 24 GiB heap, varying from run to run, and its
    // budget then follows its usage, so VSG finds no memory free: it allocated
    // nothing (silently) for the windows' depth images and the scene, or
    // later Rocky's terrain tiles failed to compile (Context::reserve:
    // VK_ERROR_OUT_OF_DEVICE_MEMORY), Rocky drew them anyway, and KosmicKrisp
    // crashed binding their null descriptor sets.
    void check_memory_budget(vsg::Device& device)
    {
        if (!device.getExtensions()->memory_budget) return;
        VkPhysicalDeviceMemoryBudgetPropertiesEXT budget{};
        budget.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_BUDGET_PROPERTIES_EXT;
        VkPhysicalDeviceMemoryProperties2 properties{};
        properties.sType = VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_MEMORY_PROPERTIES_2;
        properties.pNext = &budget;
        vkGetPhysicalDeviceMemoryProperties2(*device.getPhysicalDevice(), &properties);
        for (uint32_t i = 0; i < properties.memoryProperties.memoryHeapCount; ++i)
        {
            if (budget.heapBudget[i] < properties.memoryProperties.memoryHeaps[i].size / 10)
            {
                const_cast<vsg::DeviceExtensions*>(device.getExtensions())->memory_budget = false;   // (heap sizes instead)
                return;
            }
        }
    }

#if defined(_WIN32)
    // VSG's Win32 window, for a host's window (native_window in
    // vkf_viewer_create). VSG's own pumps all of the thread's messages each
    // frame (the host's too, so its handlers would run inside a frame) and
    // destroys the window when deleted; the host owns both.
    class HostWin32Window : public vsg::Inherit<vsgWin32::Win32_Window, HostWin32Window>
    {
    public:
        explicit HostWin32Window(vsg::ref_ptr<vsg::WindowTraits> traits) :
            Inherit(traits) {}

        bool pollEvents(vsg::UIEvents& events) override { return vsg::Window::pollEvents(events); }

    protected:
        ~HostWin32Window() override
        {
            if (_window) RevokeDragDrop(_window);   // (VSG registered it for file drops)
            releaseWindow();
        }
    };
#endif

    // The Vulkan device of the windows: all windows share the first one's, so a
    // scene shown in several windows has its GPU data once.
    vsg::observer_ptr<vsg::Device>& shared_device()
    {
        static vsg::observer_ptr<vsg::Device> device;
        return device;
    }

    // The Vulkan driver, loaded ahead of the first window. The loader loads it
    // in its first call, which on macOS takes 0.1 to 0.25 s, most of opening
    // the first window: KosmicKrisp's library binds about 3000 symbols with a
    // lookup through every library loaded before it (in a Qt program, many).
    // Programs set up their scene before opening a window, so a FrameManager
    // given its root frame starts loading it (vkf_driver_preload), on a thread
    // of its own, by making an instance (no extensions; the loader's calls
    // before an instance exists may be made from any thread). The instance is
    // kept until the first window has its own, so that the driver stays loaded
    // in between. A program exiting first waits for the thread.
    struct DriverPreload
    {
        std::thread thread;
        VkInstance instance = VK_NULL_HANDLE;

        void start()
        {
            thread = std::thread([this] {
                VkApplicationInfo app{VK_STRUCTURE_TYPE_APPLICATION_INFO};
                app.apiVersion = VK_API_VERSION_1_0;
                VkInstanceCreateInfo info{VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO};
                info.pApplicationInfo = &app;
                const char* portability = VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME;   // (a portability driver needs it: MoltenVK)
                if (vsg::isExtensionSupported(portability))
                {
                    info.flags |= VK_INSTANCE_CREATE_ENUMERATE_PORTABILITY_BIT_KHR;
                    info.enabledExtensionCount = 1;
                    info.ppEnabledExtensionNames = &portability;
                }
                if (vkCreateInstance(&info, nullptr, &instance) != VK_SUCCESS) instance = VK_NULL_HANDLE;
            });
        }

        // (once the first window's instance exists, and at exit)
        void finish()
        {
            if (thread.joinable()) thread.join();
            if (instance) vkDestroyInstance(instance, nullptr);
            instance = VK_NULL_HANDLE;
        }

        ~DriverPreload() { finish(); }
    };

    DriverPreload& driver_preload()
    {
        static DriverPreload preload;
        return preload;
    }

    void preload_driver()
    {
        static std::once_flag once;
        std::call_once(once, [] {
            if (!shared_device().valid()) driver_preload().start();
        });
    }

    struct Node
    {
        vsg::ref_ptr<vsg::Node> node;
        virtual ~Node() = default;
    };

    // A label (vkf_label_create): its text and layout, to change them later.
    struct Label : Node
    {
        vsg::ref_ptr<vsg::Text> text;
        vsg::ref_ptr<vsg::StandardLayout> layout;
        uint32_t capacity = 0; // glyphs allocated (texts up to this long are updated in place)
        bool truncated = false; // a text was cut to the capacity (warned once)
    };

    struct LineStrip : Node
    {
        vsg::ref_ptr<vsg::floatArray> points; // 8 floats per point: (xyz, distance) high, then low
        vsg::ref_ptr<vsg::vec4Array> style;  // [color, (width, separate segments, dash pattern, its factor)]; markers: see marker.vert
        vsg::ref_ptr<vsg::Draw> draw;
        bool markers = false;                // a quad per point
        std::shared_ptr<LineLod> lod;        // lines: the level of detail (EyeRelative)
        vsg::ref_ptr<vsg::vec4Array> colors; // lines: a color per point (colored), else one, white
        bool colored = false;
        bool ribbon = false;                 // a surface between pairs of points (vkf_ribbon_create)
        // lines: the glow and arrows of a highlight (shown with arrows: see
        // make_line), sorted with the translucent surfaces by the line's bound
        vsg::ref_ptr<vsg::Switch> glow;
        vsg::ref_ptr<vsg::DepthSorted> glowSorted;

        bool segments() const { return style->at(1).y > 0.5f; }
        // the vertices drawing points first .. first+n-1 (1-based)
        void setRange(uint32_t first, uint32_t n)
        {
            if (ribbon) {                    // a quad (6 vertices) between consecutive pairs
                draw->firstVertex = 6 * ((first - 1) / 2);
                draw->vertexCount = (n >= 4) ? 6 * (n / 2 - 1) : 0;
            } else if (markers) {
                draw->firstVertex = 6 * (first - 1);
                draw->vertexCount = 6 * n;
            } else if (segments()) {
                draw->firstVertex = 6 * ((first - 1) / 2);
                draw->vertexCount = 6 * (n / 2);
            } else if (lod) {                // a strip: drawn from lod->firstPoint (EyeRelative: its vertex count)
                draw->firstVertex = 0;
                draw->vertexCount = (n >= 2) ? 6 * (n - 1) : 0;
            } else {
                draw->firstVertex = 6 * (first - 1);
                draw->vertexCount = (n >= 2) ? 6 * (n - 1) : 0;
            }
            if (lod)
            {
                lod->firstPoint = first - 1;
                lod->drawnPoints = first - 1 + n;
                lod->strip = !markers && !ribbon && !segments();
            }
        }
    };

    // Nodes removed from the scene, kept alive until no frame in flight can
    // still use their GPU resources: released after retire_frames more frames
    // (of any window; several frames in flight per window, a few windows).
    constexpr uint64_t retire_frames = 32;
    uint64_t frames_drawn = 0;
    std::deque<std::pair<uint64_t, vsg::ref_ptr<vsg::Node>>> retired;

    void retire(vsg::ref_ptr<vsg::Node> node) { retired.emplace_back(frames_drawn + retire_frames, std::move(node)); }

    // Remove `node` from a group's children (retired); false if it is not one.
    bool remove_from(vsg::Group::Children& children, const vsg::ref_ptr<vsg::Node>& node)
    {
        auto itr = std::find(children.begin(), children.end(), node);
        if (itr == children.end()) return false;
        retire(node);
        children.erase(itr);
        return true;
    }

    // ---------------------------------------------------------------------
    // Lighting: VSG's Phong and PBR shader sets with vkframes' fragment shaders,
    // for vkframes' lights (OpenFrames' lighting model), used for spheres and
    // loaded models:
    // * a light has separate diffuse and specular colors: vkframes makes a
    //   light's specular part a VSG light of negative color (VSG drops lights
    //   whose intensity is not positive), which vkframes' Phong shader reads
    //   as specular only, and its other lights as diffuse only (PBR ignores
    //   the specular parts);
    // * point lights do not fall off with distance (as OpenGL's default,
    //   which OpenFrames uses: a Sun light at any distance);
    // * Phong: an optional night texture ("nightMap", define VKF_NIGHT_MAP,
    //   binding 9), shown where the diffuse light is weak (OpenFrames'
    //   setNightTextureMap);
    // * Phong: a body's twilight ("vkfBody", define VKF_BODY, binding 12: the
    //   sine of its twilight angle; meshes made by vkf_mesh_create), its
    //   diffuse light extended past the terminator (vkf_mesh_set_twilight);
    // * Phong: view lighting (define VKF_VIEW_LIGHTING, for frames' axes): the
    //   scene's lights are ignored and the color is shaded by how the surface
    //   faces the viewer only (soft, the same from any side).
    // The shaders are vkframes' copies of VSG's (shaders/vsg/standard_phong.frag
    // and standard_pbr.frag, VSG 1.1.16), with these changes marked; they
    // take the place of VSG's fragment stage in its shader sets (VSG's vertex
    // stage, descriptor layout and variant compiling are kept).

    // The shader set with `glsl` as its fragment stage. VSG's precompiled
    // variants (SPIR-V of its own source for common #defines) are dropped:
    // its variants are compiled from this source.
    void use_fragment_source(vsg::ShaderSet& shaderSet, const char* glsl)
    {
        for (auto& stage : shaderSet.stages)
        {
            if (stage->stage != VK_SHADER_STAGE_FRAGMENT_BIT || !stage->module) continue;
            stage->module = vsg::ShaderModule::create(std::string(glsl), stage->module->hints);
        }
        shaderSet.variants.clear();
    }

    vsg::ref_ptr<vsg::ShaderSet> phong_shader_set()
    {
        auto shaderSet = vsg::createPhongShaderSet();
        use_fragment_source(*shaderSet, vkf_shaders::standard_phong_frag_glsl);
        shaderSet->addDescriptorBinding("nightMap", "VKF_NIGHT_MAP", 1, 9, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 1,
                                        VK_SHADER_STAGE_FRAGMENT_BIT,
                                        vsg::ubvec4Array2D::create(1, 1, vsg::Data::Properties{VK_FORMAT_R8G8B8A8_UNORM}));
        shaderSet->addDescriptorBinding("vkfBody", "VKF_BODY", 1, 12, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1,
                                        VK_SHADER_STAGE_FRAGMENT_BIT, vsg::vec4Value::create(vsg::vec4(0.0f, 0.0f, 0.0f, 0.0f)));
        return shaderSet;
    }

    vsg::ref_ptr<vsg::ShaderSet> pbr_shader_set()
    {
        auto shaderSet = vsg::createPhysicsBasedRenderingShaderSet();
        use_fragment_source(*shaderSet, vkf_shaders::standard_pbr_frag_glsl);
        return shaderSet;
    }

    // Color: as in OpenGL (and so OpenFrames), everything is in display
    // (sRGB-encoded) values: windows have UNORM framebuffers (VSG's default is
    // sRGB, which would brighten colors and blend in linear light) and images
    // are read as UNORM (their bytes as they are, not decoded to linear). So a
    // color given to vkframes appears as given, textures as in their files, and
    // transparency and lighting look as in OpenFrames.
    vsg::vec4 to_vec4(const float* rgba) { return vsg::vec4(rgba[0], rgba[1], rgba[2], rgba[3]); }

    // The UNORM format for an sRGB one (else the format).
    VkFormat unorm_format(VkFormat format)
    {
        switch (format)
        {
        case VK_FORMAT_R8_SRGB: return VK_FORMAT_R8_UNORM;
        case VK_FORMAT_R8G8_SRGB: return VK_FORMAT_R8G8_UNORM;
        case VK_FORMAT_R8G8B8_SRGB: return VK_FORMAT_R8G8B8_UNORM;
        case VK_FORMAT_B8G8R8_SRGB: return VK_FORMAT_B8G8R8_UNORM;
        case VK_FORMAT_R8G8B8A8_SRGB: return VK_FORMAT_R8G8B8A8_UNORM;
        case VK_FORMAT_B8G8R8A8_SRGB: return VK_FORMAT_B8G8R8A8_UNORM;
        case VK_FORMAT_BC1_RGB_SRGB_BLOCK: return VK_FORMAT_BC1_RGB_UNORM_BLOCK;
        case VK_FORMAT_BC1_RGBA_SRGB_BLOCK: return VK_FORMAT_BC1_RGBA_UNORM_BLOCK;
        case VK_FORMAT_BC2_SRGB_BLOCK: return VK_FORMAT_BC2_UNORM_BLOCK;
        case VK_FORMAT_BC3_SRGB_BLOCK: return VK_FORMAT_BC3_UNORM_BLOCK;
        case VK_FORMAT_BC7_SRGB_BLOCK: return VK_FORMAT_BC7_UNORM_BLOCK;
        default: return format;
        }
    }

    // Options for reading files: vsgXchange's readers (images, models, fonts),
    // with vkframes' shader sets (used by models and meshes).
    vsg::ref_ptr<vsg::Options> read_options()
    {
        static auto options = [] {
            auto o = vsg::Options::create(vsgXchange::all::create());
            o->shaderSets["phong"] = phong_shader_set();
            o->shaderSets["pbr"] = pbr_shader_set();
            return o;
        }();
        return options;
    }

    // A light (vkf_light_create): ambient, diffuse and specular parts, as a
    // directional light or a point light (the other's intensities are 0).
    struct LightNode : Node
    {
        vsg::ref_ptr<vsg::AmbientLight> ambient;
        vsg::ref_ptr<vsg::DirectionalLight> directional, directionalSpecular;
        vsg::ref_ptr<vsg::PointLight> point, pointSpecular;
    };

    struct Image
    {
        vsg::ref_ptr<vsg::Data> data;
    };

    // A triangle mesh drawn with VSG's Phong shader set; its arrays are filled
    // by Fortran in place before the first frame.
    struct Mesh : Node
    {
        vsg::ref_ptr<vsg::vec3Array> vertices;
        vsg::ref_ptr<vsg::vec3Array> normals;
        vsg::ref_ptr<vsg::vec2Array> texcoords;
        vsg::ref_ptr<vsg::uintArray> indices;
        vsg::ref_ptr<vsg::vec4Array> colors;              // a color per vertex (flag 32), or one for all
        vsg::ref_ptr<vsg::PhongMaterialValue> material;   // lit meshes: changeable (vkf_mesh_material)
        vsg::ref_ptr<vsg::vec4Value> body;                // lit meshes: x = the sine of the twilight angle
    };

    // Pipeline states of unlit transparent surfaces (planes, cone sides):
    // blended, seen from both sides, not writing depth (OpenFrames'
    // RadialPlane and PolyhedralCone state sets).
    struct TransparentStates : public vsg::Visitor
    {
        void apply(vsg::Object& object) override { object.traverse(*this); }
        void apply(vsg::RasterizationState& state) override { state.cullMode = VK_CULL_MODE_NONE; }
        void apply(vsg::DepthStencilState& state) override { state.depthWriteEnable = VK_FALSE; }
        void apply(vsg::ColorBlendState& state) override { state.configureAttachments(true); }
    };

    // Pipeline states of lit translucent surfaces (a translucent Sphere): blended,
    // front faces only (the back of a closed surface isn't drawn through it;
    // or both, for an open one), not writing depth. Drawn in translucent_bin,
    // after the opaque scene.
    struct TranslucentStates : public vsg::Visitor
    {
        VkCullModeFlags cullMode = VK_CULL_MODE_BACK_BIT;
        void apply(vsg::Object& object) override { object.traverse(*this); }
        void apply(vsg::RasterizationState& state) override { state.cullMode = cullMode; }
        void apply(vsg::DepthStencilState& state) override { state.depthWriteEnable = VK_FALSE; }
        void apply(vsg::ColorBlendState& state) override { state.configureAttachments(true); }
    };

    // Pipeline states of lit opaque two-sided surfaces (an open Polyhedron):
    // no faces culled (the back lit as the front: VSG_TWO_SIDED_LIGHTING).
    struct TwoSidedStates : public vsg::Visitor
    {
        void apply(vsg::Object& object) override { object.traverse(*this); }
        void apply(vsg::RasterizationState& state) override { state.cullMode = VK_CULL_MODE_NONE; }
    };

    // The bin of translucent meshes (vsg::DepthSorted), drawn after the
    // opaque scene, farthest first (each window's scene view has it).
    constexpr int32_t translucent_bin = 10;

    vsg::ref_ptr<vsg::vec4Array> dynamic_vec4_array(uint32_t n)
    {
        auto a = vsg::vec4Array::create(n, vsg::vec4(0.0f, 0.0f, 0.0f, 1.0f));
        a->properties.dataVariance = vsg::DYNAMIC_DATA;
        return a;
    }

    // The sky's stars or texture (vkf_stars_create, vkf_sky_texture_create):
    // stars hold 8 floats per star; style is the sky's attitude (a mat4) and
    // parameters (a vec4).
    struct SkyNode : Node
    {
        vsg::ref_ptr<vsg::floatArray> stars;
        vsg::ref_ptr<vsg::vec4Array> style;
        vsg::ref_ptr<vsg::Draw> draw;
    };

    vsg::ref_ptr<vsg::vec4Array> sky_style()
    {
        auto style = dynamic_vec4_array(5);
        for (int i = 0; i < 4; ++i)
            style->at(i) = vsg::vec4(i == 0, i == 1, i == 2, i == 3); // the identity
        return style;
    }

    vsg::dmat4 to_dmat4(const double* m)
    {
        vsg::dmat4 mat;
        for (int c = 0; c < 4; ++c)
            for (int r = 0; r < 4; ++r) mat[c][r] = m[4 * c + r];
        return mat;
    }

    // Store the animation time on the views of a command graph (see
    // animation_time_key; negative: frame_time()).
    void set_animation_time(vsg::CommandGraph& commandGraph, float t)
    {
        for (auto& child : commandGraph.children)
            if (auto renderGraph = child.cast<vsg::RenderGraph>())
                for (auto& grandchild : renderGraph->children)
                    if (auto view = grandchild.cast<vsg::View>()) view->setValue(animation_time_key, t);
    }

    // Write a recorder's slot to ffmpeg (see Recorder), once its frame is done:
    // ffmpeg is started at the first frame (the video's size and byte order
    // known then). False on failure (the recorder then fails).
    bool write_slot(Recorder& rec, Recorder::Slot& slot)
    {
        if (!slot.pending || rec.failed) return !rec.failed;
        slot.pending = false;
        if (slot.fence && slot.fence->wait(std::numeric_limits<uint64_t>::max()) != VK_SUCCESS) rec.failed = true;
        if (!rec.failed && !rec.pipe)
        {
            char size[32];
            std::snprintf(size, sizeof(size), "%ux%u", rec.width, rec.height);
            std::string command = rec.ffmpeg + " -hide_banner -loglevel error -y -f rawvideo -pix_fmt " +
                                  (rec.bgra ? "bgra" : "rgba") + " -s " + size + " -r " + std::to_string(rec.fps) +
                                  " -i - " + rec.output;
#if defined(_WIN32)
            command = "\"" + command + "\"";       // (cmd.exe strips the outer quotes)
            rec.pipe = _popen(command.c_str(), "wb");
#else
            rec.pipe = popen(command.c_str(), "w");
#endif
            if (!rec.pipe)
            {
                std::cerr << "vkframes: recording: could not start " << command << std::endl;
                rec.failed = true;
            }
        }
        if (rec.failed) return false;
        const size_t size = size_t(rec.width) * rec.height * 4;
        if (std::fwrite(slot.mapped, 1, size, rec.pipe) != size)
        {
            std::cerr << "vkframes: recording: ffmpeg stopped (see its messages above)" << std::endl;
            rec.failed = true;
            return false;
        }
        ++rec.written;
        return true;
    }

    // Stop a viewer's recording: the frames copied written, ffmpeg's input
    // closed (it finishes the file) and the buffers released. Returns the
    // frames written, or -1 on failure.
    long long finish_recording(Viewer& v)
    {
        auto& rec = v.recorder;
        if (!rec.active) return -1;
        if (v.compiled) v.viewer->deviceWaitIdle();
        if (rec.previous >= 0) write_slot(rec, rec.slots[rec.previous]);   // (the older first)
        for (auto& slot : rec.slots) write_slot(rec, slot);
        bool ok = !rec.failed;
        if (rec.pipe)
        {
#if defined(_WIN32)
            ok = _pclose(rec.pipe) == 0 && ok;
#else
            ok = pclose(rec.pipe) == 0 && ok;
#endif
        }
        const long long written = static_cast<long long>(rec.written);
        for (auto& slot : rec.slots)
            if (slot.buffer && slot.mapped)
                if (auto memory = slot.buffer->getDeviceMemory(v.window->getDevice()->deviceID)) memory->unmap();
        rec = Recorder{};
        if (v.commandGraph) set_animation_time(*v.commandGraph, -1.0f);
        return ok ? written : -1;
    }

    // Store the pixel ratio on the views of a command graph (read by EyeRelative).
    void set_pixel_ratio(vsg::CommandGraph& commandGraph, float ratio)
    {
        for (auto& child : commandGraph.children)
            if (auto renderGraph = child.cast<vsg::RenderGraph>())
                for (auto& grandchild : renderGraph->children)
                    if (auto view = grandchild.cast<vsg::View>()) view->setValue(pixel_ratio_key, ratio);
    }

    // The size the views of a window are displayed at (see display_size_key).
    void set_display_size(vsg::CommandGraph& commandGraph, float width, float height)
    {
        for (auto& child : commandGraph.children)
            if (auto renderGraph = child.cast<vsg::RenderGraph>())
                for (auto& grandchild : renderGraph->children)
                    if (auto view = grandchild.cast<vsg::View>()) view->setValue(display_size_key, vsg::vec2(width, height));
    }

} // namespace

extern "C" {

// (defined with the lights, below; used by vkf_viewer_create)
VKF_API void* vkf_light_create();
VKF_API int vkf_light_set(void* light, const float* ambient, const float* diffuse, const float* specular,
                          const double* position);

// Start loading the Vulkan driver for the first window (DriverPreload), unless
// a window is open already.
VKF_API void vkf_driver_preload()
{
    guarded("vkf_driver_preload", 0, [&]() {
        preload_driver();
        return 0;
    });
}

// Name of the first Vulkan physical device; returns its length, or -1 on failure.
// Needs no window, so it checks that the Vulkan loader finds a driver (MoltenVK on macOS).
VKF_API int vkf_vulkan_device_name(char* buf, int buflen)
{
    return guarded("vkf_vulkan_device_name", -1, [&]() {
        vsg::Names extensions;
        if (vsg::isExtensionSupported(VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME))
            extensions.push_back(VK_KHR_PORTABILITY_ENUMERATION_EXTENSION_NAME);
        auto instance = vsg::Instance::create(extensions, vsg::Names{});
        const auto& devices = instance->getPhysicalDevices();
        if (devices.empty()) return -1;
        return to_fortran(devices.front()->getProperties().deviceName, buf, buflen);
    });
}

// ---------------------------------------------------------------------------
// Viewer: one window, one scene, one trackball camera

// A viewer with one window, an empty scene and a trackball camera; null on failure.
//
// width, height: the window size in window units (logical pixels).
// native_window: 0 for a window of its own, or the native handle of an existing
//   window to render into (e.g. a Qt QWindow's winId(): an NSView* on macOS, an
//   HWND on Windows, an xcb window on Linux). The host then owns that window's
//   events and forwards them (vkf_viewer_mouse, ..., vkf_viewer_resize).
// pixel_ratio: framebuffer pixels per window unit of an existing window (Qt's
//   devicePixelRatio); ignored for a window of its own.
// vsync: 1 to present at the display's refresh (FIFO), 0 as soon as a frame is
//   ready (IMMEDIATE, where the driver has it; else FIFO), e.g. for benchmarks.
VKF_API void* vkf_viewer_create(int width, int height, const char* title, int title_len, long long native_window, double pixel_ratio,
                                int vsync)
{
    return guarded("vkf_viewer_create", static_cast<void*>(nullptr), [&]() -> void* {
        auto traits = vsg::WindowTraits::create();
        traits->windowTitle = to_string(title, title_len);
        traits->width = static_cast<uint32_t>(width);
        traits->height = static_cast<uint32_t>(height);
        traits->swapchainPreferences.imageUsage |= VK_IMAGE_USAGE_TRANSFER_SRC_BIT; // for capture
        traits->swapchainPreferences.surfaceFormat = {VK_FORMAT_B8G8R8A8_UNORM, VK_COLOR_SPACE_SRGB_NONLINEAR_KHR}; // see "Color"
        traits->swapchainPreferences.presentMode = vsync ? VK_PRESENT_MODE_FIFO_KHR : VK_PRESENT_MODE_IMMEDIATE_KHR;
        void* childView = nullptr;
        if (native_window != 0)
        {
            // the type each VSG platform window expects in nativeWindow
#if defined(_WIN32)
            traits->nativeWindow = reinterpret_cast<HWND>(native_window);
#elif defined(__APPLE__)
            childView = create_child_view(reinterpret_cast<void*>(native_window));
            traits->nativeWindow = reinterpret_cast<unsigned long long>(childView); // takes the size in points
#else
            traits->nativeWindow = static_cast<uint32_t>(native_window); // xcb_window_t
#endif
#if !defined(__APPLE__)
            traits->width = static_cast<uint32_t>(std::lround(width * pixel_ratio)); // in pixels
            traits->height = static_cast<uint32_t>(std::lround(height * pixel_ratio));
#endif
        }

        // share the device of the windows already open (created with the first window)
        auto& device = shared_device();
        auto existing = device.ref_ptr();
#if !defined(__APPLE__)
        if (existing) traits->device = existing;
#endif
        vkf::device_features(*traits);

        auto v = std::make_unique<Viewer>();
        v->childView = childView;
#if defined(_WIN32)
        if (native_window != 0)
            v->window = HostWin32Window::create(traits);
        else
#endif
            v->window = vsg::Window::create(traits);
#if defined(__APPLE__)
        if (native_window == 0) accept_first_mouse_in_vsg_windows(); // (VSG's view class is registered by now)
        // the instance or shared device, and a Metal surface (see "macOS windows")
        if (existing)
            v->window->setDevice(existing);
        else
            v->window->setInstance(metal_instance(*traits));
        v->metalLayer = new_window_layer(childView);
        if (childView) disable_layer_animations(v->metalLayer);      // (see disable_layer_animations)
        if (!childView) limit_window_size(v->metalLayer);            // (never empty: see limit_window_size)
        v->window->setSurface(vsg::ref_ptr<vsg::Surface>(new MetalSurface(v->window->getOrCreateInstance(), v->metalLayer)));
        if (existing) WindowSetup::init_formats(*v->window);
#endif
        if (!device.valid())
        {
            auto created = v->window->getOrCreateDevice();
            check_memory_budget(*created);
            device = created;
        }
        v->viewer = vsg::Viewer::create();
        v->viewer->addWindow(v->window);
        v->scene = vsg::Group::create();
        v->hudScene = vsg::Group::create();   // (so that HUD text can also be added while running)
        // the default light: in eye coordinates (OpenFrames' headlight)
        auto light = static_cast<LightNode*>(vkf_light_create());
        auto eye = vsg::AbsoluteTransform::create();
        eye->addChild(light->node);
        v->headlight = vsg::Switch::create();
        v->headlight->addChild(true, eye);
        v->headlightHandle = light;
        v->scene->addChild(v->headlight);
        const auto extent = v->window->extent2D();
        v->pixelRatio = (native_window != 0) ? static_cast<float>(pixel_ratio)
                                             : static_cast<float>(extent.width) / static_cast<float>(width);

        const double aspect = static_cast<double>(extent.width) / static_cast<double>(extent.height);
        auto lookAt = ViewLookAt::create(vsg::dvec3(0.0, -4.0, 0.0), vsg::dvec3(0.0, 0.0, 0.0), vsg::dvec3(0.0, 0.0, 1.0));
        auto perspective = vsg::Perspective::create(30.0, aspect, 0.001, 1000.0);
        v->camera = vsg::Camera::create(perspective, lookAt, vsg::ViewportState::create(extent));

        v->viewer->addEventHandler(vsg::CloseHandler::create(v->viewer));
        v->viewer->addEventHandler(EventRecorder::create(v.get()));
        // the trackball (mouse only, as OpenFrames': its keys are the program's
        // and vkframes' built-in keys, WindowProxy: no viewpoint on the space
        // bar, no camera moves on w/a/s/d, q/e, i/o and the arrow keys)
        auto trackball = vsg::Trackball::create(v->camera);
        trackball->keyViewpointMap.clear();
        for (auto key : {&trackball->turnLeftKey, &trackball->turnRightKey, &trackball->pitchUpKey, &trackball->pitchDownKey,
                         &trackball->rollLeftKey, &trackball->rollRightKey, &trackball->moveForwardKey, &trackball->moveBackwardKey,
                         &trackball->moveLeftKey, &trackball->moveRightKey, &trackball->moveUpKey, &trackball->moveDownKey})
            *key = vsg::KEY_Undefined;
        v->viewer->addEventHandler(trackball);
        v->trackball = trackball;
        live_viewers().push_back(v.get());
        driver_preload().finish();   // (the driver is loaded for the window's instance now)
        return v.release();
    });
}

VKF_API void vkf_viewer_delete(void* viewer)
{
    auto v = static_cast<Viewer*>(viewer);
    guarded("vkf_viewer_delete", 0, [&]() {
        if (v && v->recorder.active) finish_recording(*v);
        if (v && v->compiled) v->viewer->deviceWaitIdle();
        if (v && v->gui) vkf::gui_delete(v->gui, *v->viewer);
        if (v) delete static_cast<Node*>(v->headlightHandle);
        auto vsgViewer = v ? v->viewer : vsg::ref_ptr<vsg::Viewer>();
        auto& viewers = live_viewers();
        viewers.erase(std::remove(viewers.begin(), viewers.end(), v), viewers.end());
#if defined(__APPLE__)
        void* childView = v ? v->childView : nullptr;
        if (v) metal_layers().erase(v->metalLayer);
#endif
        delete v;
        if (vsgViewer) vkf::release_viewer(vsgViewer.get());   // the optional parts' state for it (Rocky)
        vsgViewer = nullptr;                                   // (the window goes with it)
#if defined(__APPLE__)
        delete_child_view(childView);
#endif
        return 0;
    });
}

// Events from a host that owns the window (native_window in vkf_viewer_create),
// already converted to VSG's conventions by the caller: positions in
// framebuffer pixels from the top left; button 1 = left, 2 = middle, 3 =
// right; mask = the buttons held (VSG's BUTTON_MASK_1/2/3 = 256/512/1024).

// kind: 1 = move, 2 = button press, 3 = button release.
VKF_API int vkf_viewer_mouse(void* viewer, int kind, int x, int y, int mask, int button)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    const auto t = vsg::clock::now();
    const auto m = static_cast<vsg::ButtonMask>(mask);
    auto& events = v->window->bufferedEvents;
    switch (kind)
    {
    case 1: events.push_back(vsg::MoveEvent::create(v->window, t, x, y, m)); break;
    case 2: events.push_back(vsg::ButtonPressEvent::create(v->window, t, x, y, m, static_cast<uint32_t>(button))); break;
    case 3: events.push_back(vsg::ButtonReleaseEvent::create(v->window, t, x, y, m, static_cast<uint32_t>(button))); break;
    default: return -1;
    }
    return 0;
}

// The keyboard and mouse events since the last call (see EventRecorder), up to
// `capacity` (12 integers each, into `out`), oldest first. Returns how many
// were copied (the rest are kept for the next call), or -1 on failure.
VKF_API int vkf_viewer_events(void* viewer, int* out, int capacity)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v || capacity < 0) return -1;
    const auto n = std::min(v->events.size(), static_cast<size_t>(capacity));
    for (size_t i = 0; i < n; ++i)
        std::copy(v->events[i].begin(), v->events[i].end(), out + 12 * i);
    v->events.erase(v->events.begin(), v->events.begin() + static_cast<std::ptrdiff_t>(n));
    return static_cast<int>(n);
}

// The swapchain's size (framebuffer pixels): a VSG window's size, as it is
// resized (a host's window has the size it gives, vkf_viewer_resize).
VKF_API int vkf_viewer_size(void* viewer, int* width, int* height)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    const auto extent = v->window->extent2D();
    *width = static_cast<int>(extent.width);
    *height = static_cast<int>(extent.height);
    return 0;
}

// Scroll wheel: dy > 0 away from the user.
VKF_API int vkf_viewer_scroll(void* viewer, float dy)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    v->window->bufferedEvents.push_back(vsg::ScrollWheelEvent::create(v->window, vsg::clock::now(), vsg::vec3(0.0f, dy, 0.0f)));
    return 0;
}

// A key press (pressed = 1) or release (0), as VSG key symbols and modifiers
// (vsg/ui/KeyEvent.h: ASCII for printable keys, X11-style codes otherwise).
VKF_API int vkf_viewer_key(void* viewer, int pressed, int symbol, int modified_symbol, int modifier)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    const auto t = vsg::clock::now();
    const auto key = static_cast<vsg::KeySymbol>(symbol);
    const auto modifiedKey = static_cast<vsg::KeySymbol>(modified_symbol);
    const auto mod = static_cast<vsg::KeyModifier>(modifier);
    if (pressed)
        v->window->bufferedEvents.push_back(vsg::KeyPressEvent::create(v->window, t, key, modifiedKey, mod));
    else
        v->window->bufferedEvents.push_back(vsg::KeyReleaseEvent::create(v->window, t, key, modifiedKey, mod));
    return 0;
}

// The host resized the window (new size in framebuffer pixels): rebuild the swapchain.
VKF_API int vkf_viewer_resize(void* viewer, int width, int height)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    return guarded("vkf_viewer_resize", -1, [&]() {
        // no width or height (e.g. a collapsed splitter): not drawn (Metal has
        // no drawables of no size, and a swapchain can't be built for it) until
        // it has a size again
        v->empty = width <= 0 || height <= 0;
        if (v->empty) return 0;
        v->window->bufferedEvents.push_back(vsg::ConfigureWindowEvent::create(
            v->window, vsg::clock::now(), 0, 0, static_cast<uint32_t>(width), static_cast<uint32_t>(height)));
        if (!v->compiled)
            v->resizePending = true;
#if defined(__APPLE__)
        else if (v->childView)                                  // rebuilt once the size settles
        {
            v->resizeDeferred = true;
            v->lastResize = vsg::clock::now();
            // drawn with the new aspect ratio meanwhile (scaled to the new size by Core Animation)
            if (auto perspective = v->camera->projectionMatrix.cast<vsg::Perspective>(); perspective && height > 0)
                perspective->aspectRatio = double(width) / double(height);
            // and sizes in pixels (labels, HUD text, markers, line widths) for the new size
            if (v->commandGraph) set_display_size(*v->commandGraph, float(width), float(height));
        }
#endif
        else
            v->window->resize();
        return 0;
    });
}

// A window embedded in a host now has `ratio` framebuffer pixels per window
// unit (Qt's devicePixelRatio changed: moved to a screen of another scale, or
// the display's scaling changed). Sizes in window units (line widths, markers,
// labels) are scaled by it; the host also resizes (vkf_viewer_resize) to the
// new framebuffer size, which rebuilds the swapchain (on macOS VSG also takes
// the layer's scale from its window then).
// Framebuffer pixels per window unit (2 on Retina; a host's as given).
VKF_API double vkf_viewer_pixel_ratio(void* viewer)
{
    auto v = static_cast<Viewer*>(viewer);
    return v ? static_cast<double>(v->pixelRatio) : 1.0;
}

VKF_API int vkf_viewer_set_pixel_ratio(void* viewer, double ratio)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v || !(ratio > 0.0)) return -1;
    v->pixelRatio = static_cast<float>(ratio);
    if (v->commandGraph) set_pixel_ratio(*v->commandGraph, v->pixelRatio);
    return 0;
}

// The host is closing the window: the next vkf_viewer_frame returns 0.
VKF_API int vkf_viewer_close(void* viewer)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    v->window->bufferedEvents.push_back(vsg::CloseWindowEvent::create(v->window, vsg::clock::now()));
    return 0;
}

// The camera's eye, center and up vectors (3 doubles each, in view-frame
// coordinates), as the trackball left them.
VKF_API int vkf_viewer_get_look_at(void* viewer, double* eye, double* center, double* up)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    auto lookAt = v->camera->viewMatrix.cast<vsg::LookAt>();
    if (!lookAt) return -1;
    for (int i = 0; i < 3; ++i)
    {
        eye[i] = lookAt->eye[i];
        center[i] = lookAt->center[i];
        up[i] = lookAt->up[i];
    }
    return 0;
}

// The transform from world (scene root) to view-frame coordinates (4x4
// doubles), applied after the trackball's eye/center/up (see ViewLookAt).
VKF_API int vkf_viewer_set_view_transform(void* viewer, const double* m)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    auto lookAt = v->camera->viewMatrix.cast<ViewLookAt>();
    if (!lookAt) return -1;
    lookAt->worldToView = to_dmat4(m);
    return 0;
}

// An orthographic projection with the given view-volume bounds.
VKF_API int vkf_viewer_orthographic(void* viewer, double left, double right, double bottom, double top,
                                    double near_distance, double far_distance)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    v->camera->projectionMatrix = vsg::Orthographic::create(left, right, bottom, top, near_distance, far_distance);
    return 0;
}

// Set the camera's eye, center and up vectors (3 doubles each, in view-frame coordinates).
VKF_API int vkf_viewer_look_at(void* viewer, const double* eye, const double* center, const double* up)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    auto lookAt = v->camera->viewMatrix.cast<vsg::LookAt>();
    if (!lookAt) return -1;
    lookAt->eye.set(eye[0], eye[1], eye[2]);
    lookAt->center.set(center[0], center[1], center[2]);
    lookAt->up.set(up[0], up[1], up[2]);
    return 0;
}

// The background (clear) color, rgba.
VKF_API int vkf_viewer_set_background(void* viewer, const float* rgba)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    const vsg::vec4 color = to_vec4(rgba);
    v->window->clearColor() = color;          // used when the scene is compiled
    if (v->commandGraph)                      // already compiled: the render graph has its own copy
    {
        for (auto& child : v->commandGraph->children)
            if (auto renderGraph = child.cast<vsg::RenderGraph>())
                renderGraph->setClearValues(VkClearColorValue{{color.r, color.g, color.b, color.a}});
    }
    return 0;
}

// Set the camera's vertical field of view (degrees) and near/far distances.
VKF_API int vkf_viewer_perspective(void* viewer, double fovy, double near_distance, double far_distance)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    auto perspective = v->camera->projectionMatrix.cast<vsg::Perspective>();
    if (!perspective) // e.g. after an orthographic projection
    {
        const auto extent = v->window->extent2D();
        perspective = vsg::Perspective::create(fovy, double(extent.width) / double(extent.height), near_distance, far_distance);
        v->camera->projectionMatrix = perspective;
    }
    perspective->fieldOfViewY = fovy;
    perspective->nearDistance = near_distance;
    perspective->farDistance = far_distance;
    return 0;
}

// Add a node to the viewer's scene (the scene holds a reference; the node handle
// can be deleted afterwards), also while running (it is compiled first).
VKF_API int vkf_viewer_add_node(void* viewer, void* node)
{
    auto v = static_cast<Viewer*>(viewer);
    auto n = static_cast<Node*>(node);
    if (!v || !n) return -1;
    return guarded("vkf_viewer_add_node", -1, [&]() {
        if (!compile_for(*v, n->node)) return -1;
        v->scene->addChild(n->node);
        return 0;
    });
}

// Remove a node from the viewer's scene (its GPU data are released a few
// frames later). Returns -1 if it is not in the scene.
VKF_API int vkf_viewer_remove_node(void* viewer, void* node)
{
    auto v = static_cast<Viewer*>(viewer);
    auto n = static_cast<Node*>(node);
    if (!v || !n) return -1;
    return remove_from(v->scene->children, n->node) ? 0 : -1;
}

// The first part of a frame (compiling the scene on the first call): up to the
// window's events handled (the trackball has moved the camera), before the
// scene is drawn (vkf_viewer_frame_draw): a program can still change the
// scene for this frame in between. Returns 1 while the viewer is running, 0
// once its window has been closed, -1 on failure.
VKF_API int vkf_viewer_frame_begin(void* viewer)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    ++frames_drawn;
    while (!retired.empty() && retired.front().first <= frames_drawn) retired.pop_front();
    return guarded("vkf_viewer_frame", -1, [&]() {
        if (v->empty)                                   // (a host's window of no size: not drawn, see vkf_viewer_resize)
        {
            v->skipped = true;
            return 1;
        }
        if (!v->compiled)
        {
            // (no VSG headlight: the window's light is vkframes', v->headlight)
            v->commandGraph = vsg::createCommandGraphForView(v->window, v->camera, v->scene, VK_SUBPASS_CONTENTS_INLINE, false);
            for (auto& child : v->commandGraph->children)   // translucent meshes: after the opaque scene, farthest first
                if (auto renderGraph = child.cast<vsg::RenderGraph>(); renderGraph && !renderGraph->children.empty())
                    if (auto view = renderGraph->children.front().cast<vsg::View>())
                        view->bins.push_back(vsg::Bin::create(translucent_bin, vsg::Bin::DESCENDING));
            if (v->hudScene)
            {
                // the HUD: a second view in the same render pass, after the scene
                auto hudCamera = vsg::Camera::create(HudProjection::create(),
                                                     vsg::LookAt::create(vsg::dvec3(0.0, 0.0, 0.0), vsg::dvec3(0.0, 0.0, -1.0), vsg::dvec3(0.0, 1.0, 0.0)),
                                                     vsg::ViewportState::create(v->window->extent2D()));
                for (auto& child : v->commandGraph->children)
                    if (auto renderGraph = child.cast<vsg::RenderGraph>()) renderGraph->addChild(vsg::View::create(hudCamera, v->hudScene));
            }
            set_pixel_ratio(*v->commandGraph, v->pixelRatio);
            if (v->gui) vkf::gui_attach(v->gui, v->window, *v->viewer, *v->commandGraph);   // (on top of the HUD)
            v->commandGraph->addChild(GrabFrame::create(v->window, &v->recorder));      // (after the render pass)
            v->viewer->assignRecordAndSubmitTaskAndPresentation({v->commandGraph});
            prepare_views(*v);                                  // (compiled with the scene)
            v->viewer->compile();
            v->compiled = true;
            if (v->resizePending) v->window->resize();
            v->resizePending = false;
        }
        prepare_views(*v);
        if (v->resizeDeferred &&
            std::chrono::duration<double>(vsg::clock::now() - v->lastResize).count() >= resize_settle)
        {
            // the size has settled (see resize_settle): VSG scales the aspect ratio
            // by the change of size, so it is the old size's again first
            const auto extent = v->window->extent2D();
            if (auto perspective = v->camera->projectionMatrix.cast<vsg::Perspective>(); perspective && extent.height > 0)
                perspective->aspectRatio = double(extent.width) / double(extent.height);
            v->window->resize();
            v->resizeDeferred = false;
            if (v->commandGraph) set_display_size(*v->commandGraph, 0.0f, 0.0f);   // (the render area's again)
        }
#if defined(__APPLE__)
        // VSG's own windows (not embedded): VSG rebuilds the swapchain only when
        // acquiring an image reports it out of date, which KosmicKrisp does not
        // when the window is resized (the old size is drawn, stretched). VSG does
        // queue the new size (ConfigureWindowEvent, in pixels): rebuilt here,
        // before the next image is acquired.
        if (v->resizeOwn) v->window->resize();
        v->resizeOwn = false;
#endif
        if (!v->viewer->advanceToNextFrame()) return 0;
#if defined(__APPLE__)
        if (!v->childView)
        {
            const auto extent = v->window->extent2D();
            for (auto& event : v->viewer->getEvents())
                if (auto configure = event.cast<vsg::ConfigureWindowEvent>(); configure && configure->window.ref_ptr() == v->window &&
                    configure->width > 0 && configure->height > 0 &&       // (never empty: see limit_window_size)
                    (configure->width != extent.width || configure->height != extent.height))
                    v->resizeOwn = true;
        }
#endif
        v->viewer->handleEvents();
        return 1;
    });
}

// The rest of a frame begun by vkf_viewer_frame_begin: the scene's updates,
// drawn and presented. Returns 1, or -1 on failure.
VKF_API int vkf_viewer_frame_draw(void* viewer)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    return guarded("vkf_viewer_frame_draw", -1, [&]() {
        if (v->skipped)                                 // (not begun: vkf_viewer_frame_begin)
        {
            v->skipped = false;
            return 1;
        }
        auto& rec = v->recorder;
        if (rec.active)        // (animations at the video's time)
            set_animation_time(*v->commandGraph, rec.animationStart + static_cast<float>(double(rec.copied) / rec.fps));
        v->viewer->update();
        v->viewer->recordAndSubmit();
        if (rec.active && rec.current >= 0 && !v->viewer->recordAndSubmitTasks.empty())
            rec.slots[rec.current].fence = v->viewer->recordAndSubmitTasks.front()->fence();   // (this frame's)
        v->viewer->present();
        if (rec.active)        // the frame before (done, or nearly), while this one renders
        {
            if (rec.previous >= 0 && rec.previous != rec.current) write_slot(rec, rec.slots[rec.previous]);
            rec.previous = rec.current;
        }
        vkf::gui_frame_done(v->gui);                   // (the panels are described again for the next frame)
        return 1;
    });
}

// The viewer's GUI panels (vkf_gui.cpp), made on first use with the font file
// `font` (empty: ImGui's) of `size` window units: a handle for the vkf_gui_*
// functions, valid while the viewer exists.
VKF_API void* vkf_viewer_gui(void* viewer, const char* font, int font_len, double size)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return nullptr;
    return guarded("vkf_viewer_gui", static_cast<void*>(nullptr), [&]() -> void* {
        if (!v->gui) v->gui = vkf::gui_create(v->pixelRatio, to_string(font, font_len), static_cast<float>(size));
        if (v->compiled && !vkf::gui_attached(v->gui)) vkf::gui_attach(v->gui, v->window, *v->viewer, *v->commandGraph);
        return v->gui;
    });
}

// Render one frame (vkf_viewer_frame_begin, then vkf_viewer_frame_draw).
// Returns 1 while the viewer is running, 0 once its window has been closed,
// -1 on failure.
VKF_API int vkf_viewer_frame(void* viewer)
{
    const int status = vkf_viewer_frame_begin(viewer);
    return status == 1 ? vkf_viewer_frame_draw(viewer) : status;
}

// Start recording the frames drawn to a video, `fps` frames per second of
// it: piped to `ffmpeg` (its command: the program, quoted if need be) as raw
// video, encoded with `output` (ffmpeg's output options and the file, quoted;
// e.g. -c:v libx264 -crf 18 -pix_fmt yuv420p "orbit.mp4"). See Recorder.
// ffmpeg starts at the first frame drawn. Returns 0, or -1 if already
// recording. (POSIX: SIGPIPE is ignored from then on, so that a failed ffmpeg
// is reported instead of ending the program.)
VKF_API int vkf_viewer_record_start(void* viewer, const char* ffmpeg, int ffmpeg_len, const char* output, int output_len,
                                    double fps)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v || v->recorder.active || fps <= 0.0) return -1;
    return guarded("vkf_viewer_record_start", -1, [&]() {
#if !defined(_WIN32)
        std::signal(SIGPIPE, SIG_IGN);
#endif
        auto& rec = v->recorder;
        rec = Recorder{};
        rec.ffmpeg = to_string(ffmpeg, ffmpeg_len);
        rec.output = to_string(output, output_len);
        rec.fps = fps;
        rec.animationStart = frame_time();
        rec.active = true;
        return 0;
    });
}

// Stop recording (vkf_viewer_record_start): the last frames written, and
// ffmpeg's input closed, so that it finishes the file. Returns the number of
// frames written, or -1 if not recording or on failure (ffmpeg's messages
// are on the standard error).
VKF_API long long vkf_viewer_record_stop(void* viewer)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    return guarded("vkf_viewer_record_stop", -1LL, [&]() { return finish_recording(*v); });
}

// Frames recorded so far (copied: written, or to be written), or -1 if not
// recording or recording failed.
VKF_API long long vkf_viewer_record_frames(void* viewer)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v || !v->recorder.active || v->recorder.failed) return -1;
    return static_cast<long long>(v->recorder.copied);
}

// The trackball's sensitivity: `zoom_speed` scales the zoom of each scroll
// step (wheel or trackpad: VSG zooms 10% per step) and of a right drag (its
// zoomScale); `throw_` (1) keeps the camera moving after a drag released
// while moving (VSG's supportsThrow), 0 stops it at once. Returns 0, or -1.
VKF_API int vkf_viewer_set_camera_controls(void* viewer, double zoom_speed, int throw_)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v || !v->trackball || zoom_speed <= 0.0) return -1;
    v->zoomSpeed = static_cast<float>(zoom_speed);
    v->trackball->zoomScale = zoom_speed;
    v->trackball->supportsThrow = throw_ != 0;
    return 0;
}

// Whether a left button press takes the mouse from the trackball (armed 1:
// a handle is under the cursor; see EventRecorder), until it is released.
// Returns 0, or -1.
VKF_API int vkf_viewer_set_grab(void* viewer, int armed)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    v->grabArmed = armed != 0;
    return 0;
}

// Write the last presented frame to an image file (format from the extension,
// e.g. .png). Call after vkf_viewer_frame. Returns 0 on success.
VKF_API int vkf_viewer_capture(void* viewer, const char* filename, int filename_len)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v || !v->compiled) return -1;
    return guarded("vkf_viewer_capture", -1, [&]() {
        auto device = v->window->getDevice();
        auto swapchain = v->window->getSwapchain();
        const size_t index = v->window->imageIndex();
        if (!device || !swapchain || index >= swapchain->getImageViews().size())
        {
            std::cerr << "vkf_viewer_capture: no image drawn to capture (" << (swapchain ? "image " + std::to_string(index) : "no swapchain")
                      << ")" << std::endl;
            return -1;
        }

        const VkFormat format = swapchain->getImageFormat();
        const bool bgra = (format == VK_FORMAT_B8G8R8A8_UNORM || format == VK_FORMAT_B8G8R8A8_SRGB);
        const bool rgba = (format == VK_FORMAT_R8G8B8A8_UNORM || format == VK_FORMAT_R8G8B8A8_SRGB);
        if (!bgra && !rgba)
        {
            std::cerr << "vkf_viewer_capture: unsupported swapchain format " << format << std::endl;
            return -1;
        }

        auto image = swapchain->getImageViews()[index]->image;
        const VkExtent2D extent = swapchain->getExtent();
        const VkDeviceSize size = VkDeviceSize(extent.width) * extent.height * 4;
        const uint32_t deviceID = device->deviceID;

        v->viewer->deviceWaitIdle();
        auto buffer = vsg::createBufferAndMemory(device, size, VK_BUFFER_USAGE_TRANSFER_DST_BIT, VK_SHARING_MODE_EXCLUSIVE,
                                                 VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT);

        const int queueFamily = v->window->getOrCreatePhysicalDevice()->getQueueFamily(VK_QUEUE_GRAPHICS_BIT);
        auto commandPool = vsg::CommandPool::create(device, queueFamily);
        auto queue = device->getQueue(queueFamily);

        const VkImageSubresourceRange range{VK_IMAGE_ASPECT_COLOR_BIT, 0, 1, 0, 1};
        auto toTransfer = vsg::PipelineBarrier::create(
            VK_PIPELINE_STAGE_TRANSFER_BIT, VK_PIPELINE_STAGE_TRANSFER_BIT, 0,
            vsg::ImageMemoryBarrier::create(VK_ACCESS_MEMORY_READ_BIT, VK_ACCESS_TRANSFER_READ_BIT, VK_IMAGE_LAYOUT_PRESENT_SRC_KHR,
                                            VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, VK_QUEUE_FAMILY_IGNORED, VK_QUEUE_FAMILY_IGNORED, image, range));
        auto toPresent = vsg::PipelineBarrier::create(
            VK_PIPELINE_STAGE_TRANSFER_BIT, VK_PIPELINE_STAGE_TRANSFER_BIT, 0,
            vsg::ImageMemoryBarrier::create(VK_ACCESS_TRANSFER_READ_BIT, VK_ACCESS_MEMORY_READ_BIT, VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL,
                                            VK_IMAGE_LAYOUT_PRESENT_SRC_KHR, VK_QUEUE_FAMILY_IGNORED, VK_QUEUE_FAMILY_IGNORED, image, range));

        const VkResult result = vsg::submitCommandsToQueue(commandPool, queue, [&](vsg::CommandBuffer& commandBuffer) {
            toTransfer->record(commandBuffer);
            VkBufferImageCopy region{};
            region.imageSubresource = {VK_IMAGE_ASPECT_COLOR_BIT, 0, 0, 1};
            region.imageExtent = {extent.width, extent.height, 1};
            vkCmdCopyImageToBuffer(commandBuffer, image->vk(deviceID), VK_IMAGE_LAYOUT_TRANSFER_SRC_OPTIMAL, buffer->vk(deviceID), 1, &region);
            toPresent->record(commandBuffer);
        });
        if (result != VK_SUCCESS) return -1;

        auto pixels = vsg::ubvec4Array2D::create(extent.width, extent.height, vsg::Data::Properties{VK_FORMAT_R8G8B8A8_UNORM});
        void* mapped = nullptr;
        auto memory = buffer->getDeviceMemory(deviceID);
        if (memory->map(buffer->getMemoryOffset(deviceID), size, 0, &mapped) != VK_SUCCESS) return -1;
        std::memcpy(pixels->dataPointer(), mapped, static_cast<size_t>(size));
        memory->unmap();
        for (auto& p : *pixels)
        {
            if (bgra) std::swap(p.r, p.b);
            p.a = 255;
        }

        // (into the file's directory, made if it doesn't exist)
        const auto path = to_string(filename, filename_len);
        std::error_code ec;
        if (const auto dir = std::filesystem::path(path).parent_path(); !dir.empty()) std::filesystem::create_directories(dir, ec);
        auto options = vsg::Options::create(vsgXchange::all::create());
        if (vsg::write(pixels, path, options)) return 0;
        std::cerr << "vkf_viewer_capture: could not write " << path << std::endl;
        return -1;
    });
}

// ---------------------------------------------------------------------------
// Nodes

VKF_API void vkf_node_delete(void* node)
{
    delete static_cast<Node*>(node);
}

// Add child to a group node (a transform or a switch). While running, compile the
// child first (vkf_compile_node).
VKF_API int vkf_node_add_child(void* parent, void* child)
{
    auto p = static_cast<Node*>(parent);
    auto c = static_cast<Node*>(child);
    if (!p || !c) return -1;
    if (auto group = p->node.cast<vsg::Group>())
    {
        group->addChild(c->node);
        return 0;
    }
    if (auto sw = p->node.cast<vsg::Switch>())
    {
        // a new child takes the switch's current state (that of its first child, on if none)
        sw->addChild(sw->children.empty() ? true : sw->children.front().mask != vsg::MASK_OFF, c->node);
        return 0;
    }
    return -1;
}

// Remove child from a group node (a transform or a switch), also while
// running: its GPU data are released a few frames later. Returns -1 if it is
// not a child.
VKF_API int vkf_node_remove_child(void* parent, void* child)
{
    auto p = static_cast<Node*>(parent);
    auto c = static_cast<Node*>(child);
    if (!p || !c) return -1;
    if (auto group = p->node.cast<vsg::Group>())
    {
        auto& children = group->children;
        auto itr = std::find(children.begin(), children.end(), c->node);
        if (itr == children.end()) return -1;
        retire(c->node);
        children.erase(itr);
        return 0;
    }
    if (auto sw = p->node.cast<vsg::Switch>())
    {
        auto& children = sw->children;
        auto itr = std::find_if(children.begin(), children.end(), [&](const vsg::Switch::Child& ch) { return ch.node == c->node; });
        if (itr == children.end()) return -1;
        retire(c->node);
        children.erase(itr);
        return 0;
    }
    return -1;
}

// Compile a node (and its subgraph) for the viewers that are running, so it
// can be added to a scene between frames: for each of them (windows share
// their device, so its GPU objects are made once; each viewer also learns its
// dynamic data, which it uploads). Viewers not yet running compile it with
// their scene at their first frame. Returns 0 on success.
VKF_API int vkf_compile_node(void* node)
{
    auto n = static_cast<Node*>(node);
    if (!n) return -1;
    return guarded("vkf_compile_node", -1, [&]() {
        for (auto* v : live_viewers())
            if (!compile_for(*v, n->node)) return -1;
        return 0;
    });
}

// A viewer that exists (the oldest running one, else the oldest), or null:
// for frames shown while running (Rocky maps are made for a viewer).
VKF_API void* vkf_viewer_any()
{
    auto& viewers = live_viewers();
    for (auto* v : viewers)
        if (v->compiled) return v;
    return viewers.empty() ? nullptr : viewers.front();
}

// A switch node: a group whose children can be hidden and shown together.
VKF_API void* vkf_switch_create()
{
    return guarded("vkf_switch_create", static_cast<void*>(nullptr), [&]() -> void* {
        auto n = std::make_unique<Node>();
        n->node = vsg::Switch::create();
        return n.release();
    });
}

// Show (visible = 1) or hide (0) all children of a switch.
VKF_API int vkf_switch_set(void* node, int visible)
{
    auto n = static_cast<Node*>(node);
    if (!n) return -1;
    auto sw = n->node.cast<vsg::Switch>();
    if (!sw) return -1;
    sw->setAllChildren(visible != 0);
    return 0;
}

// Bounding sphere of a node in its own coordinates (center: 3 doubles).
// Returns 0, or -1 if the node is empty.
VKF_API int vkf_node_bound(void* node, double* center, double* radius)
{
    auto n = static_cast<Node*>(node);
    if (!n) return -1;
    auto computeBounds = vsg::ComputeBounds::create();
    n->node->accept(*computeBounds);
    const auto& box = computeBounds->bounds;
    if (!box.valid()) return -1;
    const vsg::dvec3 c = (box.min + box.max) * 0.5;
    for (int i = 0; i < 3; ++i) center[i] = c[i];
    *radius = vsg::length(box.max - box.min) * 0.5;
    return 0;
}

// A transform node (a group whose children are placed by a 4x4 matrix), initially identity.
// A sphere's level of detail (SphereLod) for a body of `radius` (its largest
// semi-axis) centered on the node's origin; meshes are added with
// vkf_sphere_lod_add.
VKF_API void* vkf_sphere_lod_create(double radius)
{
    return guarded("vkf_sphere_lod_create", static_cast<void*>(nullptr), [&]() -> void* {
        auto lod = SphereLod::create();
        lod->radius = radius;
        auto n = std::make_unique<Node>();
        n->node = lod;
        return n.release();
    });
}

// Add `mesh` (a node handle) of `segments` segments around the equator.
VKF_API int vkf_sphere_lod_add(void* lod, void* mesh, int segments)
{
    auto l = static_cast<Node*>(lod);
    auto m = static_cast<Node*>(mesh);
    if (!l || !m) return -1;
    auto s = l->node.cast<SphereLod>();
    if (!s) return -1;
    auto pos = std::upper_bound(s->segments.begin(), s->segments.end(), segments) - s->segments.begin();
    s->segments.insert(s->segments.begin() + pos, segments);
    s->children.insert(s->children.begin() + pos, m->node);
    return 0;
}

// The most segments any view wanted since the last call (0 if not drawn).
VKF_API int vkf_sphere_lod_requested(void* lod)
{
    auto l = static_cast<Node*>(lod);
    if (!l) return -1;
    auto s = l->node.cast<SphereLod>();
    if (!s) return -1;
    const int r = s->requested;
    s->requested = 0;
    return r;
}

VKF_API void* vkf_transform_create()
{
    return guarded("vkf_transform_create", static_cast<void*>(nullptr), [&]() -> void* {
        auto n = std::make_unique<Node>();
        n->node = vsg::MatrixTransform::create();
        return n.release();
    });
}

VKF_API int vkf_transform_set_matrix(void* transform, const double* m)
{
    auto n = static_cast<Node*>(transform);
    if (!n) return -1;
    auto t = n->node.cast<vsg::MatrixTransform>();
    if (!t) return -1;
    t->matrix = to_dmat4(m);
    return 0;
}

// ---------------------------------------------------------------------------
// Images and models (read through vsgXchange)

// Read an image file (png, jpg, ...); null on failure.
VKF_API void* vkf_image_read(const char* filename, int filename_len)
{
    return guarded("vkf_image_read", static_cast<void*>(nullptr), [&]() -> void* {
        const auto path = to_string(filename, filename_len);
        auto data = vsg::read_cast<vsg::Data>(path, read_options());
        if (!data)
        {
            std::cerr << "vkf_image_read: could not read " << path << std::endl;
            return nullptr;
        }
        data->properties.format = unorm_format(data->properties.format); // its bytes as they are (see "Color")
        auto image = std::make_unique<Image>();
        image->data = data;
        return image.release();
    });
}

VKF_API void vkf_image_delete(void* image)
{
    delete static_cast<Image*>(image);
}

// Read a model file (3ds, obj, gltf, ...); null on failure.
VKF_API void* vkf_model_read(const char* filename, int filename_len)
{
    return guarded("vkf_model_read", static_cast<void*>(nullptr), [&]() -> void* {
        const auto path = to_string(filename, filename_len);
        auto node = vsg::read_cast<vsg::Node>(path, read_options());
        if (!node)
        {
            std::cerr << "vkf_model_read: could not read " << path << std::endl;
            return nullptr;
        }
        vkf::use_cached_shaders(*node);
        auto n = std::make_unique<Node>();
        n->node = node;
        return n.release();
    });
}

// ---------------------------------------------------------------------------
// Meshes: indexed triangles with normals and texture coordinates, lit by VSG's
// Phong shader set

// A mesh of n_vertices vertices and n_indices triangle indices, with a color
// (rgba, multiplying the texture) and an optional texture (image handle, or
// null). Fill its arrays with vkf_mesh_array before the first frame.
// A triangle mesh of n_vertices vertices and n_indices indices, filled from
// Fortran (vkf_mesh_array), of color rgba, with an optional texture and night
// texture (vkf_image_read handles, or null). flags: 1 for an unlit transparent
// surface (TransparentStates; no textures), 2 for view lighting (frames' axes;
// see "Lighting"), else lit by the scene's lights with a material
// (vkf_mesh_material); plus 4 for vertices that change after the mesh is
// compiled (rewritten in place, then vkf_mesh_vertices_changed); plus 8 for a
// lit translucent surface (its material's alpha; TranslucentStates, depth
// sorted with the bound of vkf_mesh_set_bound); plus 16 for a two-sided
// surface (lit: no faces culled, the back lit as the front; e.g. an open
// surface); plus 32 for a color per vertex (vkf_mesh_array 5; rgba is then
// each vertex's initial color).
VKF_API void* vkf_mesh_create(int n_vertices, int n_indices, const float* rgba, void* texture, void* night_texture,
                              int flags)
{
    if (n_vertices < 3 || n_indices < 3) return nullptr;
    return guarded("vkf_mesh_create", static_cast<void*>(nullptr), [&]() -> void* {
        auto mesh = std::make_unique<Mesh>();
        mesh->vertices = vsg::vec3Array::create(static_cast<uint32_t>(n_vertices));
        mesh->normals = vsg::vec3Array::create(static_cast<uint32_t>(n_vertices));
        mesh->texcoords = vsg::vec2Array::create(static_cast<uint32_t>(n_vertices));
        mesh->indices = vsg::uintArray::create(static_cast<uint32_t>(n_indices));
        if ((flags & 4) != 0) mesh->vertices->properties.dataVariance = vsg::DYNAMIC_DATA;
        const bool perVertex = (flags & 32) != 0;
        auto colors = vsg::vec4Array::create(perVertex ? static_cast<uint32_t>(n_vertices) : 1u, to_vec4(rgba));
        mesh->colors = colors;

        const bool unlit = (flags & 1) != 0;
        auto config = vsg::GraphicsPipelineConfigurator::create(
            unlit ? vsg::createFlatShadedShaderSet(read_options()) : vsg::createPhongShaderSet(read_options()));
        // OpenFrames' default material (OpenGL's): no specular reflection
        mesh->material = vsg::PhongMaterialValue::create();
        mesh->material->properties.dataVariance = vsg::DYNAMIC_DATA;
        mesh->material->value().ambient = vsg::vec4(1.0f, 1.0f, 1.0f, 1.0f);
        mesh->material->value().diffuse = vsg::vec4(1.0f, 1.0f, 1.0f, 1.0f);
        mesh->material->value().specular = vsg::vec4(0.0f, 0.0f, 0.0f, 1.0f);
        mesh->material->value().shininess = 0.0f;
        vsg::DataList arrays;
        bool ok = config->assignArray(arrays, "vsg_Vertex", VK_VERTEX_INPUT_RATE_VERTEX, mesh->vertices) &&
                  config->assignArray(arrays, "vsg_Normal", VK_VERTEX_INPUT_RATE_VERTEX, mesh->normals) &&
                  config->assignArray(arrays, "vsg_TexCoord0", VK_VERTEX_INPUT_RATE_VERTEX, mesh->texcoords) &&
                  config->assignArray(arrays, "vsg_Color",
                                      perVertex ? VK_VERTEX_INPUT_RATE_VERTEX : VK_VERTEX_INPUT_RATE_INSTANCE, colors) &&
                  config->assignDescriptor("material", mesh->material);
        if (ok && !unlit)                // a body's twilight (none until vkf_mesh_set_twilight)
        {
            mesh->body = vsg::vec4Value::create(vsg::vec4(0.0f, 0.0f, 0.0f, 0.0f));
            mesh->body->properties.dataVariance = vsg::DYNAMIC_DATA;
            ok = config->assignDescriptor("vkfBody", mesh->body);
        }
        auto sampler = vsg::Sampler::create();
        sampler->addressModeU = VK_SAMPLER_ADDRESS_MODE_REPEAT;
        sampler->addressModeV = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
        if (ok && texture && !unlit) ok = config->assignTexture("diffuseMap", static_cast<Image*>(texture)->data, sampler);
        if (ok && night_texture && !unlit)
            ok = config->assignTexture("nightMap", static_cast<Image*>(night_texture)->data, sampler);
        if (!ok)
        {
            std::cerr << "vkf_mesh_create: the shader set lacks an expected attribute" << std::endl;
            return nullptr;
        }
        if (unlit)
        {
            TransparentStates states;
            config->accept(states);
        }
        else if ((flags & 8) != 0)
        {
            TranslucentStates states;
            states.cullMode = ((flags & 16) != 0) ? VK_CULL_MODE_NONE : VK_CULL_MODE_BACK_BIT;
            config->accept(states);
        }
        else if ((flags & 16) != 0)
        {
            TwoSidedStates states;
            config->accept(states);
        }
        if (!unlit && (flags & 2) != 0)
        {
            if (!config->shaderHints) config->shaderHints = vsg::ShaderCompileSettings::create();
            config->shaderHints->defines.insert("VKF_VIEW_LIGHTING");
        }
        if (!unlit && (flags & 16) != 0)
        {
            if (!config->shaderHints) config->shaderHints = vsg::ShaderCompileSettings::create();
            config->shaderHints->defines.insert("VSG_TWO_SIDED_LIGHTING");
        }
        config->init();
        {
            // meshes with the same states (a frame's axes, spheres...) share one
            // pipeline, else each is created on the first frame (3 to 5 ms each
            // with KosmicKrisp); not their descriptors (copyTo's sharedObjects
            // would merge equal materials: changing one would change all)
            static auto shared = vsg::SharedObjects::create();
            std::scoped_lock<std::recursive_mutex> lock(shared->mutex);
            shared->share(config->layout->setLayouts);
            shared->share(config->layout);
            config->graphicsPipeline->layout = config->layout;
            shared->share(config->graphicsPipeline);
            config->bindGraphicsPipeline->pipeline = config->graphicsPipeline;
            shared->share(config->bindGraphicsPipeline);
        }

        auto stateGroup = vsg::StateGroup::create();
        config->copyTo(stateGroup);
        vkf::use_cached_shaders(*stateGroup);
        auto draw = vsg::VertexIndexDraw::create();
        draw->assignArrays(arrays);
        draw->assignIndices(mesh->indices);
        draw->indexCount = static_cast<uint32_t>(n_indices);
        draw->instanceCount = 1;
        stateGroup->addChild(draw);
        if (!unlit && (flags & 8) != 0)          // drawn after the opaque scene, sorted by depth
            mesh->node = vsg::DepthSorted::create(translucent_bin, vsg::dsphere(0.0, 0.0, 0.0, 1.0), stateGroup);
        else
            mesh->node = stateGroup;
        return mesh.release();
    });
}

// A translucent mesh's bounding sphere (its coordinates), for sorting it by
// depth and culling it. Returns 0, or -1 if it isn't a translucent mesh.
VKF_API int vkf_mesh_set_bound(void* mesh, double x, double y, double z, double radius)
{
    auto m = dynamic_cast<Mesh*>(static_cast<Node*>(mesh));
    if (!m) return -1;
    auto sorted = m->node.cast<vsg::DepthSorted>();
    if (!sorted) return -1;
    sorted->bound = vsg::dsphere(x, y, z, radius);
    return 0;
}

// The material of a lit mesh: 19 floats (vsg::PhongMaterial: ambient, diffuse,
// specular, emissive rgba, shininess, alpha mask, alpha mask cutoff); call
// vkf_mesh_material_changed after writing them.
VKF_API float* vkf_mesh_material(void* mesh)
{
    auto m = dynamic_cast<Mesh*>(static_cast<Node*>(mesh));
    if (!m || !m->material) return nullptr;
    return &(m->material->value().ambient.x);
}

VKF_API int vkf_mesh_material_changed(void* mesh)
{
    auto m = dynamic_cast<Mesh*>(static_cast<Node*>(mesh));
    if (!m || !m->material) return -1;
    m->material->dirty();
    return 0;
}

// A lit mesh's twilight: its diffuse light extended to `angle` (radians)
// past the terminator, fading out (0: none; shaders/vsg/standard_phong.frag).
VKF_API int vkf_mesh_set_twilight(void* mesh, float angle)
{
    auto m = dynamic_cast<Mesh*>(static_cast<Node*>(mesh));
    if (!m || !m->body) return -1;
    m->body->value().x = std::sin(std::clamp(angle, 0.0f, 1.5f));
    m->body->dirty();
    return 0;
}

// Upload a mesh's vertices again after they were rewritten in place (a mesh
// created with flag 4).
VKF_API int vkf_mesh_vertices_changed(void* mesh)
{
    auto m = dynamic_cast<Mesh*>(static_cast<Node*>(mesh));
    if (!m) return -1;
    m->vertices->dirty();
    return 0;
}

// One of the mesh's arrays, to fill in place: 1 = vertices (3 floats each),
// 2 = normals (3 floats), 3 = texture coordinates (2 floats), 4 = triangle
// indices (uint32, 0-based), 5 = colors (4 floats each: one per vertex with
// flag 32, else one). Valid while the mesh handle exists.
VKF_API void* vkf_mesh_array(void* mesh, int which)
{
    auto m = dynamic_cast<Mesh*>(static_cast<Node*>(mesh));
    if (!m) return nullptr;
    switch (which)
    {
    case 1: return m->vertices->dataPointer();
    case 2: return m->normals->dataPointer();
    case 3: return m->texcoords->dataPointer();
    case 4: return m->indices->dataPointer();
    case 5: return m->colors->dataPointer();
    default: return nullptr;
    }
}

// ---------------------------------------------------------------------------
// Labels: text that always faces the screen (shaders/label.vert)

// Read a font file (TrueType, ... through vsgXchange's freetype); null on
// failure. Converting a font (its glyphs' signed distance fields) takes
// seconds, so the result is cached (cache_dir) and read from there next time.
VKF_API void* vkf_font_read(const char* filename, int filename_len)
{
    return guarded("vkf_font_read", static_cast<void*>(nullptr), [&]() -> void* {
        const auto path = to_string(filename, filename_len);
        const auto cached = vkf::font_cache_file(std::filesystem::path(path));
        vsg::ref_ptr<vsg::Font> font;
        if (!cached.empty() && std::filesystem::exists(cached))
            font = vsg::read_cast<vsg::Font>(cached.string(), read_options());
        if (!font)
        {
            font = vsg::read_cast<vsg::Font>(path, read_options());
            if (!font)
            {
                std::cerr << "vkf_font_read: could not read " << path << std::endl;
                return nullptr;
            }
            if (!cached.empty()) vkf::write_font_cache(font, cached, read_options());
        }
        auto f = std::make_unique<Font>();
        f->font = font;
        return f.release();
    });
}

VKF_API void vkf_font_delete(void* font)
{
    delete static_cast<Font*>(font);
}

// A label: `text` at `position` (3 doubles, in its parent's coordinates) in
// color `rgba`, facing the screen. size > 0: `size` window units tall (a fixed
// screen size); size < 0: -size scene units tall, but at most `max_size` window
// units on screen. halign, valign: vsg::StandardLayout's alignments (0
// baseline, 1 left/top, 2 center, 3 right/bottom).
VKF_API void* vkf_label_create(void* font, const char* text, int text_len, const float* rgba, const double* position,
                               float size, float max_size, int halign, int valign)
{
    auto f = static_cast<Font*>(font);
    if (!f) return nullptr;
    return guarded("vkf_label_create", static_cast<void*>(nullptr), [&]() -> void* {
        auto layout = vsg::StandardLayout::create();
        layout->billboard = true;
        layout->billboardAutoScaleDistance = size;       // read by label.vert as the size mode
        layout->outlineWidth = max_size;                 // read by label.vert as the maximum size
        layout->position = vsg::vec3(static_cast<float>(position[0]), static_cast<float>(position[1]), static_cast<float>(position[2]));
        layout->horizontal = vsg::vec3(1.0f, 0.0f, 0.0f);  // character size 1: scaled by label.vert
        layout->vertical = vsg::vec3(0.0f, 1.0f, 0.0f);
        layout->color = to_vec4(rgba);
        layout->horizontalAlignment = static_cast<vsg::StandardLayout::Alignment>(halign);
        layout->verticalAlignment = static_cast<vsg::StandardLayout::Alignment>(valign);

        auto label = vsg::Text::create();
        label->technique = vsg::GpuLayoutTechnique::create();
        label->font = f->font;
        label->layout = layout;
        label->text = vsg::stringValue::create(to_string(text, text_len));
        label->shaderSet = label_shaders().shaderSet;
        // glyphs allocated: room to change the text in place (a multiple of 4: glyph indices are uvec4s)
        const uint32_t capacity = 4 * ((std::max<uint32_t>(256, 2 * static_cast<uint32_t>(std::max(text_len, 0))) + 3) / 4);
        label->setup(capacity, read_options());   // room to change the text in place

        auto group = LabelGroup::create();
        group->addChild(label);
        auto n = std::make_unique<Label>();
        n->node = group;
        n->text = label;
        n->layout = layout;
        n->capacity = capacity;
        return n.release();
    });
}

// Change a label's text, in place: a text longer than the label's capacity
// (glyphs allocated when it was made) is cut to it (a longer one would not be
// drawn), with a warning.
VKF_API int vkf_label_set_text(void* label, const char* text, int text_len)
{
    auto l = dynamic_cast<Label*>(static_cast<Node*>(label));
    if (!l) return -1;
    return guarded("vkf_label_set_text", -1, [&]() {
        auto str = to_string(text, text_len);
        if (str.size() > l->capacity)                    // (bytes: at least as many as glyphs)
        {
            auto n = static_cast<size_t>(l->capacity);
            while (n > 0 && (static_cast<unsigned char>(str[n]) & 0xC0) == 0x80) --n;   // (a UTF-8 character's start)
            str.resize(n);
            if (!l->truncated) std::cerr << "vkframes: label text cut to " << l->capacity << " characters" << std::endl;
            l->truncated = true;
        }
        l->text->text = vsg::stringValue::create(str);
        l->text->setup(l->capacity, read_options());
        return 0;
    });
}

// Change a label's color (rgba).
VKF_API int vkf_label_set_color(void* label, const float* rgba)
{
    auto l = dynamic_cast<Label*>(static_cast<Node*>(label));
    if (!l) return -1;
    return guarded("vkf_label_set_color", -1, [&]() {
        l->layout->color = to_vec4(rgba);
        l->text->setup(l->capacity, read_options());
        return 0;
    });
}

// Change a label's layout, in place: its position (3 doubles), size and
// maximum size (as vkf_label_create's) and alignments.
VKF_API int vkf_label_set_layout(void* label, const double* position, float size, float max_size, int halign, int valign)
{
    auto l = dynamic_cast<Label*>(static_cast<Node*>(label));
    if (!l) return -1;
    return guarded("vkf_label_set_layout", -1, [&]() {
        l->layout->position = vsg::vec3(static_cast<float>(position[0]), static_cast<float>(position[1]), static_cast<float>(position[2]));
        l->layout->billboardAutoScaleDistance = size;
        l->layout->outlineWidth = max_size;
        l->layout->horizontalAlignment = static_cast<vsg::StandardLayout::Alignment>(halign);
        l->layout->verticalAlignment = static_cast<vsg::StandardLayout::Alignment>(valign);
        l->text->setup(l->capacity, read_options());
        return 0;
    });
}

// Draw a label on top of the scene (on != 0: at the near plane, as HUD
// labels, not hidden by what is in front of it) or depth-tested (0).
VKF_API int vkf_label_set_on_top(void* label, int on)
{
    auto l = dynamic_cast<Label*>(static_cast<Node*>(label));
    if (!l) return -1;
    auto group = l->node.cast<LabelGroup>();
    if (!group) return -1;
    group->hud = on != 0;
    return 0;
}

// Draw a label at infinity (on != 0: its position a direction, in its
// parent's axes; at the far plane, behind the scene, as the sky's stars) or at
// its position (0).
VKF_API int vkf_label_set_at_infinity(void* label, int on)
{
    auto l = dynamic_cast<Label*>(static_cast<Node*>(label));
    if (!l) return -1;
    auto group = l->node.cast<LabelGroup>();
    if (!group) return -1;
    group->atInfinity = on != 0;
    return 0;
}

// Show a label in the viewer's HUD (at x, y from 0 to 1 across the viewport,
// bottom to top: the label's position), on top of the scene. Also while running.
VKF_API int vkf_viewer_hud_add(void* viewer, void* label)
{
    auto v = static_cast<Viewer*>(viewer);
    auto l = dynamic_cast<Label*>(static_cast<Node*>(label));
    if (!v || !l) return -1;
    if (auto group = l->node.cast<LabelGroup>()) group->hud = true;
    return guarded("vkf_viewer_hud_add", -1, [&]() {
        if (!compile_for(*v, l->node)) return -1;
        v->hudScene->addChild(l->node);
        return 0;
    });
}

// Remove a label from the viewer's HUD (its GPU data are released a few frames later).
VKF_API int vkf_viewer_hud_remove(void* viewer, void* label)
{
    auto v = static_cast<Viewer*>(viewer);
    auto n = static_cast<Node*>(label);
    if (!v || !n) return -1;
    return remove_from(v->hudScene->children, n->node) ? 0 : -1;
}

// ---------------------------------------------------------------------------
// Line strips: thick lines of constant pixel width (shaders/line.vert), with
// points kept to double precision (drawn relative to the eye)

// A line of up to capacity points (see vkf_line_create), with a color per
// point when `colored` (vkf_line_colors): blended along each segment and
// multiplied by the style's color.
static void* make_line(int capacity, bool colored, bool ribbon = false)
{
    if (capacity < 2) return nullptr;
    return guarded(ribbon ? "vkf_ribbon_create" : "vkf_line_create", static_cast<void*>(nullptr), [&]() -> void* {
        auto& pipeline = ribbon ? ribbon_pipeline() : line_pipeline();
        auto line = std::make_unique<LineStrip>();
        line->ribbon = ribbon;
        line->points = vsg::floatArray::create(8 * static_cast<uint32_t>(capacity), 0.0f);
        line->points->properties.dataVariance = vsg::DYNAMIC_DATA;
        line->style = dynamic_vec4_array(11);    // [color, params, (points written, colored), highlight, trail: shaders/line.vert]
        line->colored = colored;
        line->colors = vsg::vec4Array::create(colored ? static_cast<uint32_t>(capacity) : 1u, vsg::vec4(1.0f, 1.0f, 1.0f, 1.0f));
        if (colored) line->colors->properties.dataVariance = vsg::DYNAMIC_DATA;
        line->style->at(2).y = colored ? 1.0f : 0.0f;

        auto descriptorSet = vsg::DescriptorSet::create(pipeline.descriptorSetLayout, vsg::Descriptors{
            vsg::DescriptorBuffer::create(line->points, 0, 0, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
            vsg::DescriptorBuffer::create(line->style, 1, 0, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
            vsg::DescriptorBuffer::create(line->colors, 3, 0, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
        });

        auto stateGroup = vsg::StateGroup::create();
        stateGroup->add(pipeline.bindPipeline);
        stateGroup->add(vsg::BindDescriptorSet::create(VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline.pipelineLayout, 0, descriptorSet));
        line->draw = vsg::Draw::create(0, 1, 0, 0);
        stateGroup->addChild(line->draw);
        auto eyeRelative = EyeRelative::create(pipeline.pipelineLayout);
        if (!ribbon && pipeline.bindPlainPipeline && pipeline.bindDashedPipeline)
        {
            // the leaner variants (plain, dashed) before the full one: all compiled, one drawn (line_variant)
            for (auto& bind : {pipeline.bindPlainPipeline, pipeline.bindDashedPipeline})
            {
                auto variantGroup = vsg::StateGroup::create();
                variantGroup->add(bind);
                variantGroup->add(vsg::BindDescriptorSet::create(VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline.pipelineLayout, 0, descriptorSet));
                variantGroup->addChild(line->draw);
                eyeRelative->addChild(variantGroup);
            }
            eyeRelative->variants = true;
        }
        line->lod = std::make_shared<LineLod>();
        line->lod->draw = line->draw;
        line->lod->style = line->style;
        line->lod->points = line->points;
        eyeRelative->lod = line->lod;
        eyeRelative->linePipeline = !ribbon;
        eyeRelative->addChild(stateGroup);
        if (ribbon)
        {
            line->node = eyeRelative;
            return line.release();
        }
        // a highlight's glow and arrows: a second pass of the same draw (the
        // glow pipeline: writing no depth), after the opaque scene, sorted with
        // the translucent surfaces (else, its faint glow writing depth hid a
        // translucent surface drawn after it: a dark band about the line);
        // the first pass then draws the line only. Shown while highlighted
        // with arrows (vkf_line_set_highlight).
        auto glowState = vsg::StateGroup::create();
        glowState->add(pipeline.bindGlowPipeline);
        glowState->add(vsg::BindDescriptorSet::create(VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline.pipelineLayout, 0, descriptorSet));
        glowState->addChild(line->draw);
        auto glowRelative = EyeRelative::create(pipeline.pipelineLayout);
        glowRelative->lod = line->lod;
        glowRelative->linePipeline = true;
        glowRelative->pass = 1;
        glowRelative->addChild(glowState);
        line->glowSorted = vsg::DepthSorted::create(translucent_bin, vsg::dsphere(0.0, 0.0, 0.0, 1.0e30), glowRelative);
        line->glow = vsg::Switch::create();
        line->glow->addChild(false, line->glowSorted);
        auto group = vsg::Group::create();
        group->addChild(eyeRelative);
        group->addChild(line->glow);
        line->node = group;
        return line.release();
    });
}

// A line strip holding up to capacity points (initially none). Fortran fills
// its point and style buffers in place (vkf_line_points, vkf_line_style) and
// then calls vkf_line_points_changed / vkf_line_style_changed. It can be shown
// in any number of windows.
VKF_API void* vkf_line_create(int capacity) { return make_line(capacity, false); }

// A line with a color per point (r, g, b, a, in display values), blended along
// each segment (vkf_line_colors; uploaded with the points by
// vkf_line_points_changed).
VKF_API void* vkf_line_create_colored(int capacity) { return make_line(capacity, true); }

// A ribbon of up to capacity points (even; initially none): a translucent
// surface through pairs of points (2i-1, 2i), a quad between each pair and the
// next (shaders/ribbon.vert), in the style's color, times each point's color
// if `colored` (vkf_line_colors). The buffers are a line's (vkf_line_points,
// vkf_line_style, vkf_line_points_changed, vkf_line_set_range with whole
// pairs).
VKF_API void* vkf_ribbon_create(int capacity, int colored) { return make_line(capacity, colored != 0, true); }

// The colors of a line made by vkf_line_create_colored: 4 floats per point,
// to fill in place (null for other lines).
VKF_API float* vkf_line_colors(void* line, int* capacity)
{
    auto l = dynamic_cast<LineStrip*>(static_cast<Node*>(line));
    if (!l || !l->colored) return nullptr;
    *capacity = static_cast<int>(l->colors->size());
    return &(l->colors->at(0).x);
}

// Markers at up to capacity points (initially none): a screen-space quad per
// point, of the shape and size in the style buffer (shaders/marker.vert), with
// image (a vkf_image_read handle, or null) for the image shape; `colored`: a
// color per marker (vkf_line_colors), times the style's. The point and style
// buffers are those of a line (vkf_line_points, vkf_line_style,
// vkf_line_points_changed, vkf_line_set_range, vkf_line_style_changed).
VKF_API void* vkf_marker_create(int capacity, void* image, int colored)
{
    if (capacity < 1) return nullptr;
    return guarded("vkf_marker_create", static_cast<void*>(nullptr), [&]() -> void* {
        auto& pipeline = marker_pipeline();
        auto marker = std::make_unique<LineStrip>();
        marker->markers = true;
        marker->points = vsg::floatArray::create(8 * static_cast<uint32_t>(capacity), 0.0f);
        marker->points->properties.dataVariance = vsg::DYNAMIC_DATA;
        marker->style = dynamic_vec4_array(2);
        marker->colored = colored != 0;
        marker->colors = vsg::vec4Array::create(marker->colored ? static_cast<uint32_t>(capacity) : 1u, vsg::vec4(1.0f, 1.0f, 1.0f, 1.0f));
        if (marker->colored) marker->colors->properties.dataVariance = vsg::DYNAMIC_DATA;
        marker->style->at(1).w = marker->colored ? 1.0f : 0.0f;    // (shaders/marker.vert)

        vsg::ref_ptr<vsg::Data> data;
        if (image) data = static_cast<Image*>(image)->data;
        else data = vsg::ubvec4Array2D::create(1, 1, vsg::ubvec4(255, 255, 255, 255), vsg::Data::Properties{VK_FORMAT_R8G8B8A8_UNORM});
        auto sampler = vsg::Sampler::create();
        sampler->addressModeU = sampler->addressModeV = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;

        auto descriptorSet = vsg::DescriptorSet::create(pipeline.descriptorSetLayout, vsg::Descriptors{
            vsg::DescriptorBuffer::create(marker->points, 0, 0, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
            vsg::DescriptorBuffer::create(marker->style, 1, 0, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
            vsg::DescriptorImage::create(sampler, data, 2, 0, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
            vsg::DescriptorBuffer::create(marker->colors, 3, 0, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
        });

        auto stateGroup = vsg::StateGroup::create();
        stateGroup->add(pipeline.bindPipeline);
        stateGroup->add(vsg::BindDescriptorSet::create(VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline.pipelineLayout, 0, descriptorSet));
        marker->draw = vsg::Draw::create(0, 1, 0, 0);
        stateGroup->addChild(marker->draw);
        auto eyeRelative = EyeRelative::create(pipeline.pipelineLayout);
        eyeRelative->addChild(stateGroup);
        marker->node = eyeRelative;
        return marker.release();
    });
}

// ---------------------------------------------------------------------------
// Lights (see "Lighting" above)

// A light, initially a directional light along -z (in its node's
// coordinates) of white diffuse color and no ambient or specular part.
VKF_API void* vkf_light_create()
{
    return guarded("vkf_light_create", static_cast<void*>(nullptr), [&]() -> void* {
        auto l = std::make_unique<LightNode>();
        l->ambient = vsg::AmbientLight::create();
        l->directional = vsg::DirectionalLight::create();
        l->directionalSpecular = vsg::DirectionalLight::create();
        l->point = vsg::PointLight::create();
        l->pointSpecular = vsg::PointLight::create();
        auto group = vsg::Group::create();
        group->addChild(l->ambient);
        group->addChild(l->directional);
        group->addChild(l->directionalSpecular);
        group->addChild(l->point);
        group->addChild(l->pointSpecular);
        l->node = group;
        const float none[3] = {0.0f, 0.0f, 0.0f}, white[3] = {1.0f, 1.0f, 1.0f};
        const double down[4] = {0.0, 0.0, 1.0, 0.0};
        vkf_light_set(l.get(), none, white, none, down);
        return l.release();
    });
}

// Set a light's colors (rgb, 0 to 1) and position (OpenGL's: x, y, z, w; w = 0
// for a directional light from direction x, y, z, else a point light at
// x/w, y/w, z/w), in its node's coordinates. Returns 0.
VKF_API int vkf_light_set(void* light, const float* ambient, const float* diffuse, const float* specular,
                          const double* position)
{
    auto l = dynamic_cast<LightNode*>(static_cast<Node*>(light));
    if (!l) return -1;
    const bool point = position[3] != 0.0;
    const bool hasSpecular = specular[0] > 0.0f || specular[1] > 0.0f || specular[2] > 0.0f;
    l->ambient->color = vsg::vec3(ambient[0], ambient[1], ambient[2]);
    l->ambient->intensity = 1.0f;
    for (auto* d : {l->directional.get(), l->directionalSpecular.get()})
        d->direction = vsg::normalize(-vsg::dvec3(position[0], position[1], position[2]));
    for (auto* p : {l->point.get(), l->pointSpecular.get()})
        p->position = point ? vsg::dvec3(position[0], position[1], position[2]) / position[3] : vsg::dvec3();
    l->directional->color = l->point->color = vsg::vec3(diffuse[0], diffuse[1], diffuse[2]);
    // the specular parts: negative colors (read as specular only); unused lights have intensity 0 (dropped)
    l->directionalSpecular->color = l->pointSpecular->color = -vsg::vec3(specular[0], specular[1], specular[2]);
    l->directional->intensity = point ? 0.0f : 1.0f;
    l->point->intensity = point ? 1.0f : 0.0f;
    l->directionalSpecular->intensity = (!point && hasSpecular) ? 1.0f : 0.0f;
    l->pointSpecular->intensity = (point && hasSpecular) ? 1.0f : 0.0f;
    return 0;
}

// The viewer's default light (a vkf_light_create light in eye coordinates,
// owned by the viewer), and whether it is on.
VKF_API void* vkf_viewer_light(void* viewer)
{
    auto v = static_cast<Viewer*>(viewer);
    return v ? v->headlightHandle : nullptr;
}

VKF_API int vkf_viewer_light_enable(void* viewer, int enabled)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    v->headlight->setAllChildren(enabled != 0);
    return 0;
}

// ---------------------------------------------------------------------------
// Sky: stars and texture at infinity, drawn behind the scene

// Stars (up to capacity; initially none): Fortran fills the buffer in place
// (vkf_sky_stars: per star, direction x, y, z, size in window units, then r,
// g, b, -) and calls vkf_sky_stars_changed.
VKF_API void* vkf_stars_create(int capacity)
{
    if (capacity < 1) return nullptr;
    return guarded("vkf_stars_create", static_cast<void*>(nullptr), [&]() -> void* {
        auto& pipeline = stars_pipeline();
        auto sky = std::make_unique<SkyNode>();
        sky->stars = vsg::floatArray::create(8 * static_cast<uint32_t>(capacity), 0.0f);
        sky->stars->properties.dataVariance = vsg::DYNAMIC_DATA;
        sky->style = sky_style();
        auto descriptorSet = vsg::DescriptorSet::create(pipeline.descriptorSetLayout, vsg::Descriptors{
            vsg::DescriptorBuffer::create(sky->stars, 0, 0, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
            vsg::DescriptorBuffer::create(sky->style, 1, 0, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
        });
        auto stateGroup = vsg::StateGroup::create();
        stateGroup->add(pipeline.bindPipeline);
        stateGroup->add(vsg::BindDescriptorSet::create(VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline.pipelineLayout, 0, descriptorSet));
        sky->draw = vsg::Draw::create(0, 1, 0, 0);
        stateGroup->addChild(sky->draw);
        auto eyeRelative = EyeRelative::create(pipeline.pipelineLayout); // the view's rotation only
        eyeRelative->addChild(stateGroup);
        sky->node = eyeRelative;
        return sky.release();
    });
}

// The sky texture: image (a vkf_image_read handle; equirectangular, as a
// Sphere's texture) on the sky seen from inside.
VKF_API void* vkf_sky_texture_create(void* image)
{
    auto img = static_cast<Image*>(image);
    if (!img) return nullptr;
    return guarded("vkf_sky_texture_create", static_cast<void*>(nullptr), [&]() -> void* {
        auto& pipeline = sky_pipeline();
        auto sky = std::make_unique<SkyNode>();
        sky->style = sky_style();
        auto sampler = vsg::Sampler::create();
        sampler->addressModeV = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
        auto descriptorSet = vsg::DescriptorSet::create(pipeline.descriptorSetLayout, vsg::Descriptors{
            vsg::DescriptorBuffer::create(sky->style, 1, 0, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
            vsg::DescriptorImage::create(sampler, img->data, 2, 0, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
        });
        auto stateGroup = vsg::StateGroup::create();
        stateGroup->add(pipeline.bindPipeline);
        stateGroup->add(vsg::BindDescriptorSet::create(VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline.pipelineLayout, 0, descriptorSet));
        sky->draw = vsg::Draw::create(3, 1, 0, 0); // one triangle covering the viewport
        stateGroup->addChild(sky->draw);
        auto eyeRelative = EyeRelative::create(pipeline.pipelineLayout);
        eyeRelative->addChild(stateGroup);
        sky->node = eyeRelative;
        return sky.release();
    });
}

// The stars' buffer (8 floats per star) and its capacity in stars; null for a
// sky texture.
VKF_API float* vkf_sky_stars(void* node, int* capacity)
{
    auto sky = dynamic_cast<SkyNode*>(static_cast<Node*>(node));
    if (!sky || !sky->stars) return nullptr;
    *capacity = static_cast<int>(sky->stars->size() / 8);
    return sky->stars->data();
}

// The first n stars were written: draw them, and upload the buffer.
VKF_API int vkf_sky_stars_changed(void* node, int n)
{
    auto sky = dynamic_cast<SkyNode*>(static_cast<Node*>(node));
    if (!sky || !sky->stars || n < 0 || 8 * static_cast<uint32_t>(n) > sky->stars->size()) return -1;
    sky->stars->dirty();
    sky->draw->vertexCount = 6 * static_cast<uint32_t>(n);
    return 0;
}

// The sky's style: 20 floats, the attitude (a column-major 4x4 matrix, sky
// frame to root frame), then 4 parameters (unused); call
// vkf_sky_style_changed after writing them.
VKF_API float* vkf_sky_style(void* node)
{
    auto sky = dynamic_cast<SkyNode*>(static_cast<Node*>(node));
    if (!sky) return nullptr;
    return &(sky->style->at(0).x);
}

VKF_API int vkf_sky_style_changed(void* node)
{
    auto sky = dynamic_cast<SkyNode*>(static_cast<Node*>(node));
    if (!sky) return -1;
    sky->style->dirty();
    return 0;
}

// The strip's point buffer, 8 floats per point (x, y, z and the distance along
// the strip from its start, high parts, then their low parts; see
// shaders/line.vert; markers ignore the distance), and its capacity in
// points. Valid while the line handle exists.
VKF_API float* vkf_line_points(void* line, int* capacity)
{
    auto l = dynamic_cast<LineStrip*>(static_cast<Node*>(line));
    if (!l) return nullptr;
    *capacity = static_cast<int>(l->points->size() / 8);
    return l->points->data();
}

// The strip's style buffer: 8 floats (r, g, b, a, width in window units,
// 1 for separate segments (pairs of points) or 0 for a strip, the dash
// pattern (16 bits; 65535 or 0: solid), window units per bit of it). Set the
// segment flag before drawing points (it changes how they are counted).
VKF_API float* vkf_line_style(void* line)
{
    auto l = dynamic_cast<LineStrip*>(static_cast<Node*>(line));
    if (!l) return nullptr;
    return &(l->style->at(0).x);
}

// The first n points of the buffer have been written: draw n points and upload
// the buffer. Returns 0, or -1 if n exceeds the capacity.
VKF_API int vkf_line_points_changed(void* line, int n)
{
    auto l = dynamic_cast<LineStrip*>(static_cast<Node*>(line));
    if (!l || n < 0 || 8 * static_cast<uint32_t>(n) > l->points->size()) return -1;
    l->points->dirty();
    if (l->colored) l->colors->dirty();
    l->setRange(1, static_cast<uint32_t>(n));
    if (!l->markers && l->style->size() > 2)       // lines: the points written (neighbours for the joins)
    {
        l->style->at(2).x = static_cast<float>(n);
        l->style->dirty();
    }
    return 0;
}

// A line strip's level of detail (see EyeRelative): the bounding sphere of
// its points written (center, radius), and error[k], k = 0 .. levels-1: how far
// the points skipped can be from the segments drawn at stride 2^(k+1), in the
// points' units. levels = 0: all points drawn.
VKF_API int vkf_line_set_lod(void* line, const double* center, double radius, const float* error, int levels)
{
    auto l = dynamic_cast<LineStrip*>(static_cast<Node*>(line));
    if (!l || !l->lod || levels < 0 || levels > 16) return -1;
    l->lod->center = vsg::dvec3(center[0], center[1], center[2]);
    l->lod->radius = radius;
    for (int k = 0; k < levels; ++k) l->lod->error[k] = error[k];
    l->lod->levels = levels;
    l->lod->hasBound = true;
    if (l->glowSorted) l->glowSorted->bound = vsg::dsphere(center[0], center[1], center[2], radius);
    return 0;
}

// Draw points first .. first+n-1 only (1-based; no data is uploaded): e.g.
// for trace mode. (Separate segments: first odd, n even.)
VKF_API int vkf_line_set_range(void* line, int first, int n)
{
    auto l = dynamic_cast<LineStrip*>(static_cast<Node*>(line));
    if (!l || first < 1 || n < 0 || 8 * static_cast<uint32_t>(first - 1 + n) > l->points->size()) return -1;
    l->setRange(static_cast<uint32_t>(first), static_cast<uint32_t>(n));
    return 0;
}

// A line's highlight (shaders/line.vert): 12 floats, its color (r, g, b, a),
// (flags: 1 pulse, 2 arrows; arrow spacing and band width in window units,
// arrow speed in window units per second; as in that order: flags, spacing,
// speed, band width), (pulse period in seconds, 0, 0, 0). Uploaded at once.
// Returns 0, or -1 if `line` isn't a line (or a ribbon).
VKF_API int vkf_line_set_highlight(void* line, const float* values)
{
    auto l = dynamic_cast<LineStrip*>(static_cast<Node*>(line));
    if (!l || l->markers || l->style->size() < 6) return -1;
    for (int k = 0; k < 3; ++k)
        l->style->at(3 + k) = vsg::vec4(values[4 * k], values[4 * k + 1], values[4 * k + 2], values[4 * k + 3]);
    l->style->dirty();
    if (l->glow) l->glow->setAllChildren((static_cast<int>(values[4] + 0.5f) & 2) != 0);   // (arrows: the glow's pass)
    return 0;
}

// A line's trail (shaders/line.vert): 20 floats, (the distance along it at
// the current time as a high/low pair, the direction (1: distance increasing
// with time, -1 decreasing; 0: no trail), the color flags (1: the past's
// color replaces the line's, 2: the future's)), (the past's cutoff and its
// fade's start, the future's fade's start and its cutoff: distances from the
// current time's, in that direction), the past's color (rgba; its alpha
// multiplies the line's if not replacing), the future's, (the past's dash
// pattern and factor, the future's). Uploaded at once. Returns 0, or -1 if
// `line` isn't a line strip.
VKF_API int vkf_line_set_trail(void* line, const float* values)
{
    auto l = dynamic_cast<LineStrip*>(static_cast<Node*>(line));
    if (!l || l->markers || l->ribbon || l->style->size() < 11) return -1;
    for (int k = 0; k < 5; ++k)
        l->style->at(6 + k) = vsg::vec4(values[4 * k], values[4 * k + 1], values[4 * k + 2], values[4 * k + 3]);
    l->style->dirty();
    return 0;
}

// A line's dashes: 0 anchored to the line (the default: sizes in scene units,
// see dash_levels), 1 screen-locked (a constant length in pixels, following
// the line on screen, as OpenGL's line stipple: see ScreenArcs), 2 a fixed
// length of the line (see fixed_dashes; strips): `length` scene units for the
// pattern, or if 0 its length on screen when first drawn. 1 and 2 cost a
// projection of the points drawn per view and frame. Returns 0, or -1 if
// `line` isn't a line.
VKF_API int vkf_line_set_dash_mode(void* line, int mode, double length)
{
    auto l = dynamic_cast<LineStrip*>(static_cast<Node*>(line));
    if (!l || l->markers || l->ribbon || !l->lod) return -1;
    l->lod->screenDashes = mode == 1;
    if (mode != 2 || length != l->lod->dashLength) l->lod->fixedBase.clear();
    l->lod->fixedDashes = mode == 2;
    l->lod->dashLength = (mode == 2) ? std::max(length, 0.0) : 0.0;
    l->style->at(2).z = mode == 1 ? 1.0f : 0.0f;
    l->style->dirty();
    return 0;
}

VKF_API int vkf_line_style_changed(void* line)
{
    auto l = dynamic_cast<LineStrip*>(static_cast<Node*>(line));
    if (!l) return -1;
    l->style->dirty();
    return 0;
}

} // extern "C"

// For the optional parts of the layer in other files (vkf_rocky.cpp): the VSG
// viewer of a viewer handle, and node handles.
namespace vkf
{
    vsg::ref_ptr<vsg::Viewer> vsg_viewer(void* viewer)
    {
        auto v = static_cast<Viewer*>(viewer);
        return v ? v->viewer : vsg::ref_ptr<vsg::Viewer>();
    }

    void* wrap_node(vsg::ref_ptr<vsg::Node> node)
    {
        auto n = std::make_unique<Node>();
        n->node = node;
        return n.release();
    }

    vsg::ref_ptr<vsg::Node> unwrap_node(void* node)
    {
        auto n = static_cast<Node*>(node);
        return n ? n->node : vsg::ref_ptr<vsg::Node>();
    }
} // namespace vkf

#endif // VKF_EXTERNAL_LAYER
