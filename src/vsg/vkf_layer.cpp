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

#if defined(_WIN32)
#    define VKF_API __declspec(dllexport)
#    include <windows.h>
#else
#    define VKF_API
#endif

#include "vkf_shaders.h"

#include <vsg/all.h>
#include <vsgXchange/all.h>

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstring>
#include <deque>
#include <iostream>
#include <map>
#include <memory>
#include <string>
#include <typeinfo>

#if defined(__APPLE__)
#    include <CoreGraphics/CGGeometry.h>
#    include <cxxabi.h>
#    include <objc/message.h>
#    include <objc/runtime.h>
#endif

namespace
{
    std::string to_string(const char* s, int len)
    {
        return (s && len > 0) ? std::string(s, static_cast<size_t>(len)) : std::string();
    }

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

#if defined(__APPLE__)
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
    // QNSView has a container layer), where MoltenVK finds no CAMetalLayer; a
    // plain NSView takes VSG's CAMetalLayer as its layer. Mouse and keys go to
    // `parent` (the host). Objective-C runtime calls, so this stays plain C++.
    void* create_child_view(void* parent)
    {
        auto send = [](auto f, auto... args) { return reinterpret_cast<decltype(f)>(objc_msgSend)(args...); };
        using IdSel = id (*)(id, SEL);
        id p = static_cast<id>(parent);
#    if defined(__x86_64__)
        CGRect bounds;
        reinterpret_cast<void (*)(CGRect*, id, SEL)>(objc_msgSend_stret)(&bounds, p, sel_registerName("bounds"));
#    else
        CGRect bounds = send(static_cast<CGRect (*)(id, SEL)>(nullptr), p, sel_registerName("bounds"));
#    endif
        id view = send(static_cast<IdSel>(nullptr), reinterpret_cast<id>(render_view_class()), sel_registerName("alloc"));
        view = send(static_cast<id (*)(id, SEL, CGRect)>(nullptr), view, sel_registerName("initWithFrame:"), bounds);
        // NSViewWidthSizable | NSViewHeightSizable
        send(static_cast<void (*)(id, SEL, unsigned long)>(nullptr), view, sel_registerName("setAutoresizingMask:"), 2ul | 16ul);
        send(static_cast<void (*)(id, SEL, id)>(nullptr), p, sel_registerName("addSubview:"), view);
        return view;
    }

    void delete_child_view(void* view)
    {
        if (!view) return;
        id v = static_cast<id>(view);
        reinterpret_cast<void (*)(id, SEL)>(objc_msgSend)(v, sel_registerName("removeFromSuperview"));
        reinterpret_cast<void (*)(id, SEL)>(objc_msgSend)(v, sel_registerName("release"));
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
    // (sky.vert, sky.frag: no points). Bindings: 0 the points, 1 the style (or
    // the sky's attitude), 2 an image. The sky kinds are drawn at the far
    // plane without writing depth, stars blended by the brighter color.
    enum class PointsKind { lines, markers, stars, skyTexture };

    struct PointsPipeline
    {
        vsg::ref_ptr<vsg::DescriptorSetLayout> descriptorSetLayout;
        vsg::ref_ptr<vsg::PipelineLayout> pipelineLayout;
        vsg::ref_ptr<vsg::BindGraphicsPipeline> bindPipeline;

        PointsPipeline(const std::vector<uint32_t>& vert, const std::vector<uint32_t>& frag, PointsKind kind)
        {
            vsg::DescriptorSetLayoutBindings bindings;
            if (kind != PointsKind::skyTexture)
                bindings.push_back({0, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER, 1, VK_SHADER_STAGE_VERTEX_BIT, nullptr});
            bindings.push_back({1, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER, 1, VK_SHADER_STAGE_VERTEX_BIT | VK_SHADER_STAGE_FRAGMENT_BIT, nullptr});
            if (kind == PointsKind::markers || kind == PointsKind::skyTexture)
                bindings.push_back({2, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER, 1, VK_SHADER_STAGE_FRAGMENT_BIT, nullptr});
            descriptorSetLayout = vsg::DescriptorSetLayout::create(bindings);
            // VSG pushes the projection and model-view matrices (2 x mat4, offset 0);
            // EyeRelative pushes the eye position and the viewport (3 x vec4, offset 128)
            pipelineLayout = vsg::PipelineLayout::create(vsg::DescriptorSetLayouts{descriptorSetLayout},
                                                         vsg::PushConstantRanges{{VK_SHADER_STAGE_VERTEX_BIT, 0, 176}});
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
            vsg::GraphicsPipelineStates states{
                vsg::VertexInputState::create(), // no vertex buffers: points come from the storage buffer
                vsg::InputAssemblyState::create(VK_PRIMITIVE_TOPOLOGY_TRIANGLE_LIST),
                rasterization,
                vsg::MultisampleState::create(),
                colorBlend,
                depth,
            };
            bindPipeline = vsg::BindGraphicsPipeline::create(vsg::GraphicsPipeline::create(pipelineLayout, stages, states));
        }
    };

    // The line pipeline, created on first use and kept for the whole run (VSG
    // compiles it once per device; not destroyed at exit, to avoid depending
    // on the order of static destruction).
    PointsPipeline& line_pipeline()
    {
        static PointsPipeline* pipeline = new PointsPipeline(vkf_shaders::line_vert, vkf_shaders::line_frag, PointsKind::lines);
        return *pipeline;
    }

    PointsPipeline& marker_pipeline()
    {
        static PointsPipeline* pipeline = new PointsPipeline(vkf_shaders::marker_vert, vkf_shaders::marker_frag, PointsKind::markers);
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

    // Framebuffer pixels per window unit, stored on each vsg::View (2 on Retina).
    constexpr const char* pixel_ratio_key = "vkf_pixel_ratio";

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
            for (auto& [hints, stages] : shaderSet->variants)
            {
                if (hints && hints->defines.count("BILLBOARD") && hints->defines.count("GPU_LAYOUT"))
                {
                    auto vert = vsg::ShaderModule::create(vkf_shaders::label_vert);
                    auto frag = vsg::ShaderModule::create(vkf_shaders::label_frag);
                    vert->hints = frag->hints = hints; // this variant: never recompiled
                    stages = {vsg::ShaderStage::create(VK_SHADER_STAGE_VERTEX_BIT, "main", vert),
                              vsg::ShaderStage::create(VK_SHADER_STAGE_FRAGMENT_BIT, "main", frag)};
                }
            }
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
    // near plane).
    class LabelGroup : public vsg::Inherit<vsg::Group, LabelGroup>
    {
    public:
        bool hud = false;

        void traverse(vsg::RecordTraversal& visitor) const override
        {
            float viewport[4];
            recorded_viewport(visitor, viewport);
            if (hud) viewport[3] = 1.0f;
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
    class EyeRelative : public vsg::Inherit<vsg::Group, EyeRelative>
    {
    public:
        explicit EyeRelative(vsg::ref_ptr<vsg::PipelineLayout> layout) :
            pipelineLayout(layout) {}

        vsg::ref_ptr<vsg::PipelineLayout> pipelineLayout;

        void traverse(vsg::RecordTraversal& visitor) const override
        {
            auto state = visitor.getState();
            const vsg::dmat4 modelView = state->modelviewMatrixStack.top();
            const vsg::dvec3 eye = vsg::inverse(modelView) * vsg::dvec3(0.0, 0.0, 0.0);

            float constants[12] = {};       // eye high, eye low, viewport (width, height, pixel ratio, time)
            for (int i = 0; i < 3; ++i) split(eye[i], constants[i], constants[4 + i]);
            recorded_viewport(visitor, constants + 8);
            constants[11] = frame_time();
            auto& commandBuffer = *state->_commandBuffer;
            vkCmdPushConstants(commandBuffer, pipelineLayout->vk(commandBuffer.deviceID), VK_SHADER_STAGE_VERTEX_BIT,
                               128, sizeof(constants), constants);

            vsg::dmat4 rotationOnly = modelView;
            rotationOnly[3][0] = rotationOnly[3][1] = rotationOnly[3][2] = 0.0;
            state->modelviewMatrixStack.push(rotationOnly);
            state->dirty = true;
            Group::traverse(visitor);
            state->modelviewMatrixStack.pop();
            state->dirty = true;
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

    struct Viewer
    {
        vsg::ref_ptr<vsg::Viewer> viewer;
        vsg::ref_ptr<vsg::Window> window;
        vsg::ref_ptr<vsg::Group> scene;
        vsg::ref_ptr<vsg::Camera> camera;
        vsg::ref_ptr<vsg::CommandGraph> commandGraph;
        vsg::ref_ptr<vsg::Group> hudScene;     // HUD text, drawn by a second view on top (created when needed)
        void* childView = nullptr;             // macOS: the view VSG renders into, in the host's view
        float pixelRatio = 1.0f;               // framebuffer pixels per window unit (2 on Retina)
        bool compiled = false;
    };

    // The Vulkan device of the windows: all windows share the first one's, so a
    // scene shown in several windows has its GPU data once.
    vsg::observer_ptr<vsg::Device>& shared_device()
    {
        static vsg::observer_ptr<vsg::Device> device;
        return device;
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
    };

    struct LineStrip : Node
    {
        vsg::ref_ptr<vsg::floatArray> points; // 6 floats per point: xyz high, xyz low
        vsg::ref_ptr<vsg::vec4Array> style;  // [color, (width, separate segments, -, -)]; markers: see marker.vert
        vsg::ref_ptr<vsg::Draw> draw;
        bool markers = false;                // a quad per point

        bool segments() const { return style->at(1).y > 0.5f; }
        // the vertices drawing points first .. first+n-1 (1-based)
        void setRange(uint32_t first, uint32_t n)
        {
            if (markers) {
                draw->firstVertex = 6 * (first - 1);
                draw->vertexCount = 6 * n;
            } else if (segments()) {
                draw->firstVertex = 6 * ((first - 1) / 2);
                draw->vertexCount = 6 * (n / 2);
            } else {
                draw->firstVertex = 6 * (first - 1);
                draw->vertexCount = (n >= 2) ? 6 * (n - 1) : 0;
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

    // Options for reading files: vsgXchange's readers (images, models, fonts).
    vsg::ref_ptr<vsg::Options> read_options()
    {
        static auto options = vsg::Options::create(vsgXchange::all::create());
        return options;
    }

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
    };

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

    // Store the pixel ratio on the views of a command graph (read by EyeRelative).
    void set_pixel_ratio(vsg::CommandGraph& commandGraph, float ratio)
    {
        for (auto& child : commandGraph.children)
            if (auto renderGraph = child.cast<vsg::RenderGraph>())
                for (auto& grandchild : renderGraph->children)
                    if (auto view = grandchild.cast<vsg::View>()) view->setValue(pixel_ratio_key, ratio);
    }
} // namespace

extern "C" {

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
VKF_API void* vkf_viewer_create(int width, int height, const char* title, int title_len, long long native_window, double pixel_ratio)
{
    return guarded("vkf_viewer_create", static_cast<void*>(nullptr), [&]() -> void* {
        auto traits = vsg::WindowTraits::create();
        traits->windowTitle = to_string(title, title_len);
        traits->width = static_cast<uint32_t>(width);
        traits->height = static_cast<uint32_t>(height);
        traits->swapchainPreferences.imageUsage |= VK_IMAGE_USAGE_TRANSFER_SRC_BIT; // for capture
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
        if (auto existing = device.ref_ptr()) traits->device = existing;

        auto v = std::make_unique<Viewer>();
        v->childView = childView;
        v->window = vsg::Window::create(traits);
        if (!device.valid()) device = v->window->getOrCreateDevice();
        v->viewer = vsg::Viewer::create();
        v->viewer->addWindow(v->window);
        v->scene = vsg::Group::create();
        const auto extent = v->window->extent2D();
        v->pixelRatio = (native_window != 0) ? static_cast<float>(pixel_ratio)
                                             : static_cast<float>(extent.width) / static_cast<float>(width);

        const double aspect = static_cast<double>(extent.width) / static_cast<double>(extent.height);
        auto lookAt = ViewLookAt::create(vsg::dvec3(0.0, -4.0, 0.0), vsg::dvec3(0.0, 0.0, 0.0), vsg::dvec3(0.0, 0.0, 1.0));
        auto perspective = vsg::Perspective::create(30.0, aspect, 0.001, 1000.0);
        v->camera = vsg::Camera::create(perspective, lookAt, vsg::ViewportState::create(extent));

        v->viewer->addEventHandler(vsg::CloseHandler::create(v->viewer));
        v->viewer->addEventHandler(vsg::Trackball::create(v->camera));
        return v.release();
    });
}

VKF_API void vkf_viewer_delete(void* viewer)
{
    auto v = static_cast<Viewer*>(viewer);
    guarded("vkf_viewer_delete", 0, [&]() {
        if (v && v->compiled) v->viewer->deviceWaitIdle();
#if defined(__APPLE__)
        void* childView = v ? v->childView : nullptr;
        delete v;
        delete_child_view(childView);
#else
        delete v;
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
        v->window->bufferedEvents.push_back(vsg::ConfigureWindowEvent::create(
            v->window, vsg::clock::now(), 0, 0, static_cast<uint32_t>(width), static_cast<uint32_t>(height)));
        v->window->resize();
        return 0;
    });
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
    const vsg::vec4 color(rgba[0], rgba[1], rgba[2], rgba[3]);
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
// can be deleted afterwards). Must be called before the first frame.
VKF_API int vkf_viewer_add_node(void* viewer, void* node)
{
    auto v = static_cast<Viewer*>(viewer);
    auto n = static_cast<Node*>(node);
    if (!v || !n || v->compiled) return -1;
    v->scene->addChild(n->node);
    return 0;
}

// Render one frame (compiling the scene on the first call). Returns 1 while the
// viewer is running, 0 once its window has been closed, -1 on failure.
VKF_API int vkf_viewer_frame(void* viewer)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    ++frames_drawn;
    while (!retired.empty() && retired.front().first <= frames_drawn) retired.pop_front();
    return guarded("vkf_viewer_frame", -1, [&]() {
        if (!v->compiled)
        {
            v->commandGraph = vsg::createCommandGraphForView(v->window, v->camera, v->scene);
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
            v->viewer->assignRecordAndSubmitTaskAndPresentation({v->commandGraph});
            v->viewer->compile();
            v->compiled = true;
        }
        if (!v->viewer->advanceToNextFrame()) return 0;
        v->viewer->handleEvents();
        v->viewer->update();
        v->viewer->recordAndSubmit();
        v->viewer->present();
        return 1;
    });
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
        if (!device || !swapchain || index >= swapchain->getImageViews().size()) return -1;

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

        auto options = vsg::Options::create(vsgXchange::all::create());
        return vsg::write(pixels, to_string(filename, filename_len), options) ? 0 : -1;
    });
}

// ---------------------------------------------------------------------------
// Nodes

VKF_API void vkf_node_delete(void* node)
{
    delete static_cast<Node*>(node);
}

// Add child to a group node (a transform or a switch). Must be called before the first frame.
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

// Compile a node (and its subgraph) for a viewer that is already running, so
// it can be added to the scene between frames. Windows share their device, so
// one viewer compiles it for all of them. Returns 0 on success.
VKF_API int vkf_viewer_compile_node(void* viewer, void* node)
{
    auto v = static_cast<Viewer*>(viewer);
    auto n = static_cast<Node*>(node);
    if (!v || !n) return -1;
    if (!v->compiled) return 0; // compiled with the scene at the first frame
    return guarded("vkf_viewer_compile_node", -1, [&]() {
        auto result = v->viewer->compileManager->compile(n->node);
        if (!result) return -1;
        vsg::updateViewer(*v->viewer, result);
        return 0;
    });
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
VKF_API void* vkf_mesh_create(int n_vertices, int n_indices, const float* rgba, void* texture)
{
    if (n_vertices < 3 || n_indices < 3) return nullptr;
    return guarded("vkf_mesh_create", static_cast<void*>(nullptr), [&]() -> void* {
        auto mesh = std::make_unique<Mesh>();
        mesh->vertices = vsg::vec3Array::create(static_cast<uint32_t>(n_vertices));
        mesh->normals = vsg::vec3Array::create(static_cast<uint32_t>(n_vertices));
        mesh->texcoords = vsg::vec2Array::create(static_cast<uint32_t>(n_vertices));
        mesh->indices = vsg::uintArray::create(static_cast<uint32_t>(n_indices));
        auto colors = vsg::vec4Array::create(1, vsg::vec4(rgba[0], rgba[1], rgba[2], rgba[3]));

        auto config = vsg::GraphicsPipelineConfigurator::create(vsg::createPhongShaderSet(read_options()));
        vsg::DataList arrays;
        bool ok = config->assignArray(arrays, "vsg_Vertex", VK_VERTEX_INPUT_RATE_VERTEX, mesh->vertices) &&
                  config->assignArray(arrays, "vsg_Normal", VK_VERTEX_INPUT_RATE_VERTEX, mesh->normals) &&
                  config->assignArray(arrays, "vsg_TexCoord0", VK_VERTEX_INPUT_RATE_VERTEX, mesh->texcoords) &&
                  config->assignArray(arrays, "vsg_Color", VK_VERTEX_INPUT_RATE_INSTANCE, colors) &&
                  config->assignDescriptor("material", vsg::PhongMaterialValue::create());
        if (ok && texture)
        {
            auto sampler = vsg::Sampler::create();
            sampler->addressModeU = VK_SAMPLER_ADDRESS_MODE_REPEAT;
            sampler->addressModeV = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;
            ok = config->assignTexture("diffuseMap", static_cast<Image*>(texture)->data, sampler);
        }
        if (!ok)
        {
            std::cerr << "vkf_mesh_create: the Phong shader set lacks an expected attribute" << std::endl;
            return nullptr;
        }
        config->init();

        auto stateGroup = vsg::StateGroup::create();
        config->copyTo(stateGroup);
        auto draw = vsg::VertexIndexDraw::create();
        draw->assignArrays(arrays);
        draw->assignIndices(mesh->indices);
        draw->indexCount = static_cast<uint32_t>(n_indices);
        draw->instanceCount = 1;
        stateGroup->addChild(draw);
        mesh->node = stateGroup;
        return mesh.release();
    });
}

// One of the mesh's arrays, to fill in place: 1 = vertices (3 floats each),
// 2 = normals (3 floats), 3 = texture coordinates (2 floats), 4 = triangle
// indices (uint32, 0-based). Valid while the mesh handle exists.
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
    default: return nullptr;
    }
}

// ---------------------------------------------------------------------------
// Labels: text that always faces the screen (shaders/label.vert)

// Read a font file (TrueType, ... through vsgXchange's freetype); null on failure.
VKF_API void* vkf_font_read(const char* filename, int filename_len)
{
    return guarded("vkf_font_read", static_cast<void*>(nullptr), [&]() -> void* {
        const auto path = to_string(filename, filename_len);
        auto font = vsg::read_cast<vsg::Font>(path, read_options());
        if (!font)
        {
            std::cerr << "vkf_font_read: could not read " << path << std::endl;
            return nullptr;
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
        layout->color = vsg::vec4(rgba[0], rgba[1], rgba[2], rgba[3]);
        layout->horizontalAlignment = static_cast<vsg::StandardLayout::Alignment>(halign);
        layout->verticalAlignment = static_cast<vsg::StandardLayout::Alignment>(valign);

        auto label = vsg::Text::create();
        label->technique = vsg::GpuLayoutTechnique::create();
        label->font = f->font;
        label->layout = layout;
        label->text = vsg::stringValue::create(to_string(text, text_len));
        label->shaderSet = label_shaders().shaderSet;
        // glyphs allocated: room to change the text in place (a multiple of 4: glyph indices are uvec4s)
        const uint32_t capacity = 4 * ((std::max<uint32_t>(64, 2 * static_cast<uint32_t>(std::max(text_len, 0))) + 3) / 4);
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

// Change a label's text (texts up to its capacity are updated in place).
VKF_API int vkf_label_set_text(void* label, const char* text, int text_len)
{
    auto l = dynamic_cast<Label*>(static_cast<Node*>(label));
    if (!l) return -1;
    return guarded("vkf_label_set_text", -1, [&]() {
        l->text->text = vsg::stringValue::create(to_string(text, text_len));
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
        l->layout->color = vsg::vec4(rgba[0], rgba[1], rgba[2], rgba[3]);
        l->text->setup(l->capacity, read_options());
        return 0;
    });
}

// Show a label in the viewer's HUD (at x, y from 0 to 1 across the viewport,
// bottom to top: the label's position), on top of the scene. Before the first frame.
VKF_API int vkf_viewer_hud_add(void* viewer, void* label)
{
    auto v = static_cast<Viewer*>(viewer);
    auto l = dynamic_cast<Label*>(static_cast<Node*>(label));
    if (!v || !l || v->compiled) return -1;
    if (auto group = l->node.cast<LabelGroup>()) group->hud = true;
    if (!v->hudScene) v->hudScene = vsg::Group::create();
    v->hudScene->addChild(l->node);
    return 0;
}

// ---------------------------------------------------------------------------
// Line strips: thick lines of constant pixel width (shaders/line.vert), with
// points kept to double precision (drawn relative to the eye)

// A line strip holding up to capacity points (initially none). Fortran fills
// its point and style buffers in place (vkf_line_points, vkf_line_style) and
// then calls vkf_line_points_changed / vkf_line_style_changed. It can be shown
// in any number of windows.
VKF_API void* vkf_line_create(int capacity)
{
    if (capacity < 2) return nullptr;
    return guarded("vkf_line_create", static_cast<void*>(nullptr), [&]() -> void* {
        auto& pipeline = line_pipeline();
        auto line = std::make_unique<LineStrip>();
        line->points = vsg::floatArray::create(6 * static_cast<uint32_t>(capacity), 0.0f);
        line->points->properties.dataVariance = vsg::DYNAMIC_DATA;
        line->style = dynamic_vec4_array(2);

        auto descriptorSet = vsg::DescriptorSet::create(pipeline.descriptorSetLayout, vsg::Descriptors{
            vsg::DescriptorBuffer::create(line->points, 0, 0, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
            vsg::DescriptorBuffer::create(line->style, 1, 0, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
        });

        auto stateGroup = vsg::StateGroup::create();
        stateGroup->add(pipeline.bindPipeline);
        stateGroup->add(vsg::BindDescriptorSet::create(VK_PIPELINE_BIND_POINT_GRAPHICS, pipeline.pipelineLayout, 0, descriptorSet));
        line->draw = vsg::Draw::create(0, 1, 0, 0);
        stateGroup->addChild(line->draw);
        auto eyeRelative = EyeRelative::create(pipeline.pipelineLayout);
        eyeRelative->addChild(stateGroup);
        line->node = eyeRelative;
        return line.release();
    });
}

// Markers at up to capacity points (initially none): a screen-space quad per
// point, of the shape and size in the style buffer (shaders/marker.vert), with
// image (a vkf_image_read handle, or null) for the image shape. The point and
// style buffers are those of a line (vkf_line_points, vkf_line_style,
// vkf_line_points_changed, vkf_line_set_range, vkf_line_style_changed).
VKF_API void* vkf_marker_create(int capacity, void* image)
{
    if (capacity < 1) return nullptr;
    return guarded("vkf_marker_create", static_cast<void*>(nullptr), [&]() -> void* {
        auto& pipeline = marker_pipeline();
        auto marker = std::make_unique<LineStrip>();
        marker->markers = true;
        marker->points = vsg::floatArray::create(6 * static_cast<uint32_t>(capacity), 0.0f);
        marker->points->properties.dataVariance = vsg::DYNAMIC_DATA;
        marker->style = dynamic_vec4_array(2);

        vsg::ref_ptr<vsg::Data> data;
        if (image) data = static_cast<Image*>(image)->data;
        else data = vsg::ubvec4Array2D::create(1, 1, vsg::ubvec4(255, 255, 255, 255), vsg::Data::Properties{VK_FORMAT_R8G8B8A8_UNORM});
        auto sampler = vsg::Sampler::create();
        sampler->addressModeU = sampler->addressModeV = VK_SAMPLER_ADDRESS_MODE_CLAMP_TO_EDGE;

        auto descriptorSet = vsg::DescriptorSet::create(pipeline.descriptorSetLayout, vsg::Descriptors{
            vsg::DescriptorBuffer::create(marker->points, 0, 0, VK_DESCRIPTOR_TYPE_STORAGE_BUFFER),
            vsg::DescriptorBuffer::create(marker->style, 1, 0, VK_DESCRIPTOR_TYPE_UNIFORM_BUFFER),
            vsg::DescriptorImage::create(sampler, data, 2, 0, VK_DESCRIPTOR_TYPE_COMBINED_IMAGE_SAMPLER),
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

// The strip's point buffer, 6 floats per point (x, y, z high parts, then x, y,
// z low parts; see shaders/line.vert), and its capacity in points. Valid while
// the line handle exists.
VKF_API float* vkf_line_points(void* line, int* capacity)
{
    auto l = dynamic_cast<LineStrip*>(static_cast<Node*>(line));
    if (!l) return nullptr;
    *capacity = static_cast<int>(l->points->size() / 6);
    return l->points->data();
}

// The strip's style buffer: 8 floats (r, g, b, a, width in window units,
// 1 for separate segments (pairs of points) or 0 for a strip, -, -). Set the
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
    if (!l || n < 0 || 6 * static_cast<uint32_t>(n) > l->points->size()) return -1;
    l->points->dirty();
    l->setRange(1, static_cast<uint32_t>(n));
    return 0;
}

// Draw points first .. first+n-1 only (1-based; no data is uploaded): e.g.
// for trace mode. (Separate segments: first odd, n even.)
VKF_API int vkf_line_set_range(void* line, int first, int n)
{
    auto l = dynamic_cast<LineStrip*>(static_cast<Node*>(line));
    if (!l || first < 1 || n < 0 || 6 * static_cast<uint32_t>(first - 1 + n) > l->points->size()) return -1;
    l->setRange(static_cast<uint32_t>(first), static_cast<uint32_t>(n));
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

#endif // VKF_EXTERNAL_LAYER
