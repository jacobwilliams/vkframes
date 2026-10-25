// vkframes C++ VSG layer: generic rendering primitives behind an extern "C"
// interface, called from Fortran (src/vsg/vkf_layer.f90).
//
// Conventions:
//   - objects cross the boundary as opaque pointers; the layer owns them and
//     Fortran releases them with the matching *_delete call;
//   - strings cross as (pointer, length), not NUL-terminated;
//   - no C++ exception crosses the boundary: each entry point catches and
//     reports failure through its return value.

#include <vsg/all.h>

#include <algorithm>
#include <cstring>
#include <iostream>
#include <memory>
#include <string>

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
        return fallback;
    }

    struct Viewer
    {
        vsg::ref_ptr<vsg::Viewer> viewer;
        vsg::ref_ptr<vsg::Window> window;
        vsg::ref_ptr<vsg::Group> scene;
        vsg::ref_ptr<vsg::Camera> camera;
        bool compiled = false;
    };

    struct Node
    {
        vsg::ref_ptr<vsg::Node> node;
    };
} // namespace

extern "C" {

// Name of the first Vulkan physical device; returns its length, or -1 on failure.
// Needs no window, so it checks that the Vulkan loader finds a driver (MoltenVK on macOS).
int vkf_vulkan_device_name(char* buf, int buflen)
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

// A viewer with one window, an empty scene and a trackball camera; null on failure.
void* vkf_viewer_create(int width, int height, const char* title, int title_len)
{
    return guarded("vkf_viewer_create", static_cast<void*>(nullptr), [&]() -> void* {
        auto traits = vsg::WindowTraits::create();
        traits->windowTitle = to_string(title, title_len);
        traits->width = static_cast<uint32_t>(width);
        traits->height = static_cast<uint32_t>(height);

        auto v = std::make_unique<Viewer>();
        v->window = vsg::Window::create(traits);
        v->viewer = vsg::Viewer::create();
        v->viewer->addWindow(v->window);
        v->scene = vsg::Group::create();

        const auto extent = v->window->extent2D();
        const double aspect = static_cast<double>(extent.width) / static_cast<double>(extent.height);
        auto lookAt = vsg::LookAt::create(vsg::dvec3(0.0, -4.0, 0.0), vsg::dvec3(0.0, 0.0, 0.0), vsg::dvec3(0.0, 0.0, 1.0));
        auto perspective = vsg::Perspective::create(30.0, aspect, 0.01, 100.0);
        v->camera = vsg::Camera::create(perspective, lookAt, vsg::ViewportState::create(extent));

        v->viewer->addEventHandler(vsg::CloseHandler::create(v->viewer));
        v->viewer->addEventHandler(vsg::Trackball::create(v->camera));
        return v.release();
    });
}

void vkf_viewer_delete(void* viewer)
{
    auto v = static_cast<Viewer*>(viewer);
    if (v && v->viewer) v->viewer->deviceWaitIdle();
    delete v;
}

// Add a node to the viewer's scene (the scene holds a reference; the node handle
// can be deleted afterwards). Must be called before the first frame.
int vkf_viewer_add_node(void* viewer, void* node)
{
    auto v = static_cast<Viewer*>(viewer);
    auto n = static_cast<Node*>(node);
    if (!v || !n || v->compiled) return -1;
    v->scene->addChild(n->node);
    return 0;
}

// Render one frame (compiling the scene on the first call). Returns 1 while the
// viewer is running, 0 once its window has been closed, -1 on failure.
int vkf_viewer_frame(void* viewer)
{
    auto v = static_cast<Viewer*>(viewer);
    if (!v) return -1;
    return guarded("vkf_viewer_frame", -1, [&]() {
        if (!v->compiled)
        {
            auto commandGraph = vsg::createCommandGraphForView(v->window, v->camera, v->scene);
            v->viewer->assignRecordAndSubmitTaskAndPresentation({commandGraph});
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

// A lit sphere of the given radius and color (rgba), centered at the origin.
void* vkf_sphere_create(double radius, const float* rgba)
{
    return guarded("vkf_sphere_create", static_cast<void*>(nullptr), [&]() -> void* {
        vsg::GeometryInfo geom;
        const float d = static_cast<float>(2.0 * radius);
        geom.dx.set(d, 0.0f, 0.0f);
        geom.dy.set(0.0f, d, 0.0f);
        geom.dz.set(0.0f, 0.0f, d);
        if (rgba) geom.color.set(rgba[0], rgba[1], rgba[2], rgba[3]);
        auto n = std::make_unique<Node>();
        n->node = vsg::Builder::create()->createSphere(geom);
        return n.release();
    });
}

void vkf_node_delete(void* node)
{
    delete static_cast<Node*>(node);
}

} // extern "C"
