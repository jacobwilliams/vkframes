// vkframes: maps and terrain with Rocky (https://github.com/pelicanmapping/rocky),
// optional. Built with Rocky when tools/fpm_vsg.py finds it installed (the
// `rocky` pixi environment: VKF_ROCKY defined); otherwise these functions only
// report that Rocky is not available.
//
// A map is Rocky's MapNode (geocentric, in meters) under a scale transform to
// the scene's units, created with the Rocky context of the viewer it is first
// shown in (one context per viewer, kept for the run). Rocky pages its tiles
// in through the viewer's update operations, which vkf_viewer_frame runs.

#ifndef VKF_EXTERNAL_LAYER  // Windows: built into vkf.dll instead (see fpm_vsg.py)

#if defined(_WIN32)
#    define VKF_API __declspec(dllexport)
#else
#    define VKF_API
#endif

#include <vsg/all.h>

#include <iostream>
#include <string>

#ifdef VKF_ROCKY
#    include <rocky/ElevationSampler.h>
#    include <rocky/GDALElevationLayer.h>
#    include <rocky/GDALImageLayer.h>
#    include <rocky/Profile.h>
#    include <rocky/TMSElevationLayer.h>
#    include <rocky/TMSImageLayer.h>
#    include <rocky/vsg/MapNode.h>
#    include <rocky/vsg/SharedRenderData.h>
#    include <rocky/vsg/ViewDependentState.h>
#    include <rocky/vsg/VSGContext.h>
#    include <gdal.h>
#    include <algorithm>
#    include <map>
#endif

namespace vkf
{
    // from vkf_layer.cpp
    vsg::ref_ptr<vsg::Viewer> vsg_viewer(void* viewer);
    void* wrap_node(vsg::ref_ptr<vsg::Node> node);
    vsg::ref_ptr<vsg::Node> unwrap_node(void* node);

    // called by vkf_viewer_delete (defined below)
    void release_viewer(vsg::Viewer* viewer);

    // called by vkf_viewer_create and vkf_viewer_frame (defined below)
    void device_features(vsg::WindowTraits& traits);
    void prepare_view(vsg::Viewer* viewer, vsg::View& view, vsg::ref_ptr<vsg::Device> device);
} // namespace vkf

#ifdef VKF_ROCKY
namespace
{
    // One Rocky context per VSG viewer, released when vkframes deletes the
    // viewer (vkf::release_viewer): its destructor stops Rocky's tile jobs,
    // which must not run on into the program's exit.
    std::map<vsg::Viewer*, rocky::VSGContextSingleton>& contexts()
    {
        static auto* all = new std::map<vsg::Viewer*, rocky::VSGContextSingleton>; // (not destroyed at exit)
        return *all;
    }

    // Removes vsgXchange's OpenSceneGraph reader (in conda-forge's Windows
    // build): it claims streamed TIFF before Rocky's GDAL reader, reloading
    // all OSG plugins per tile from Rocky's threads, and crashes.
    void remove_osg_reader(vsg::ReaderWriters& readerWriters)
    {
        for (auto it = readerWriters.begin(); it != readerWriters.end();)
        {
            if (auto crw = it->cast<vsg::CompositeReaderWriter>()) remove_osg_reader(crw->readerWriters);
            if (std::string((*it)->className()) == "osg2vsg::OSG")
                it = readerWriters.erase(it);
            else
                ++it;
        }
    }

    rocky::VSGContext context_for(const vsg::ref_ptr<vsg::Viewer>& viewer)
    {
        auto& contexts = ::contexts();
        auto it = contexts.find(viewer.get());
        if (it == contexts.end())
        {
            it = contexts.emplace(viewer.get(), rocky::VSGContextFactory::create(viewer)).first;
            // vkframes' framebuffers are UNORM (colors in display values): the
            // terrain encodes its (linear) color to sRGB itself (a patch to
            // Rocky's terrain shader, tools/build_rocky.py)
            it->second.get()->shaderCompileSettings->defines.insert("VKF_UNORM_FRAMEBUFFER");
            remove_osg_reader(it->second.get()->readerWriterOptions->readerWriters);
        }
        return it->second.get();
    }

    rocky::MapNode* map_node(void* map)
    {
        auto node = vkf::unwrap_node(map);
        auto scale = node ? node->cast<vsg::MatrixTransform>() : nullptr;
        if (!scale || scale->children.empty()) return nullptr;
        return scale->children[0]->cast<rocky::MapNode>();
    }

    std::string to_string(const char* s, int n) { return std::string(s, static_cast<size_t>(n)); }

    // A tiled layer's source (see vkf_rocky_add_image): kind 0 a TMS service,
    // 2 a {z}/{x}/{y} template of geodetic tiles on the map's body (2 x 1 at
    // level 0, rows from the top: WMTS, NASA's Moon Trek), 3 one of web
    // Mercator tiles (1 x 1, rows from the top: OpenStreetMap, XYZ). (With a
    // profile given, Rocky numbers a template's rows from the top already;
    // its invertY would number them from the bottom, as TMS does.)
    template<class Layer>
    void set_tiles(Layer& layer, int kind, const std::string& uri, const rocky::MapNode& mapNode)
    {
        layer.uri = rocky::URI(uri);
        if (kind == 2)
        {
            layer.profile = rocky::Profile(mapNode.profile.srs().geodeticSRS(), rocky::Box(-180.0, -90.0, 180.0, 90.0), 2, 1);
        }
        else if (kind == 3)
        {
            layer.profile = rocky::Profile("spherical-mercator");
        }
    }
} // namespace
#endif

// The viewer is being deleted (vkframes dropped its references): release its
// Rocky context in the order Rocky's Application does: stop the tile jobs,
// release the scene (the map nodes, while their context lives), then the
// context (which holds the viewer).
void vkf::release_viewer(vsg::Viewer* viewer)
{
#ifdef VKF_ROCKY
    auto it = contexts().find(viewer);
    if (it == contexts().end()) return;
    it->second.get()->io.services().jobs.shutdown();
    viewer->deviceWaitIdle();
    viewer->recordAndSubmitTasks.clear();
    viewer->presentations.clear();
    contexts().erase(it);
#else
    (void)viewer;
#endif
}

// The device features Rocky's shaders need (as Rocky's DisplayManager enables
// them): non-uniform indexing of the terrain's texture arrays.
void vkf::device_features(vsg::WindowTraits& traits)
{
#ifdef VKF_ROCKY
    traits.vulkanVersion = std::max(traits.vulkanVersion, static_cast<uint32_t>(VK_API_VERSION_1_2));
    auto& features = traits.deviceFeatures->get<VkPhysicalDeviceVulkan12Features, VK_STRUCTURE_TYPE_PHYSICAL_DEVICE_VULKAN_1_2_FEATURES>();
    features.descriptorIndexing = VK_TRUE;
    features.runtimeDescriptorArray = VK_TRUE;
    features.shaderSampledImageArrayNonUniformIndexing = VK_TRUE;
#else
    (void)traits;
#endif
}

// Before the viewer's first compile: a view showing a map gets Rocky's
// view-dependent state (its shaders' view data, descriptor set 1), as Rocky's
// DisplayManager installs it.
void vkf::prepare_view(vsg::Viewer* viewer, vsg::View& view, vsg::ref_ptr<vsg::Device> device)
{
#ifdef VKF_ROCKY
    auto it = contexts().find(viewer);
    if (it == contexts().end()) return;
    auto context = it->second.get();
    if (!context->trackView(&view)) return;
    auto vds = rocky::ViewDependentStateEx::create(vsg::ref_ptr<vsg::View>(&view), device);
    view.viewDependentState = vds;
    context->sharedRenderData->viewDependentState[view.viewID] = vds;
    auto& ids = context->activeViewIDs;
    if (std::find(ids.begin(), ids.end(), view.viewID) == ids.end()) ids.push_back(view.viewID);
#else
    (void)viewer; (void)view; (void)device;
#endif
}

extern "C" {

// 1 if vkframes was built with Rocky.
VKF_API int vkf_rocky_available()
{
#ifdef VKF_ROCKY
    return 1;
#else
    return 0;
#endif
}

// A map (a node handle: vkf_node_*) for the viewer, in Rocky's profile
// `profile` (e.g. "global-geodetic", "global-qsc", "moon"; empty: Rocky's
// default), drawn at `scale` scene units per meter. Null without Rocky or on
// failure.
VKF_API void* vkf_rocky_map_create(void* viewer, const char* profile, int profile_len, double scale)
{
#ifdef VKF_ROCKY
    try
    {
        auto vsgViewer = vkf::vsg_viewer(viewer);
        if (!vsgViewer) return nullptr;
        auto context = context_for(vsgViewer);
        if (!context->status.ok())
        {
            std::cerr << "vkf_rocky_map_create: " << context->status.error().message << std::endl;
            return nullptr;
        }
        auto mapNode = rocky::MapNode::create(context);
        const auto name = to_string(profile, profile_len);
        if (!name.empty()) mapNode->profile = rocky::Profile(name);
        auto transform = vsg::MatrixTransform::create(vsg::scale(scale));
        transform->addChild(mapNode);
        return vkf::wrap_node(transform);
    }
    catch (const std::exception& e)
    {
        std::cerr << "vkf_rocky_map_create: " << e.what() << std::endl;
        return nullptr;
    }
#else
    (void)viewer; (void)profile; (void)profile_len; (void)scale;
    return nullptr;
#endif
}

// A map's settings: lighting (1: lit by the scene's lights, as vkframes'
// spheres; 0: imagery as is, Rocky's default) and the color where there is no
// imagery (rgba, 0 to 1). Returns 0, or -1.
VKF_API int vkf_rocky_map_settings(void* map, int lighting, const float* background)
{
#ifdef VKF_ROCKY
    auto mapNode = map_node(map);
    if (!mapNode) return -1;
    auto& settings = mapNode->terrainSettings();
    settings.lighting = (lighting != 0);
    settings.backgroundColor = rocky::Color(background[0], background[1], background[2], background[3]);
    return 0;
#else
    (void)map; (void)lighting; (void)background;
    return -1;
#endif
}

// The equatorial radius of a map's body, in meters (0 without Rocky or on failure).
VKF_API double vkf_rocky_map_radius(void* map)
{
#ifdef VKF_ROCKY
    auto mapNode = map_node(map);
    return mapNode ? mapNode->srs().ellipsoid().semiMajorAxis() : 0.0;
#else
    (void)map;
    return 0.0;
#endif
}

// The height (meters) of the elevation data in file `uri` (GDAL) at latitude
// and longitude (degrees) on the body of Rocky's profile `profile` (as
// vkf_rocky_map_create), sampled directly (no map or window needed; files are
// kept open for later samples). *ok is 1 if there was data there.
VKF_API double vkf_rocky_elevation(const char* uri, int uri_len, const char* profile, int profile_len,
                                   double lat, double lon, int* ok)
{
    *ok = 0;
#ifdef VKF_ROCKY
    try
    {
        static std::map<std::string, std::shared_ptr<rocky::GDALElevationLayer>> layers;
        static rocky::IOOptions io;
        const auto file = to_string(uri, uri_len);
        auto& layer = layers[file];
        if (!layer)
        {
            GDALAllRegister();   // (Rocky's context does this, but there may be no map yet)
            layer = rocky::GDALElevationLayer::create();
            layer->uri = rocky::URI(file);
            auto status = layer->open(io);
            if (status.failed())
            {
                std::cerr << "vkf_rocky_elevation: " << file << ": " << status.error().message << std::endl;
                return 0.0;
            }
        }
        if (!layer->status().ok()) return 0.0;
        const auto name = to_string(profile, profile_len);
        const auto srs = (name.empty() ? rocky::Profile("global-geodetic") : rocky::Profile(name)).srs().geodeticSRS();
        rocky::ElevationSampler sampler;
        sampler.layer = layer;
        auto sample = sampler.sample(rocky::GeoPoint(srs, lon, lat), io);
        if (!sample.ok() || sample.value().height == rocky::NO_DATA_VALUE) return 0.0;
        *ok = 1;
        return sample.value().height;
    }
    catch (const std::exception& e)
    {
        std::cerr << "vkf_rocky_elevation: " << e.what() << std::endl;
        return 0.0;
    }
#else
    (void)uri; (void)uri_len; (void)profile; (void)profile_len; (void)lat; (void)lon;
    return 0.0;
#endif
}

// Add an image layer to a map: kind 0 a TMS service (uri its root), 1 a file
// or service GDAL reads, 2 a template ({z}, {x}, {y}) of geodetic tiles on the
// map's body, rows from the top (WMTS-style, e.g. NASA's Moon Trek), 3 a
// template of web Mercator tiles (OpenStreetMap-style). Returns 0, or -1.
VKF_API int vkf_rocky_add_image(void* map, int kind, const char* uri, int uri_len)
{
#ifdef VKF_ROCKY
    auto mapNode = map_node(map);
    if (!mapNode) return -1;
    std::shared_ptr<rocky::ImageLayer> layer;
    if (kind == 0 || kind == 2 || kind == 3)
    {
        auto tms = rocky::TMSImageLayer::create();
        set_tiles(*tms, kind, to_string(uri, uri_len), *mapNode);
        layer = tms;
    }
    else if (kind == 1)
    {
        auto gdal = rocky::GDALImageLayer::create();
        gdal->uri = rocky::URI(to_string(uri, uri_len));
        layer = gdal;
    }
    else return -1;
    mapNode->map->add(layer);
    return 0;
#else
    (void)map; (void)kind; (void)uri; (void)uri_len;
    return -1;
#endif
}

// Add an elevation layer to a map: kind as vkf_rocky_add_image; encoding 0
// single-channel heights (meters), 1 Mapbox RGB, 2 Terrarium RGB. Returns 0,
// or -1.
VKF_API int vkf_rocky_add_elevation(void* map, int kind, const char* uri, int uri_len, int encoding)
{
#ifdef VKF_ROCKY
    auto mapNode = map_node(map);
    if (!mapNode) return -1;
    std::shared_ptr<rocky::ElevationLayer> layer;
    if (kind == 0 || kind == 2 || kind == 3)
    {
        auto tms = rocky::TMSElevationLayer::create();
        set_tiles(*tms, kind, to_string(uri, uri_len), *mapNode);
        layer = tms;
    }
    else if (kind == 1)
    {
        auto gdal = rocky::GDALElevationLayer::create();
        gdal->uri = rocky::URI(to_string(uri, uri_len));
        layer = gdal;
    }
    else return -1;
    using Encoding = rocky::ElevationLayer::Encoding;
    layer->encoding = (encoding == 1) ? Encoding::MapboxRGB : (encoding == 2) ? Encoding::TerrariumRGB : Encoding::SingleChannel;
    mapNode->map->add(layer);
    return 0;
#else
    (void)map; (void)kind; (void)uri; (void)uri_len; (void)encoding;
    return -1;
#endif
}

} // extern "C"

#endif // VKF_EXTERNAL_LAYER
