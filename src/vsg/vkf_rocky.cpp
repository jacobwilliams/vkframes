// vkframes: maps and terrain with Rocky (https://github.com/pelicanmapping/rocky),
// optional. Built with Rocky when tools/fpm_vsg.py finds it installed (the
// `rocky` pixi environment: VKF_ROCKY defined); otherwise these functions only
// report that Rocky is not available.
//
// A map is Rocky's MapNode (geocentric, in meters) under a scale transform to
// the scene's units, made with the Rocky context of a viewer (one context per
// viewer): Rocky pages its tiles in through that viewer's update operations,
// which vkf_viewer_frame runs, and keeps view data for its views. So a map is
// drawn in that viewer's window only (a WindowMap, its top node, skips other
// windows' views), and when that viewer is deleted the map is released (with
// its context) and draws nothing; TerrainBody then makes it again for a
// viewer that exists (vkf_rocky_map_alive).

#ifndef VKF_EXTERNAL_LAYER  // Windows: built into vkf.dll instead (see fpm_vsg.py)

#include "vkf_common.h"

using vkf::to_string;

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
#    include <set>
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
    bool prepare_view(vsg::Viewer* viewer, vsg::View& view, vsg::ref_ptr<vsg::Device> device);
} // namespace vkf

#ifdef VKF_ROCKY
namespace
{
    // A map's top node: the map (its scale transform and MapNode) is drawn
    // only in the views of its viewer's window, which have its Rocky
    // context's view data (vkf::prepare_view); once the viewer is deleted
    // (vkf::release_viewer) it has no children and no context.
    class WindowMap : public vsg::Inherit<vsg::Group, WindowMap>
    {
    public:
        rocky::VSGContext context = nullptr;

        void traverse(vsg::RecordTraversal& visitor) const override
        {
            if (!context) return;
            const auto& vds = visitor.viewDependentState;
            if (!vds || !vds->view || vds->view->viewID >= ROCKY_MAX_NUMBER_OF_VIEWS) return;
            const auto& own = context->sharedRenderData->viewDependentState[vds->view->viewID];
            if (static_cast<const vsg::ViewDependentState*>(own.get()) != vds.get()) return;   // another window's view
            Group::traverse(visitor);
        }
    };

    // A viewer's Rocky context, its maps, and the views given its view data.
    struct Context
    {
        rocky::VSGContextSingleton context;
        std::vector<vsg::observer_ptr<WindowMap>> maps;
        std::set<uint32_t> preparedViews;
    };

    // One Rocky context per VSG viewer, released when vkframes deletes the
    // viewer (vkf::release_viewer): its destructor stops Rocky's tile jobs,
    // which must not run on into the program's exit.
    std::map<vsg::Viewer*, Context>& contexts()
    {
        static auto* all = new std::map<vsg::Viewer*, Context>; // (not destroyed at exit)
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
            it = contexts.emplace(viewer.get(), Context{rocky::VSGContextFactory::create(viewer)}).first;
            auto context = it->second.context.get();
            // vkframes' framebuffers are UNORM (colors in display values): the
            // terrain encodes its (linear) color to sRGB itself (vkframes' copy
            // of Rocky's terrain shader, shaders/rocky)
            context->shaderCompileSettings->defines.insert("VKF_UNORM_FRAMEBUFFER");
            remove_osg_reader(context->readerWriterOptions->readerWriters);
            // a shader's includes are found next to it, or on the reader's
            // paths: Rocky's shader directories, for vkframes' copies of the
            // shaders it changes (shaders/rocky, first on ROCKY_FILE_PATH)
            for (auto& path : context->searchPaths)
                if (vsg::fileExists(path / "shaders")) context->readerWriterOptions->paths.push_back(path / "shaders");
        }
        return it->second.context.get();
    }

    WindowMap* window_map(void* map)
    {
        auto node = vkf::unwrap_node(map);
        return node ? node->cast<WindowMap>() : nullptr;
    }

    // The MapNode of a map (null once its viewer is deleted).
    rocky::MapNode* map_node(void* map)
    {
        auto top = window_map(map);
        if (!top || top->children.empty()) return nullptr;
        auto scale = top->children[0]->cast<vsg::MatrixTransform>();
        if (!scale || scale->children.empty()) return nullptr;
        return scale->children[0]->cast<rocky::MapNode>();
    }

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
// release the scene (its maps' MapNodes, while their context lives: other
// windows may still show the frame tree, whose WindowMaps then draw nothing),
// then the context (which holds the viewer).
void vkf::release_viewer(vsg::Viewer* viewer)
{
#ifdef VKF_ROCKY
    auto it = contexts().find(viewer);
    if (it == contexts().end()) return;
    it->second.context.get()->io.services().jobs.shutdown();
    viewer->deviceWaitIdle();                  // (the device is shared: all windows' frames)
    for (auto& observed : it->second.maps)
        if (auto map = observed.ref_ptr())
        {
            map->children.clear();
            map->context = nullptr;
        }
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

// A view of a viewer with maps gets Rocky's view-dependent state (its
// shaders' view data, descriptor set 1), as Rocky's DisplayManager installs
// it: called each frame (once a map exists, for a view not given it yet).
// True if it was given now (the caller compiles it if the viewer runs).
bool vkf::prepare_view(vsg::Viewer* viewer, vsg::View& view, vsg::ref_ptr<vsg::Device> device)
{
#ifdef VKF_ROCKY
    auto it = contexts().find(viewer);
    if (it == contexts().end()) return false;
    auto& entry = it->second;
    if (entry.preparedViews.count(view.viewID)) return false;
    auto context = entry.context.get();
    if (!context->trackView(&view)) return false;
    auto vds = rocky::ViewDependentStateEx::create(vsg::ref_ptr<vsg::View>(&view), device);
    view.viewDependentState = vds;
    context->sharedRenderData->viewDependentState[view.viewID] = vds;
    auto& ids = context->activeViewIDs;
    if (std::find(ids.begin(), ids.end(), view.viewID) == ids.end()) ids.push_back(view.viewID);
    entry.preparedViews.insert(view.viewID);
    return true;
#else
    (void)viewer; (void)view; (void)device;
    return false;
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
        auto top = WindowMap::create();
        top->context = context;
        top->addChild(transform);
        contexts()[vsgViewer.get()].maps.emplace_back(top);
        return vkf::wrap_node(top);
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

// 1 while a map can be drawn (its viewer exists), 0 once it has been released.
VKF_API int vkf_rocky_map_alive(void* map)
{
#ifdef VKF_ROCKY
    auto top = window_map(map);
    return (top && top->context) ? 1 : 0;
#else
    (void)map;
    return 0;
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
// kept open, with a sampler whose tile cache serves later samples nearby).
// *ok is 1 if there was data there.
VKF_API double vkf_rocky_elevation(const char* uri, int uri_len, const char* profile, int profile_len,
                                   double lat, double lon, int* ok)
{
    *ok = 0;
#ifdef VKF_ROCKY
    try
    {
        static std::map<std::string, rocky::ElevationSampler> samplers;
        static rocky::IOOptions io;
        const auto file = to_string(uri, uri_len);
        auto& sampler = samplers[file];
        if (!sampler.layer)
        {
            GDALAllRegister();   // (Rocky's context does this, but there may be no map yet)
            auto layer = rocky::GDALElevationLayer::create();
            layer->uri = rocky::URI(file);
            sampler.layer = layer;
            auto status = layer->open(io);
            if (status.failed())
            {
                std::cerr << "vkf_rocky_elevation: " << file << ": " << status.error().message << std::endl;
                return 0.0;
            }
        }
        if (!sampler.ok()) return 0.0;
        const auto name = to_string(profile, profile_len);
        const auto srs = (name.empty() ? rocky::Profile("global-geodetic") : rocky::Profile(name)).srs().geodeticSRS();
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
