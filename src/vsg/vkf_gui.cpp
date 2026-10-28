// vkframes: GUI panels in a window, drawn flat on top of the scene (Dear ImGui,
// through vsgImGui): see src/vkf_gui.f90.
//
// Fortran describes the panels between frames, immediate-mode, as a list of
// commands (a panel, text, a button, a checkbox, a slider, ...: vkf_gui_*);
// each widget's call returns what the user did with it in the frame before
// (clicked, or a new value), kept here by panel and label. During the frame's
// draw, the window's RenderImGui (in its render graph) replays the commands
// as ImGui calls and keeps the results; then the list is cleared for the next
// frame. So no Fortran is called from inside the draw.
//
// Each window has its own ImGui context (vsgImGui uses ImGui's current one),
// made current around its drawing and its events. The window's events go to
// ImGui first (vsgImGui's SendEventsToImGui, an event handler ahead of the
// others): those ImGui wants (over a panel) are marked handled, so the
// trackball, the program's event handler and picking don't get them. The GUI
// is scaled by the window's pixel ratio (framebuffer pixels per window unit),
// in the font given.

#ifndef VKF_EXTERNAL_LAYER  // Windows: built into vkf.dll instead (see fpm_vsg.py)

#include "vkf_common.h"

using vkf::to_string;

#include <vsg/all.h>
#include <vsgImGui/RenderImGui.h>
#include <vsgImGui/SendEventsToImGui.h>
#include <imgui.h>

#include <algorithm>
#include <map>
#include <string>
#include <vector>

namespace vkf
{
    struct Gui;
    // called by vkf_layer.cpp: vkf_viewer_gui, the frame, vkf_viewer_delete
    Gui* gui_create(float pixel_ratio, const std::string& font, float size);
    void gui_attach(Gui* gui, vsg::ref_ptr<vsg::Window> window, vsg::Viewer& viewer, vsg::CommandGraph& commandGraph);
    bool gui_attached(const Gui* gui);
    void gui_frame_done(Gui* gui);
    void gui_delete(Gui* gui, vsg::Viewer& viewer);
} // namespace vkf

namespace
{
    enum Kind
    {
        BEGIN,
        END,
        TEXT,
        BUTTON,
        CHECKBOX,
        SLIDER,
        SEPARATOR,
        SAME_LINE
    };

    struct Command
    {
        Kind kind;
        std::string text;      // panel title, text, widget label
        std::string key;       // panel and label: where its result is kept
        double x = -1.0, y = -1.0, pivot_x = 0.0, pivot_y = 0.0;   // panels: position (0 to 1, y up), pivot
        bool follow = false;   // panels: placed at the position every frame (else at first)
        bool value = false;    // checkboxes
        double number = 0.0, low = 0.0, high = 1.0;                // sliders
    };

    struct Result
    {
        bool clicked = false;  // buttons: clicked (since the program last asked)
        bool changed = false;  // checkboxes, sliders: a new value
        bool value = false;
        double number = 0.0;
    };
} // namespace

struct vkf::Gui
{
    ImGuiContext* context = nullptr;
    float pixelRatio = 1.0f;
    std::string font;
    float size = 15.0f;
    vsg::ref_ptr<vsg::Node> node;                    // in the render graph (a GuiNode)
    vsg::ref_ptr<vsg::Visitor> events;               // the event handler (a GuiEvents)
    std::vector<Command> commands;                   // for the next draw
    std::map<std::string, Result> results;           // from the draws so far
    std::string panel;                               // the panel being described
};

namespace
{
    // Replays a window's commands as ImGui calls (a child of its RenderImGui,
    // which begins and ends the ImGui frame around its children).
    class GuiCommands : public vsg::Inherit<vsg::Node, GuiCommands>
    {
    public:
        vkf::Gui* gui = nullptr;

        void accept(vsg::RecordTraversal&) const override
        {
            const ImVec2 display = ImGui::GetIO().DisplaySize;
            int open = 0;                                // panels begun and not ended
            for (const auto& c : gui->commands)
            {
                switch (c.kind)
                {
                case BEGIN:
                {
                    if (c.x >= 0.0)
                        ImGui::SetNextWindowPos(ImVec2(static_cast<float>(c.x) * display.x,
                                                       static_cast<float>(1.0 - c.y) * display.y),
                                                c.follow ? ImGuiCond_Always : ImGuiCond_FirstUseEver,
                                                ImVec2(static_cast<float>(c.pivot_x), static_cast<float>(c.pivot_y)));
                    ImGuiWindowFlags flags = ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoSavedSettings |
                                             ImGuiWindowFlags_NoCollapse;
                    if (c.follow) flags |= ImGuiWindowFlags_NoMove;
                    if (c.text.rfind("##", 0) == 0) flags |= ImGuiWindowFlags_NoTitleBar;   // (no title: a callout)
                    ImGui::Begin(c.text.c_str(), nullptr, flags);
                    ++open;
                    break;
                }
                case END:
                    if (open > 0)
                    {
                        ImGui::End();
                        --open;
                    }
                    break;
                case TEXT:
                    ImGui::TextUnformatted(c.text.c_str());
                    break;
                case BUTTON:
                    if (ImGui::Button(c.text.c_str())) gui->results[c.key].clicked = true;
                    break;
                case CHECKBOX:
                {
                    bool v = c.value;
                    if (ImGui::Checkbox(c.text.c_str(), &v))
                    {
                        auto& r = gui->results[c.key];
                        r.changed = true;
                        r.value = v;
                    }
                    break;
                }
                case SLIDER:
                {
                    double v = c.number;
                    if (ImGui::SliderScalar(c.text.c_str(), ImGuiDataType_Double, &v, &c.low, &c.high, "%.4g"))
                    {
                        auto& r = gui->results[c.key];
                        r.changed = true;
                        r.number = v;
                    }
                    break;
                }
                case SEPARATOR:
                    ImGui::Separator();
                    break;
                case SAME_LINE:
                    ImGui::SameLine();
                    break;
                }
            }
            while (open-- > 0) ImGui::End();             // (panels the program didn't end)
        }
    };

    // The window's RenderImGui, with its ImGui context made current.
    class GuiNode : public vsg::Inherit<vsg::Node, GuiNode>
    {
    public:
        vsg::ref_ptr<vsgImGui::RenderImGui> render;
        ImGuiContext* context = nullptr;

        void accept(vsg::RecordTraversal& visitor) const override
        {
            ImGui::SetCurrentContext(context);
            render->accept(visitor);
        }
    };

    // The window's events to ImGui first (with its context made current).
    class GuiEvents : public vsg::Inherit<vsg::Visitor, GuiEvents>
    {
    public:
        vsg::ref_ptr<vsgImGui::SendEventsToImGui> send;
        ImGuiContext* context = nullptr;

        void apply(vsg::UIEvent& event) override
        {
            ImGui::SetCurrentContext(context);
            event.accept(*send);
        }
    };
} // namespace

vkf::Gui* vkf::gui_create(float pixel_ratio, const std::string& font, float size)
{
    auto gui = new Gui;
    gui->pixelRatio = std::max(pixel_ratio, 0.5f);
    gui->font = font;
    gui->size = size > 0.0f ? size : 15.0f;
    return gui;
}

bool vkf::gui_attached(const Gui* gui)
{
    return gui && gui->node;
}

// Make the window's ImGui context and RenderImGui (in the first render graph),
// and put its event handler first. When the window's command graph exists.
void vkf::gui_attach(Gui* gui, vsg::ref_ptr<vsg::Window> window, vsg::Viewer& viewer, vsg::CommandGraph& commandGraph)
{
    if (!gui || gui->node) return;
    ImGuiContext* previous = ImGui::GetCurrentContext();
    gui->context = ImGui::CreateContext();
    ImGui::SetCurrentContext(gui->context);
    ImGuiIO& io = ImGui::GetIO();
    io.IniFilename = nullptr;                        // (no imgui.ini in the current directory)
    ImGui::StyleColorsDark();
    ImGuiStyle& style = ImGui::GetStyle();
    style.ScaleAllSizes(gui->pixelRatio);
    style.FontSizeBase = gui->size * gui->pixelRatio;
    style.WindowRounding = 4.0f * gui->pixelRatio;
    if (!gui->font.empty()) io.Fonts->AddFontFromFileTTF(gui->font.c_str(), gui->size * gui->pixelRatio);

    auto render = vsgImGui::RenderImGui::create(window);
    auto commands = GuiCommands::create();
    commands->gui = gui;
    render->addChild(commands);
    auto node = GuiNode::create();
    node->render = render;
    node->context = gui->context;
    gui->node = node;
    for (auto& child : commandGraph.children)
        if (auto renderGraph = child.cast<vsg::RenderGraph>())
        {
            renderGraph->addChild(node);            // (after the scene and the HUD: on top)
            break;
        }

    auto events = GuiEvents::create();
    events->send = vsgImGui::SendEventsToImGui::create();
    events->context = gui->context;
    gui->events = events;
    auto& handlers = viewer.getEventHandlers();
    handlers.insert(handlers.begin(), events);
    ImGui::SetCurrentContext(previous);
}

// After a frame's draw: the commands are described again for the next one.
void vkf::gui_frame_done(Gui* gui)
{
    if (!gui) return;
    gui->commands.clear();
    gui->panel.clear();
}

// Remove the GUI from its viewer (after the device is idle) and delete it.
void vkf::gui_delete(Gui* gui, vsg::Viewer& viewer)
{
    if (!gui) return;
    auto& handlers = viewer.getEventHandlers();
    handlers.erase(std::remove(handlers.begin(), handlers.end(), gui->events), handlers.end());
    if (gui->context)
    {
        // RenderImGui's destructor shuts its Vulkan backend down and destroys
        // the current ImGui context (and ImPlot's): this window's
        ImGui::SetCurrentContext(gui->context);
        if (auto node = gui->node.cast<GuiNode>()) node->render = nullptr;
        ImGui::SetCurrentContext(nullptr);
    }
    delete gui;
}

namespace
{
    vkf::Gui* gui_of(void* handle) { return static_cast<vkf::Gui*>(handle); }

    std::string key_of(const vkf::Gui* gui, const std::string& label) { return gui->panel + "\x1f" + label; }

    void add(vkf::Gui* gui, Command c) { gui->commands.push_back(std::move(c)); }
} // namespace

extern "C" {

// A panel titled `title` ("##name": no title bar, e.g. a callout) at (x, y)
// (0 to 1 across the window, y up; x < 0: where ImGui puts it), its point
// (pivot_x, pivot_y) (0 to 1 across it, from the top left) there; `follow`:
// placed there every frame (and not movable), else only at first. The
// widgets up to vkf_gui_end are in it.
VKF_API void vkf_gui_begin(void* gui, const char* title, int title_len, double x, double y, double pivot_x,
                           double pivot_y, int follow)
{
    auto g = gui_of(gui);
    if (!g) return;
    Command c{BEGIN};
    c.text = to_string(title, title_len);
    c.x = x;
    c.y = y;
    c.pivot_x = pivot_x;
    c.pivot_y = pivot_y;
    c.follow = follow != 0;
    g->panel = c.text;
    add(g, c);
}

VKF_API void vkf_gui_end(void* gui)
{
    auto g = gui_of(gui);
    if (!g) return;
    add(g, Command{END});
    g->panel.clear();
}

VKF_API void vkf_gui_text(void* gui, const char* text, int text_len)
{
    auto g = gui_of(gui);
    if (!g) return;
    Command c{TEXT};
    c.text = to_string(text, text_len);
    add(g, c);
}

// A button: 1 if it was clicked since the last call for it (in this panel).
VKF_API int vkf_gui_button(void* gui, const char* label, int label_len)
{
    auto g = gui_of(gui);
    if (!g) return 0;
    Command c{BUTTON};
    c.text = to_string(label, label_len);
    c.key = key_of(g, c.text);
    auto& r = g->results[c.key];
    const bool clicked = r.clicked;
    r.clicked = false;
    add(g, c);
    return clicked ? 1 : 0;
}

// A checkbox showing *value: 1 (and *value set) if the user changed it since
// the last call for it.
VKF_API int vkf_gui_checkbox(void* gui, const char* label, int label_len, int* value)
{
    auto g = gui_of(gui);
    if (!g) return 0;
    Command c{CHECKBOX};
    c.text = to_string(label, label_len);
    c.key = key_of(g, c.text);
    auto& r = g->results[c.key];
    const bool changed = r.changed;
    if (changed) *value = r.value ? 1 : 0;
    r.changed = false;
    c.value = *value != 0;
    add(g, c);
    return changed ? 1 : 0;
}

// A slider showing *value from low to high: 1 (and *value set) if the user
// changed it since the last call for it.
VKF_API int vkf_gui_slider(void* gui, const char* label, int label_len, double* value, double low, double high)
{
    auto g = gui_of(gui);
    if (!g) return 0;
    Command c{SLIDER};
    c.text = to_string(label, label_len);
    c.key = key_of(g, c.text);
    auto& r = g->results[c.key];
    const bool changed = r.changed;
    if (changed) *value = r.number;
    r.changed = false;
    c.number = *value;
    c.low = low;
    c.high = high;
    add(g, c);
    return changed ? 1 : 0;
}

VKF_API void vkf_gui_separator(void* gui)
{
    if (auto g = gui_of(gui)) add(g, Command{SEPARATOR});
}

VKF_API void vkf_gui_same_line(void* gui)
{
    if (auto g = gui_of(gui)) add(g, Command{SAME_LINE});
}

} // extern "C"

#endif // VKF_EXTERNAL_LAYER
