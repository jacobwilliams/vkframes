"""vkframes in a PySide6 GUI.

The scene and its windows are Fortran (src/vkdemo.f90, built by fpm as a
shared library); this app is the GUI. It loads the library with ctypes and
calls its C interface (vkdemo_*): each view is a QWindow, on whose native
handle (winId) the library opens a vkframes WindowProxy. VSG renders into that
window; Qt keeps its events, which are forwarded in VSG's conventions
(framebuffer pixels, VSG mouse buttons and key symbols). Each window renders
a frame on Qt's UpdateRequest and asks for the next one, so frames follow the
display's refresh; everything runs on the GUI thread.

Run with `pixi run pyside6-demo` (from the vkframes root; see demo.py, which
builds the library and sets VKDEMO_LIB). Options:
    --data dir           OpenFrames' Demos folder (Earth and sky textures;
                         default: $OPENFRAMES_DATA)
    --view n             the main window's view (1 Earth, 2 spacecraft,
                         3 spacecraft -> Earth)
    --windows n          open n windows at startup
    --sky n              0 none, 1 stars, 2 texture, 3 both
    --trace              start with the orbit traced
    --capture file       save a frame of each window (file, file_2, ...)
    --capture-frame n    at frame n (default 120)
    --quit-after s       close after s seconds
"""

import argparse
import ctypes
import os
import sys

from PySide6.QtCore import QEvent, Qt, QTimer
from PySide6.QtGui import QWindow
from PySide6.QtWidgets import (QApplication, QCheckBox, QComboBox, QDockWidget, QDoubleSpinBox,
                               QFormLayout, QLabel, QMainWindow, QPushButton, QVBoxLayout, QWidget)

MAX_WINDOWS = 4


def load_library():
    """The vkdemo library, with its functions' C signatures."""
    path = os.environ.get("VKDEMO_LIB")
    if not path:
        sys.exit("app.py: VKDEMO_LIB is not set (run through demo.py)")
    lib = ctypes.CDLL(path)
    c_int, c_double, c_char_p = ctypes.c_int, ctypes.c_double, ctypes.c_char_p
    signatures = {
        "vkdemo_init": (c_int, [c_char_p, c_int]),
        "vkdemo_window_open": (c_int, [ctypes.c_longlong, c_int, c_int, c_double, c_int]),
        "vkdemo_window_frame": (c_int, [c_int]),
        "vkdemo_window_close": (None, [c_int]),
        "vkdemo_window_resize": (None, [c_int, c_int, c_int]),
        "vkdemo_window_mouse": (None, [c_int, c_int, c_int, c_int, c_int, c_int]),
        "vkdemo_window_scroll": (None, [c_int, c_double]),
        "vkdemo_window_key": (None, [c_int, c_int, c_int, c_int, c_int]),
        "vkdemo_window_set_view": (None, [c_int, c_int]),
        "vkdemo_window_reset_view": (None, [c_int]),
        "vkdemo_window_capture": (c_int, [c_int, c_char_p, c_int]),
        "vkdemo_num_views": (c_int, []),
        "vkdemo_view_name": (c_int, [c_int, ctypes.c_char_p, c_int]),
        "vkdemo_time": (c_double, []),
        "vkdemo_set_time_scale": (None, [c_double]),
        "vkdemo_pause_time": (None, [c_int]),
        "vkdemo_reset_time": (None, []),
        "vkdemo_set_trace": (None, [c_int]),
        "vkdemo_set_line_width": (None, [c_double]),
        "vkdemo_set_sky": (None, [c_int]),
        "vkdemo_show_axes": (None, [c_int]),
        "vkdemo_sun_light": (None, [c_int]),
        "vkdemo_show_grid": (None, [c_int]),
    }
    for name, (restype, argtypes) in signatures.items():
        f = getattr(lib, name)
        f.restype = restype
        f.argtypes = argtypes
    return lib


def view_names(lib):
    names = []
    buf = ctypes.create_string_buffer(64)
    for k in range(1, lib.vkdemo_num_views() + 1):
        n = lib.vkdemo_view_name(k, buf, len(buf))
        names.append(buf.value[:n].decode())
    return names


# VSG's conventions (vsg/ui/PointerEvent.h, KeyEvent.h)
VSG_BUTTONS = {Qt.LeftButton: 1, Qt.MiddleButton: 2, Qt.RightButton: 3}
VSG_MASKS = {Qt.LeftButton: 256, Qt.MiddleButton: 512, Qt.RightButton: 1024}
VSG_KEYS = {Qt.Key_Left: 0xFF51, Qt.Key_Up: 0xFF52, Qt.Key_Right: 0xFF53, Qt.Key_Down: 0xFF54,
            Qt.Key_Home: 0xFF50, Qt.Key_Escape: 0xFF1B, Qt.Key_Return: 0xFF0D, Qt.Key_Space: 32}
MOUSE_MOVE, MOUSE_PRESS, MOUSE_RELEASE = 1, 2, 3


def vsg_mask(buttons):
    return sum(m for b, m in VSG_MASKS.items() if buttons & b)


def vsg_key(event):
    """(symbol, modified symbol, modifier) of a key event; symbol 0 if unmapped."""
    key = event.key()
    mods = event.modifiers()
    modifier = ((1 if mods & Qt.ShiftModifier else 0) | (4 if mods & Qt.ControlModifier else 0)
                | (8 if mods & Qt.AltModifier else 0) | (128 if mods & Qt.MetaModifier else 0))
    if key in VSG_KEYS:
        symbol = VSG_KEYS[key]
    elif Qt.Key_A <= key <= Qt.Key_Z:
        symbol = ord("a") + (key - Qt.Key_A)
    else:
        text = event.text()
        symbol = ord(text) if len(text) == 1 and 32 <= ord(text) < 127 else 0
    modified = symbol
    if Qt.Key_A <= key <= Qt.Key_Z and modifier & 1:
        modified = int(key)                                  # upper case
    return symbol, modified, modifier


class VkWindow(QWindow):
    """A QWindow that vkframes renders into (window `id` of the library, once exposed)."""

    def __init__(self, app, start_view, on_frame=None):
        super().__init__()
        self.app = app
        self.lib = app.lib
        self.start_view = start_view
        self.on_frame = on_frame
        self.id = 0
        self.frames = 0

    # -- life cycle
    def exposeEvent(self, event):
        if self.id == 0 and self.isExposed():
            self.id = self.lib.vkdemo_window_open(int(self.winId()), self.width(), self.height(),
                                                  self.devicePixelRatio(), self.start_view)
            if self.id == 0:
                print("app.py: could not open a vkframes window")
                return
            self.requestUpdate()

    def close_view(self):
        """Close the vkframes window (before Qt destroys the native window)."""
        if self.id:
            self.lib.vkdemo_window_close(self.id)
            self.id = 0

    def event(self, event):
        if event.type() == QEvent.UpdateRequest and self.id:
            if self.lib.vkdemo_window_frame(self.id):
                self.frames += 1
                if self.on_frame:
                    self.on_frame(self)
                self.requestUpdate()
            return True
        return super().event(event)

    def resizeEvent(self, event):
        if self.id:
            dpr = self.devicePixelRatio()
            self.lib.vkdemo_window_resize(self.id, round(event.size().width() * dpr),
                                          round(event.size().height() * dpr))
            self.requestUpdate()

    # -- input, in framebuffer pixels
    def forward_mouse(self, kind, event):
        if not self.id:
            return
        dpr = self.devicePixelRatio()
        p = event.position()
        self.lib.vkdemo_window_mouse(self.id, kind, round(p.x() * dpr), round(p.y() * dpr),
                                     vsg_mask(event.buttons()), VSG_BUTTONS.get(event.button(), 0))
        event.accept()

    def mouseMoveEvent(self, event):
        self.forward_mouse(MOUSE_MOVE, event)

    def mousePressEvent(self, event):
        self.forward_mouse(MOUSE_PRESS, event)

    def mouseReleaseEvent(self, event):
        self.forward_mouse(MOUSE_RELEASE, event)

    def wheelEvent(self, event):
        dy = event.angleDelta().y()
        if self.id and dy:
            self.lib.vkdemo_window_scroll(self.id, 1.0 if dy > 0 else -1.0)
            event.accept()

    def forward_key(self, pressed, event):
        symbol, modified, modifier = vsg_key(event)
        if self.id and symbol:
            self.lib.vkdemo_window_key(self.id, int(pressed), symbol, modified, modifier)
            event.accept()

    def keyPressEvent(self, event):
        self.forward_key(True, event)

    def keyReleaseEvent(self, event):
        self.forward_key(False, event)


class ExtraWindow(QWidget):
    """A top-level window with its own view of the scene."""

    def __init__(self, app, index, start_view):
        super().__init__()
        self.setWindowTitle(f"vkframes {index}")
        self.index = index
        self.view = VkWindow(app, start_view, app.after_frame)
        self.view.index = index
        layout = QVBoxLayout(self)
        self.choice = QComboBox()
        self.choice.addItems(app.views)
        self.choice.setCurrentIndex(start_view - 1)
        self.choice.currentIndexChanged.connect(lambda i: app.lib.vkdemo_window_set_view(self.view.id, i + 1))
        layout.addWidget(self.choice)
        container = QWidget.createWindowContainer(self.view)
        container.setMinimumSize(320, 240)
        layout.addWidget(container, 1)
        self.resize(640, 480)

    def closeEvent(self, event):
        self.view.close_view()
        event.accept()


class MainWindow(QMainWindow):
    def __init__(self, app, options):
        super().__init__()
        self.app = app
        lib = app.lib
        self.setWindowTitle("vkframes in PySide6")
        self.view = VkWindow(app, options.view, app.after_frame)
        self.view.index = 1
        container = QWidget.createWindowContainer(self.view)
        container.setMinimumSize(320, 240)
        self.setCentralWidget(container)

        panel = QWidget()
        form = QFormLayout(panel)
        self.view_choice = QComboBox()
        self.view_choice.addItems(app.views)
        self.view_choice.setCurrentIndex(options.view - 1)
        self.view_choice.currentIndexChanged.connect(lambda i: lib.vkdemo_window_set_view(self.view.id, i + 1))
        form.addRow("View", self.view_choice)
        reset_view = QPushButton("Reset view")
        reset_view.clicked.connect(lambda: lib.vkdemo_window_reset_view(self.view.id))
        form.addRow("", reset_view)
        new_window = QPushButton("New window")
        new_window.clicked.connect(app.new_window)
        form.addRow("", new_window)

        time_scale = QDoubleSpinBox()
        time_scale.setRange(0.0, 5000.0)
        time_scale.setValue(200.0)
        time_scale.setSuffix(" x")
        time_scale.valueChanged.connect(lib.vkdemo_set_time_scale)
        form.addRow("Time scale", time_scale)
        pause = QCheckBox("Pause time")
        pause.toggled.connect(lambda checked: lib.vkdemo_pause_time(int(checked)))
        form.addRow("", pause)
        reset_time = QPushButton("Reset time")
        reset_time.clicked.connect(lib.vkdemo_reset_time)
        form.addRow("", reset_time)

        self.trace = QCheckBox("Trace orbit")
        self.trace.toggled.connect(lambda checked: lib.vkdemo_set_trace(int(checked)))
        form.addRow("", self.trace)
        width = QDoubleSpinBox()
        width.setRange(0.5, 20.0)
        width.setValue(2.0)
        width.setSuffix(" pt")
        width.valueChanged.connect(lib.vkdemo_set_line_width)
        form.addRow("Orbit width", width)
        self.sky = QComboBox()
        self.sky.addItems(["None", "Stars", "Texture", "Texture and stars"])
        self.sky.currentIndexChanged.connect(lib.vkdemo_set_sky)
        form.addRow("Sky", self.sky)
        sun = QCheckBox("Sun light (day and night)")
        sun.setChecked(True)
        sun.toggled.connect(lambda checked: lib.vkdemo_sun_light(int(checked)))
        form.addRow("", sun)
        grid = QCheckBox("Lat/lon grid")
        grid.toggled.connect(lambda checked: lib.vkdemo_show_grid(int(checked)))
        form.addRow("", grid)
        axes = QCheckBox("Show axes and labels")
        axes.setChecked(True)
        axes.toggled.connect(lambda checked: lib.vkdemo_show_axes(int(checked)))
        form.addRow("", axes)
        self.time_label = QLabel("Simulation time:")
        form.addRow(self.time_label)
        form.addRow(QLabel("<small>Left drag: rotate<br>Right drag: zoom<br>Middle drag: pan<br>"
                           "Wheel: zoom</small>"))

        dock = QDockWidget("Controls", self)
        dock.setWidget(panel)
        self.addDockWidget(Qt.LeftDockWidgetArea, dock)
        self.sky.setCurrentIndex(options.sky)
        self.trace.setChecked(options.trace)

    def closeEvent(self, event):
        for w in list(self.app.extra):
            w.close()
        self.view.close_view()
        event.accept()


class DemoApp:
    def __init__(self, options):
        self.options = options
        self.lib = load_library()
        data = (options.data or "").encode()
        if self.lib.vkdemo_init(data, len(data)) != 0:
            print("app.py: the scene had problems (see above)")
        self.views = view_names(self.lib)
        self.extra = []
        self.main = MainWindow(self, options)

    def new_window(self):
        if len(self.extra) + 1 >= MAX_WINDOWS:
            print(f"app.py: at most {MAX_WINDOWS} windows")
            return
        index = len(self.extra) + 2
        w = ExtraWindow(self, index, (index - 1) % len(self.views) + 1)
        self.extra.append(w)
        w.destroyed.connect(lambda: self.extra.remove(w) if w in self.extra else None)
        w.show()

    def after_frame(self, window):
        """After each frame: the time label (main window) and --capture."""
        if window.index == 1:
            self.main.time_label.setText(f"Simulation time: {self.lib.vkdemo_time():10.1f} s")
        o = self.options
        if o.capture and window.frames == o.capture_frame:
            root, ext = os.path.splitext(o.capture)
            file = o.capture if window.index == 1 else f"{root}_{window.index}{ext}"
            name = file.encode()
            if self.lib.vkdemo_window_capture(window.id, name, len(name)) == 0:
                print("captured " + file)

    def quit(self):
        for w in list(self.extra):
            w.close()
        self.main.close()


def main():
    parser = argparse.ArgumentParser(description="vkframes in a PySide6 GUI")
    parser.add_argument("--data", default=os.environ.get("OPENFRAMES_DATA", ""))
    parser.add_argument("--view", type=int, default=1)
    parser.add_argument("--windows", type=int, default=1)
    parser.add_argument("--sky", type=int, default=1, choices=range(4))
    parser.add_argument("--trace", action="store_true")
    parser.add_argument("--capture")
    parser.add_argument("--capture-frame", type=int, default=120)
    parser.add_argument("--quit-after", type=float, default=-1.0)
    options = parser.parse_args()

    qapp = QApplication(sys.argv)
    demo = DemoApp(options)
    demo.main.resize(1100, 700)
    demo.main.show()
    for _ in range(1, min(options.windows, MAX_WINDOWS)):
        demo.new_window()
    if options.quit_after > 0:
        QTimer.singleShot(int(1000 * options.quit_after), demo.quit)
    sys.exit(qapp.exec())


if __name__ == "__main__":
    main()
