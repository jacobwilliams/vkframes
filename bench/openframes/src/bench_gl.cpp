// The default OpenGL surface format for the OpenFrames benchmark app: a depth
// buffer, and the swap interval (0: no vsync). qt-fortran has no binding for
// QSurfaceFormat::setSwapInterval yet. Call before creating the QApplication.

#include <QSurfaceFormat>

extern "C" void bench_default_surface_format(int depth_bits, int swap_interval)
{
    QSurfaceFormat format = QSurfaceFormat::defaultFormat();
    format.setDepthBufferSize(depth_bits);
    format.setSwapInterval(swap_interval);
    QSurfaceFormat::setDefaultFormat(format);
}
