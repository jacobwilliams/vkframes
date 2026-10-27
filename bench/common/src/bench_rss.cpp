// Peak resident memory of the process, for the benchmark apps (bench_common.f90).

#if defined(_WIN32)
extern "C" double bench_peak_rss_mb() { return -1.0; }   // (not measured on Windows yet)
#else
#include <sys/resource.h>

extern "C" double bench_peak_rss_mb()
{
    struct rusage usage;
    if (getrusage(RUSAGE_SELF, &usage) != 0) return -1.0;
#if defined(__APPLE__)
    return static_cast<double>(usage.ru_maxrss) / (1024.0 * 1024.0);   // bytes
#else
    return static_cast<double>(usage.ru_maxrss) / 1024.0;              // KB
#endif
}
#endif
