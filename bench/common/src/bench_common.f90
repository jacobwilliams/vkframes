!> Shared by the vkframes and OpenFrames benchmark apps (bench/vkframes,
!> bench/openframes), so that both build the same data, run the same scenarios
!> and report the same measurements:
!>
!> * the options (`parse_options`): the scenario, the number of points, ...
!> * the trajectory (`make_orbit`): a precessing orbit, ~1000 points per
!>   revolution;
!> * timing (`now`, `cpu_seconds`) and the process's peak memory (`peak_rss_mb`);
!> * the frame times (`FrameLog`, `frame_stats`) and the appends' times
!>   (`AppendLog`);
!> * the result (`write_result`): one JSON line on stdout, also appended to the
!>   `--out` file.
!>
!> The scenarios (see bench/README.md):
!>
!> * `static`: a trajectory of `--points` points, drawn whole; time runs
!>   (a body follows the trajectory); nothing changes.
!> * `grow`: the same, and `--rate` points per second appended during the
!>   measurement (from a timer at 60 Hz, as a simulation would add them).
!> * `trace`: the trajectory drawn up to the current time (trace mode), time
!>   sweeping over the whole trajectory during the measurement.
module bench_common

    use, intrinsic :: iso_fortran_env, only: real64, int64, output_unit
    use, intrinsic :: iso_c_binding, only: c_double

    implicit none

    private

    integer, parameter, public :: dp = real64
    real(dp), parameter :: pi = acos(-1.0_dp)
    real(dp), parameter, public :: earth_radius = 6378.137_dp      !! km
    integer, parameter, public :: points_per_rev = 1000
    real(dp), parameter, public :: append_interval = 1.0_dp / 60.0_dp   !! s between appends (grow)
    real(dp), parameter, public :: view_distance = 6.0_dp * earth_radius   !! the camera's (km; the orbit fills most of the view)

    public :: BenchOptions, parse_options, make_orbit, orbit_size, now, cpu_seconds, peak_rss_mb
    public :: FrameLog, AppendLog, frame_stats, write_result, total_points

    !> What to run (the command-line options; defaults in brackets).
    type :: BenchOptions
        character(len=:), allocatable :: scenario          !! static, grow or trace [static]
        integer :: points = 1000000                         !! points before the measurement [1e6]
        real(dp) :: rate = 60000.0_dp                       !! points appended per second (grow) [6e4]
        real(dp) :: warmup = 2.0_dp                         !! s before measuring [2]
        real(dp) :: seconds = 10.0_dp                       !! s measured [10]
        logical :: per_point = .false.                      !! vkframes: add points one at a time [bulk]
        integer :: width = 1280, height = 800               !! the view's size (logical pixels)
        character(len=:), allocatable :: out                !! results file (JSON lines) [none]
        character(len=:), allocatable :: capture            !! an image of the last frame [none]
    end type BenchOptions

    !> Times at which frames were finished (s, from `now`). Preallocated, so a
    !> render thread can record into it without allocating.
    type :: FrameLog
        real(dp), allocatable :: stamps(:)
        integer :: n = 0
    contains
        procedure :: init => frame_log_init, record => frame_log_record
    end type FrameLog

    !> The appends' times (grow).
    type :: AppendLog
        integer :: calls = 0
        integer :: points = 0
        real(dp) :: total = 0.0_dp, worst = 0.0_dp         !! s
    contains
        procedure :: add => append_log_add
    end type AppendLog

    interface
        !> Peak resident memory of the process (MB; < 0 if unknown). In bench_rss.cpp.
        function bench_peak_rss_mb() result(mb) bind(C, name="bench_peak_rss_mb")
            import :: c_double
            implicit none
            real(c_double) :: mb
        end function bench_peak_rss_mb
    end interface

contains

    !> The options, from the command line (unknown arguments are left for the app).
    subroutine parse_options(opt)
        type(BenchOptions), intent(out) :: opt
        character(len=1024) :: arg
        integer :: i
        opt%scenario = 'static'
        i = 1
        do while (i <= command_argument_count())
            call get_command_argument(i, arg)
            select case (arg)
            case ('--scenario')
                call next_arg(i, arg)
                opt%scenario = trim(arg)
            case ('--points')
                call next_arg(i, arg)
                read(arg, *) opt%points
            case ('--rate')
                call next_arg(i, arg)
                read(arg, *) opt%rate
            case ('--warmup')
                call next_arg(i, arg)
                read(arg, *) opt%warmup
            case ('--seconds')
                call next_arg(i, arg)
                read(arg, *) opt%seconds
            case ('--per-point')
                opt%per_point = .true.
            case ('--size')
                call next_arg(i, arg)
                read(arg, *) opt%width
                call next_arg(i, arg)
                read(arg, *) opt%height
            case ('--out')
                call next_arg(i, arg)
                opt%out = trim(arg)
            case ('--capture')
                call next_arg(i, arg)
                opt%capture = trim(arg)
            end select
            i = i + 1
        end do
        if (all(opt%scenario /= [character(len=6) :: 'static', 'grow', 'trace'])) &
            error stop 'bench: --scenario must be static, grow or trace'
        opt%points = max(2, opt%points)
    end subroutine parse_options

    subroutine next_arg(i, arg)
        integer, intent(inout) :: i
        character(len=*), intent(out) :: arg
        i = i + 1
        call get_command_argument(i, arg)
    end subroutine next_arg

    !> Points needed: the initial ones, and (grow) those appended during the
    !> warmup and the measurement, with a margin.
    integer function total_points(opt) result(n)
        type(BenchOptions), intent(in) :: opt
        n = opt%points
        if (opt%scenario == 'grow') n = n + ceiling(opt%rate * (opt%seconds + 1.0_dp)) + 1000
    end function total_points

    !> n points of a precessing, slightly eccentric orbit (km), points_per_rev
    !> per revolution, one revolution every 100 s: times t(n) and positions
    !> pos(3, n).
    pure subroutine make_orbit(n, t, pos)
        integer, intent(in) :: n
        real(dp), intent(out) :: t(n), pos(3, n)
        real(dp), parameter :: period = 100.0_dp, inc = 50.0_dp * pi / 180.0_dp
        real(dp) :: u, node, r
        integer :: i
        do i = 1, n
            t(i) = period * real(i - 1, dp) / points_per_rev
            u = 2.0_dp * pi * t(i) / period                   ! argument of latitude
            node = 0.01_dp * u                                ! the orbit plane turns slowly
            r = orbit_size() * (1.0_dp + 0.05_dp * sin(0.37_dp * u))
            pos(1, i) = r * (cos(node) * cos(u) - sin(node) * sin(u) * cos(inc))
            pos(2, i) = r * (sin(node) * cos(u) + cos(node) * sin(u) * cos(inc))
            pos(3, i) = r * sin(u) * sin(inc)
        end do
    end subroutine make_orbit

    !> The orbit's mean radius (km).
    pure real(dp) function orbit_size()
        orbit_size = 2.0_dp * earth_radius
    end function orbit_size

    !> Wall-clock time (s), from an arbitrary origin.
    real(dp) function now()
        integer(int64) :: count, rate
        call system_clock(count, rate)
        now = real(count, dp) / real(rate, dp)
    end function now

    !> CPU time used by the process so far (s; all threads).
    real(dp) function cpu_seconds()
        call cpu_time(cpu_seconds)
    end function cpu_seconds

    real(dp) function peak_rss_mb()
        peak_rss_mb = bench_peak_rss_mb()
    end function peak_rss_mb

    subroutine frame_log_init(self, capacity)
        class(FrameLog), intent(inout) :: self
        integer, intent(in) :: capacity
        allocate(self%stamps(capacity))
        self%n = 0
    end subroutine frame_log_init

    !> A frame finished at time t (dropped when full).
    subroutine frame_log_record(self, t)
        class(FrameLog), intent(inout) :: self
        real(dp), intent(in) :: t
        if (self%n >= size(self%stamps)) return
        self%stamps(self%n + 1) = t
        self%n = self%n + 1
    end subroutine frame_log_record

    subroutine append_log_add(self, seconds, points)
        class(AppendLog), intent(inout) :: self
        real(dp), intent(in) :: seconds
        integer, intent(in) :: points
        self%calls = self%calls + 1
        self%points = self%points + points
        self%total = self%total + seconds
        self%worst = max(self%worst, seconds)
    end subroutine append_log_add

    !> Statistics of the frames finished between t0 and t1: their number, the
    !> frame rate, and the frame times (ms: mean, median, 95th and 99th
    !> percentiles, maximum).
    subroutine frame_stats(log, t0, t1, frames, fps, mean, median, p95, p99, worst)
        type(FrameLog), intent(in) :: log
        real(dp), intent(in) :: t0, t1
        integer, intent(out) :: frames
        real(dp), intent(out) :: fps, mean, median, p95, p99, worst
        real(dp), allocatable :: s(:), dt(:)
        integer :: n
        n = log%n
        s = pack(log%stamps(1:n), log%stamps(1:n) >= t0 .and. log%stamps(1:n) <= t1)
        frames = size(s)
        fps = 0.0_dp; mean = 0.0_dp; median = 0.0_dp; p95 = 0.0_dp; p99 = 0.0_dp; worst = 0.0_dp
        if (frames < 2) return
        fps = real(frames - 1, dp) / (s(frames) - s(1))
        dt = 1000.0_dp * (s(2:) - s(:frames-1))
        call sort(dt)
        mean = sum(dt) / size(dt)
        median = percentile(dt, 0.5_dp)
        p95 = percentile(dt, 0.95_dp)
        p99 = percentile(dt, 0.99_dp)
        worst = dt(size(dt))
    end subroutine frame_stats

    pure real(dp) function percentile(sorted, q)
        real(dp), intent(in) :: sorted(:), q
        percentile = sorted(max(1, min(size(sorted), ceiling(q * size(sorted)))))
    end function percentile

    !> Sort ascending (heapsort: no recursion, no extra memory).
    pure subroutine sort(a)
        real(dp), intent(inout) :: a(:)
        integer :: n, i
        real(dp) :: tmp
        n = size(a)
        do i = n / 2, 1, -1
            call sift(a, i, n)
        end do
        do i = n, 2, -1
            tmp = a(1); a(1) = a(i); a(i) = tmp
            call sift(a, 1, i - 1)
        end do
    end subroutine sort

    pure subroutine sift(a, start, last)
        real(dp), intent(inout) :: a(:)
        integer, intent(in) :: start, last
        integer :: root, child
        real(dp) :: tmp
        root = start
        do while (2 * root <= last)
            child = 2 * root
            if (child < last) then
                if (a(child) < a(child + 1)) child = child + 1
            end if
            if (a(root) >= a(child)) return
            tmp = a(root); a(root) = a(child); a(child) = tmp
            root = child
        end do
    end subroutine sift

    !> The result as one JSON line: on stdout, and appended to opt%out.
    subroutine write_result(opt, library, renderer, insert_method, insert_s, first_frame_s, log, t0, t1, appends, &
                            cpu_s, rss_load_mb)
        type(BenchOptions), intent(in) :: opt
        character(len=*), intent(in) :: library, renderer, insert_method
        real(dp), intent(in) :: insert_s, first_frame_s                 !! s
        type(FrameLog), intent(in) :: log
        real(dp), intent(in) :: t0, t1                                  !! the measurement (s, from `now`)
        type(AppendLog), intent(in) :: appends
        real(dp), intent(in) :: cpu_s                                   !! CPU time during the measurement (s)
        real(dp), intent(in) :: rss_load_mb                             !! peak memory after loading the points
        character(len=:), allocatable :: line
        real(dp) :: fps, mean, median, p95, p99, worst, append_mean
        integer :: frames, unit
        call frame_stats(log, t0, t1, frames, fps, mean, median, p95, p99, worst)
        append_mean = 0.0_dp
        if (appends%calls > 0) append_mean = 1000.0_dp * appends%total / appends%calls
        line = '{' // str('library', library) // ',' // str('renderer', renderer) // ',' // &
               str('scenario', opt%scenario) // ',' // int_field('points', opt%points) // ',' // &
               num('rate', merge(opt%rate, 0.0_dp, opt%scenario == 'grow')) // ',' // &
               str('insert_method', insert_method) // ',' // num('insert_s', insert_s) // ',' // &
               num('first_frame_s', first_frame_s) // ',' // num('seconds', t1 - t0) // ',' // &
               int_field('frames', frames) // ',' // num('fps', fps) // ',' // &
               num('frame_ms_mean', mean) // ',' // num('frame_ms_median', median) // ',' // &
               num('frame_ms_p95', p95) // ',' // num('frame_ms_p99', p99) // ',' // num('frame_ms_max', worst) // ',' // &
               int_field('appended', appends%points) // ',' // num('append_ms_mean', append_mean) // ',' // &
               num('append_ms_max', 1000.0_dp * appends%worst) // ',' // &
               num('cpu_per_wall', cpu_s / max(1.0e-9_dp, t1 - t0)) // ',' // &
               num('rss_mb_load', rss_load_mb) // ',' // num('rss_mb_peak', peak_rss_mb()) // '}'
        write(output_unit, '(a)') 'RESULT '//line
        flush(output_unit)
        if (allocated(opt%out)) then
            open(newunit=unit, file=opt%out, position='append', action='write', status='unknown')
            write(unit, '(a)') line
            close(unit)
        end if
    end subroutine write_result

    function str(key, value) result(s)
        character(len=*), intent(in) :: key, value
        character(len=:), allocatable :: s
        s = '"'//key//'": "'//value//'"'
    end function str

    function num(key, value) result(s)
        character(len=*), intent(in) :: key
        real(dp), intent(in) :: value
        character(len=:), allocatable :: s
        character(len=32) :: buf
        write(buf, '(es15.7)') value
        s = '"'//key//'": '//trim(adjustl(buf))
    end function num

    function int_field(key, value) result(s)
        character(len=*), intent(in) :: key
        integer, intent(in) :: value
        character(len=:), allocatable :: s
        character(len=16) :: buf
        write(buf, '(i0)') value
        s = '"'//key//'": '//trim(buf)
    end function int_field

end module bench_common
