!> Checks video recording (in a window of its own, with ffmpeg from the pixi
!> environment): the frames drawn are written to the file (ffprobe counts
!> them, at an even size), the simulation time advances by exactly
!> scale/fps per frame while recording, and goes on from there after; a
!> second start fails while recording; a watermark is drawn (a missing one
!> refused); an ffmpeg that fails (an unknown
!> encoder) ends the recording, and stopping says it failed.
program test_record

    use, intrinsic :: iso_fortran_env, only: int64
    use vkframes

    implicit none

    character(len=*), parameter :: file = 'build/test_record.mp4', probe = 'build/test_record.txt'
    integer, parameter :: n_frames = 30
    real(wp), parameter :: fps = 30.0_wp
    type(Sphere), target :: body
    type(FrameManager), target :: scene
    type(WindowProxy) :: window
    integer(int64) :: written
    integer :: failures, k, unit, status, counted, width, height
    logical :: ok

    failures = 0
    body = Sphere('Body', radius=1.0_wp, color=[0.3_wp, 0.6_wp, 1.0_wp])
    scene = FrameManager(body)
    if (.not. window%open(320, 240, 'vkframes: recording test')) then
        write(*, '(a)') 'FAIL: could not open a window'
        error stop 1
    end if
    call window%set_scene(scene)
    ok = window%frame()

    ! recording: the time steps exactly scale/fps per frame
    open(newunit=unit, file=file, status='replace', iostat=status)          ! (no video from before)
    if (status == 0) close(unit, status='delete')
    call check(window%start_recording(file, fps=fps), 'recording started')
    call check(window%is_recording(), 'is_recording')
    call check(.not. window%start_recording(file), 'a second start fails while recording')
    call window%set_time_scale(2.0_wp)
    call window%set_time(0.0_wp)
    do k = 1, n_frames
        ok = window%frame()
    end do
    call check(abs(window%time() - 2.0_wp * (n_frames - 1) / fps) < 1.0e-6_wp, &
               'the time steps scale/fps per frame')   ! (wall times near 1e9 s: 1e-7 s apart)
    call check(window%recorded_frames() == n_frames, 'recorded_frames')
    call check(window%stop_recording(written), 'recording stopped')
    call check(written == n_frames, 'all frames written')
    call check(.not. window%is_recording(), 'not recording after stop')
    ok = window%frame()
    call check(window%time() >= 2.0_wp * (n_frames - 1) / fps .and. window%time() < 2.0_wp * n_frames / fps + 1.0_wp, &
               'after: the time goes on from the video''s')

    ! the file: the frames and an even size
    call execute_command_line('ffprobe -v error -count_frames -select_streams v:0 -show_entries '// &
                              'stream=nb_read_frames,width,height -of csv=p=0 '//file//' > '//probe, exitstat=status)
    call check(status == 0, 'ffprobe reads the video')
    counted = -1
    width = -1
    height = -1
    open(newunit=unit, file=probe, status='old', action='read', iostat=status)
    if (status == 0) then
        read(unit, *, iostat=status) width, height, counted
        close(unit)
    end if
    call check(counted == n_frames, 'the video holds the frames')
    call check(width > 0 .and. height > 0 .and. mod(width, 2) == 0 .and. mod(height, 2) == 0, 'an even size')

    ! a watermark (drawn by ffmpeg); a missing one refused
    call check(window%start_recording('build/test_record_logo.mp4', watermark='media/logo/vkframes-logo-dark.svg', &
                                      watermark_width=0.3_wp, watermark_opacity=0.5_wp), 'recording with a watermark')
    do k = 1, 5
        ok = window%frame()
    end do
    call check(window%stop_recording(written) .and. written == 5, 'a watermarked video written')
    call check(.not. window%start_recording('build/test_record_none.mp4', watermark='no/such/logo.png'), &
               'a missing watermark refused')
    call check(.not. window%is_recording(), 'not recording then')

    ! ffmpeg failing: the recording ends
    call check(window%start_recording('build/test_record_bad.mp4', codec='no_such_encoder'), 'started (ffmpeg not yet)')
    do k = 1, 5
        ok = window%frame()
    end do
    call check(.not. window%is_recording(), 'a failed ffmpeg ends the recording')
    call check(.not. window%stop_recording(), 'stopping then: not recording')

    call window%close()
    if (failures > 0) error stop 1
    write(*, '(a)') 'PASS: video recording (frames piped to ffmpeg, the time at the video''s pace)'

contains

    subroutine check(ok, what)
        logical, intent(in) :: ok
        character(len=*), intent(in) :: what
        if (.not. ok) then
            write(*, '(a)') 'FAIL: '//what
            failures = failures + 1
        end if
    end subroutine check

end program test_record
