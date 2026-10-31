!> Checks font lookup (no window): the default font (DejaVu, installed by pixi
!> in the environment's fonts directory), a missing font, an explicit path;
!> and the cache (the tests' own: `pixi run test` sets VKFRAMES_CACHE_DIR):
!> a font converted is cached, its older entries pruned (not another font's),
!> and clear_cache removes the cache's files only.
program test_text

    use vkf_text, only: find_font, default_font
    use vkframes, only: HUDText, wp, ALIGN_LEFT_BOTTOM, ALIGN_RIGHT_TOP
    use vkf_checks, only: check, report
    use vkframes, only: cache_directory, clear_cache
    use vkf_layer, only: vkf_font_read, vkf_font_delete
    use, intrinsic :: iso_c_binding, only: c_ptr, c_int, c_associated

    implicit none

    character(len=:), allocatable :: path
    type(HUDText) :: hud, plain
    integer :: rev
    logical :: exists

    path = find_font(default_font)
    write(*, '(a)') 'default font: '//path
    call check(len(path) > 0, 'the default font is found')
    inquire(file=path, exist=exists)
    call check(exists, 'the path names a file')
    call check(find_font('no-such-font.ttf') == '', 'a missing font gives ''''')
    if (len(path) > 0) call check(find_font(path) == path, 'an explicit path is used as is')

    ! HUD text settings and its change counter
    hud = HUDText('time', position=[0.5_wp, 1.0_wp], alignment=ALIGN_RIGHT_TOP, size=12.0_wp)
    call check(hud%text() == 'time' .and. all(hud%position() == [0.5_wp, 1.0_wp]) .and. &
               hud%alignment() == ALIGN_RIGHT_TOP .and. hud%size() == 12.0_wp, 'HUD text settings')
    plain = HUDText('x')
    call check(all(plain%color() == 1.0_wp) .and. plain%alignment() == ALIGN_LEFT_BOTTOM, 'HUD text defaults')
    rev = hud%revision()
    call hud%set_text('t = 1 s')
    call check(hud%revision() > rev .and. hud%text() == 't = 1 s', 'set_text counts a change')
    call hud%show(.false.)
    call check(hud%text() == '' .and. .not. hud%is_shown(), 'hidden: no text shown')

    if (len(path) > 0) call check_cache(path)

    call report('fonts, HUD text, the cache')

contains

    !> The cache, with the font at `font`: see the program's documentation.
    subroutine check_cache(font)
        character(len=*), intent(in) :: font                !! a font file (DejaVuSans.ttf)
        character(len=:), allocatable :: dir, changed, stale, other, unrelated
        logical :: exists
        dir = cache_directory()
        call check(index(dir, 'test-cache') > 0, 'the cache: the tests'' own (VKFRAMES_CACHE_DIR)')
        if (index(dir, 'test-cache') == 0) return          ! (not clearing anyone else's)
        call check(clear_cache() >= 0, 'cleared at the start')
        call read_font(font)                                 ! (converted, cached: the directory made)
        stale = dir//'/DejaVuSans-0123456789abcdef.vsgb'    ! an older entry of the font
        other = dir//'/DejaVuSans-Bold-0123456789abcdef.vsgb'   ! another font's
        unrelated = dir//'/notes.txt'                        ! not the cache's
        call write_file(stale, 'old')
        call write_file(other, 'bold')
        call write_file(unrelated, 'notes')
        ! the font changed (a byte appended): converted again, its other entries pruned
        changed = dir//'/DejaVuSans.ttf'
        call copy_changed(font, changed)
        call read_font(changed)
        inquire(file=stale, exist=exists)
        call check(.not. exists, 'the cache: a font''s older entry pruned')
        inquire(file=other, exist=exists)
        call check(exists, 'the cache: another font''s kept (DejaVuSans-Bold)')
        ! cleared: the two entries (the changed font's, the other font's), nothing else
        call check(clear_cache() == 2, 'clear_cache: the cache''s files removed')
        inquire(file=unrelated, exist=exists)
        call check(exists, 'clear_cache: other files kept')
        call delete_file(unrelated)
        call delete_file(changed)
    end subroutine check_cache

    !> Read (convert, or from the cache) the font at `file`, and let it go.
    subroutine read_font(file)
        character(len=*), intent(in) :: file                !! a font file
        type(c_ptr) :: f
        f = vkf_font_read(file, len(file, kind=c_int))
        call check(c_associated(f), 'read '//file)
        if (c_associated(f)) call vkf_font_delete(f)
    end subroutine read_font

    !> A copy of file `from` with a byte appended, at `to`.
    subroutine copy_changed(from, to)
        character(len=*), intent(in) :: from, to            !! the file, and its changed copy
        character(len=:), allocatable :: bytes
        integer :: u, n
        open(newunit=u, file=from, access='stream', form='unformatted', status='old')
        inquire(unit=u, size=n)
        allocate(character(len=n) :: bytes)
        read(u) bytes
        close(u)
        open(newunit=u, file=to, access='stream', form='unformatted', status='replace')
        write(u) bytes//char(0)
        close(u)
    end subroutine copy_changed

    !> A small file at `file` holding `text`.
    subroutine write_file(file, text)
        character(len=*), intent(in) :: file, text          !! the file, and what it holds
        integer :: u
        open(newunit=u, file=file, status='replace')
        write(u, '(a)') text
        close(u)
    end subroutine write_file

    !> Delete `file`.
    subroutine delete_file(file)
        character(len=*), intent(in) :: file                !! the file
        integer :: u
        open(newunit=u, file=file, status='old')
        close(u, status='delete')
    end subroutine delete_file

end program test_text
