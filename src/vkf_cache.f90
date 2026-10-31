!> The cache: what is slow to make again at every launch, kept between runs
!> (src/vsg/vkf_cache.h): fonts converted for labels and HUD text (their
!> glyphs' signed distance fields: seconds each; in its `fonts` directory)
!> and shaders compiled from GLSL (about 45 ms each; in `shaders`). Fonts are named for their contents, so copies of
!> one font share an entry, and a font's older entries are removed when it is
!> converted again (after the font or VSG changed); shaders are named for
!> their source and settings.
!>
!> The directory is VKFRAMES_CACHE_DIR if set, else the user's cache
!> directory (macOS: ~/Library/Caches/vkframes; Linux: $XDG_CACHE_HOME or
!> ~/.cache, then vkframes; Windows: %LOCALAPPDATA%/vkframes/cache), created
!> (with its parents) if it doesn't exist; without one, nothing is cached
!> (all is made at each launch). `cache_directory`
!> says which; `clear_cache` deletes the cache's files, made again when next
!> needed.
module vkf_cache

    use, intrinsic :: iso_c_binding, only: c_int, c_char
    use vkf_layer, only: vkf_cache_directory, vkf_cache_clear

    implicit none

    private

    public :: cache_directory, clear_cache

contains

    !> The cache directory, created if it doesn't exist yet (e.g. a
    !> VKFRAMES_CACHE_DIR not made yet); '' if there is none or it can't be
    !> created: nothing is cached.
    function cache_directory() result(dir)
        character(len=:), allocatable :: dir
        character(kind=c_char, len=4096) :: buf
        integer(c_int) :: n
        n = vkf_cache_directory(buf, len(buf, kind=c_int))
        dir = ''
        if (n > 0) dir = buf(1:n)
    end function cache_directory

    !> Delete the cache's files (converted fonts, compiled shaders; nothing
    !> else in the directory) and its fonts and shaders directories if left
    !> empty (the cache directory itself is kept): the number of files
    !> removed, or -1 if some couldn't be. They are made again when next
    !> needed.
    integer function clear_cache() result(removed)
        removed = int(vkf_cache_clear())
    end function clear_cache

end module vkf_cache
