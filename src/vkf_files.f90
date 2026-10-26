!> Finding files: data files (e.g. the star catalog data/Stars_HYGv3.txt) by
!> name on a search path, as OpenFrames' osgDB::findDataFile.
module vkf_files

    implicit none

    private

    public :: find_in_dirs, find_data_file

contains

    !> The path dir/name of the first directory in `dirs` (separated by ';' or
    !> ':', except a ':' after a drive letter) that has `name`, or ''.
    function find_in_dirs(name, dirs) result(path)
        character(len=*), intent(in) :: name, dirs
        character(len=:), allocatable :: path
        integer :: i, j
        logical :: exists
        path = ''
        i = 1
        do while (i <= len(dirs))
            j = i
            do while (j <= len(dirs))
                if (dirs(j:j) == ';' .or. (dirs(j:j) == ':' .and. j > i + 1)) exit   ! ':' not after a drive letter
                j = j + 1
            end do
            if (j > i) then
                inquire(file=dirs(i:j-1)//'/'//name, exist=exists)
                if (exists) then
                    path = dirs(i:j-1)//'/'//name
                    return
                end if
            end if
            i = j + 1
        end do
    end function find_in_dirs

    !> The path of data file `name`: `name` itself if it exists (a path, or a
    !> file in the current directory), else the first match in the directories
    !> of the VKFRAMES_DATA_PATH environment variable; '' if not found.
    function find_data_file(name) result(path)
        character(len=*), intent(in) :: name
        character(len=:), allocatable :: path
        character(len=4096) :: env
        integer :: status
        logical :: exists
        path = ''
        if (len_trim(name) == 0) return
        inquire(file=trim(name), exist=exists)
        if (exists) then
            path = trim(name)
            return
        end if
        call get_environment_variable('VKFRAMES_DATA_PATH', env, status=status)
        if (status == 0) path = find_in_dirs(trim(name), trim(env))
    end function find_data_file

end module vkf_files
