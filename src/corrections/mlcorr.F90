module mlcorr
implicit none
contains

    !---------------------------------------------------------------------------
    ! Check whether the pipe exists
    subroutine check_fifo(pipename)
    !---------------------------------------------------------------------------
        character(len=*), intent(in) :: pipename
        logical :: file_exists

        inquire(file=pipename, exist=file_exists)

        if (.not. file_exists) then
            print '("Error: File ", a, " not found")', pipename
            stop
        end if

    end subroutine check_fifo
    !---------------------------------------------------------------------------
  
    !---------------------------------------------------------------------------
    ! Write the coordinates out
    subroutine write_coords(pipename, system_size, elements, coord, calc_grad)
    !---------------------------------------------------------------------------
        character(len=*), intent(in) :: pipename
        integer, intent(in) ::system_size
        integer, dimension(:), allocatable, intent(in) :: elements
        double precision, dimension(:,:), allocatable, intent(in) :: coord
        logical, intent(in) :: calc_grad

        integer           :: fu, rc
        integer :: i, c

        open (access='stream', &
              action='write', &
              file=pipename, &
              form='formatted', &
              iostat=rc, &
              newunit=fu, &
              status='old')

        if (rc /= 0) then
            print '("Error: Opening ", a, " failed")', pipename
            stop
        end if

        write(fu, *) system_size
        do i = 1, system_size
            write(fu, *) elements(i), coord(i,1), coord(i,2), coord(i,3)
        end do
        if (calc_grad) then
            write(fu, *) "G"
        else
            write(fu, *) "E"
        end if

        close(fu)

    end subroutine write_coords
    !---------------------------------------------------------------------------

    !---------------------------------------------------------------------------
    ! Read the results in
    subroutine get_results(pipename, system_size, energy, read_grad, grad)
    !---------------------------------------------------------------------------
        character(len=*), intent(in) :: pipename
        integer, intent(in) :: system_size
        double precision, intent(out) :: energy
        logical , intent(in) :: read_grad
        double precision, dimension(:,:), intent(out) :: grad


        integer           :: fu, rc
        character(len=200) :: buf
        integer :: line
        double precision :: x, y, z

        ! Open named pipe as formatted stream.
        open (access='stream', &
              action='read', &
              file=pipename, &
              form='formatted', &
              iostat=rc, &
              newunit=fu, &
              status='old')
        if (rc /= 0) then
            print '("Error: Opening ", a, " failed")', pipename
            stop
        end if

        line = 0

        ! Read until end of file 
        do
            read (fu, '(a)', iostat=rc) buf
            !write(*,*) ">", trim(buf)
    
            if (is_iostat_end(rc)) then
                ! End of file.
                exit
            else if (rc /= 0) then
                ! Input error.
                cycle
            end if
    

            if (line == 0) then
                ! first line is the energy
                ! convert the string to  number
                read(buf , *) energy
                if (.not. read_grad ) then
                    exit
                end if
            else
                ! remaining lines are the gradient
                read(buf , *) x, y, z
                ! they are stored only if requested
                if (read_grad) then
                    grad(line,1) = x
                    grad(line,2) = y
                    grad(line,3) = z
                end if
            end if

            line = line + 1
            if (line > system_size) then
                exit
            end if
        end do
    
        close (fu)
    end subroutine get_results
    !---------------------------------------------------------------------------

end module mlcorr

