module mr_fortranMR
    use df_precision
    use df_types
    use df_utils
    use split_mod
    use df_fortranDF
    implicit none
    private

    public mesa_history

    character(len=*),parameter :: rfmt = "(1pes40.16e3, 1x)", ifmt = "(i40, 1x)"

  
    type,extends(data_frame) :: mesa_history
        private

    contains
        private

        procedure,public :: read_history_file
    end type mesa_history


contains

    subroutine read_history_file(this,filename,read_info)
        class(mesa_history),intent(inout) :: this
        character(len=*),intent(in) :: filename
        logical,intent(in),optional :: read_info

        logical :: skip
        integer :: num_lines, num_cols, line_len, dtype, line_ind, header_ind, offset
        character(len=:),allocatable :: line
        character(len=:),dimension(:),allocatable :: headers, split_line
        real(rk) :: rval
        integer(ik) :: ival

        integer :: io_unit, io_err
        character(len=:),allocatable :: err_msg

        integer :: i

        header_ind = 6

        if (present(read_info)) then
            skip = read_info
        else
            skip = .false.
        end if

        open(newunit=io_unit,file=trim(adjustl(filename)),status="old",action="read",iostat=io_err)
        if (io_err /= 0) then
            err_msg = err_msg_io_read//" "//trim(adjustl(filename))
            error stop err_msg
        end if

        call get_num_lines(io_unit,num_lines)

        if (skip) then
            print*, "no support for reading history info yet"
            print*, "skipping to start of data"
            call skip_to_line(io_unit,header_ind)
        else
            call skip_to_line(io_unit,header_ind)
        end if

        call get_num_cols(io_unit,num_cols,line_len)
        call skip_to_line(io_unit,header_ind)

        allocate(character(len=line_len+100) :: line)
        read(unit=io_unit,fmt='(a)',iostat=io_err) line
        if (io_err /= 0) then
            err_msg = err_msg_io_read//" "//trim(adjustl(filename))
            error stop err_msg
        end if
        split_line = split(line," ")


        headers = split_line
        call fix_duplicate_headers(headers) ! for binary evolutions, some columns are dupes


        read(unit=io_unit,fmt='(a)',iostat=io_err) line
        if (io_err /= 0) then
            err_msg = err_msg_io_read//" "//trim(adjustl(filename))
            error stop err_msg
        end if
        split_line = split(line," ")
        do i=1,num_cols
            dtype = what_type(split_line(i))
            select case (dtype)
                case (REAL_NUM)
                    call this%append_emptyr(num_lines-header_ind,trim(adjustl(headers(i))))
                    read(split_line(i),fmt=*) rval
                    call this%setr(i,1,rval)
                case (INTEGER_NUM)
                    call this%append_emptyi(num_lines-header_ind,trim(adjustl(headers(i))))
                    read(split_line(i),fmt=*) ival
                    call this%seti(i,1,ival)
                case default
                    ! only real and integers should appear in mesa history cols
                    error stop "invalid type"
            end select
        end do
        line_ind = 2 + header_ind ! 2 lines have been read
        offset = -header_ind

        do while(line_ind <= num_lines)
            do i=1,num_cols
                select case (this%dtype(i))
                case (REAL_NUM)
                    read(unit=io_unit,fmt=rfmt,iostat=io_err,advance='no') rval
                    if (io_err /= 0) then
                        err_msg = err_msg_io_read//" "//trim(adjustl(filename))
                        error stop err_msg
                    end if
                    call this%setr(i,line_ind+offset,rval)
                case (INTEGER_NUM)
                    read(unit=io_unit,fmt=ifmt,iostat=io_err,advance='no') ival
                    if (io_err /= 0) then
                        err_msg = err_msg_io_read//" "//trim(adjustl(filename))
                        error stop err_msg
                    end if
                    call this%seti(i,line_ind+offset,ival)
                case default
                    ! only real and integers should appear in mesa history cols
                    error stop 'invalid type'
                end select
                if (i==num_cols) read(unit=io_unit,fmt='(a)')
            end do
            line_ind = line_ind + 1
            if (mod(line_ind,1000) == 0) print*, line_ind
        end do

        close(io_unit)

    end subroutine read_history_file

    subroutine skip_to_line(io_unit,line_num,current_line_num)
        integer,intent(in) :: io_unit, line_num
        integer,intent(in),optional :: current_line_num
 
        character(len=1) :: null
        integer :: io_err, num_skips, i

        if (present(current_line_num)) then
            num_skips = line_num - current_line_num
        else
            num_skips = line_num - 1
        end if

        do i=1,num_skips
            read(io_unit,fmt='(a)')
        end do

    end subroutine skip_to_line

    subroutine get_num_cols(unit,num_cols,line_len)
        integer,intent(in) :: unit
        integer,intent(out) :: num_cols
        integer,intent(out) :: line_len

        character(len=:),allocatable :: line
        character(len=:),dimension(:),allocatable :: line_split

        call get_len_line(unit,line_len,1000,line)
        line_split = split(line)
        num_cols = size(line_split,dim=1)

        rewind(unit)

    end subroutine get_num_cols

    subroutine fix_duplicate_headers(headers)
        character(len=*),dimension(:),intent(inout) :: headers

        character(len=:),allocatable :: new_header
        character(len=:),dimension(:),allocatable :: tmp
        integer :: i, j, headers_len, num_headers

        headers_len = len(headers)
        num_headers = size(headers,dim=1)

        do i=2,num_headers
            if (findloc(headers(1:i-1),headers(i),dim=1) > 0) then
                new_header = trim(adjustl(headers(i)))//"_b"
                if (len(new_header) > headers_len) then
                    allocate(character(len(new_header)) :: tmp(num_headers))
                    do j=1,num_headers
                        tmp(j) = headers(j)
                    end do
                    tmp(i) = new_header
                    headers = tmp
                    deallocate(tmp)
                else
                    headers(i) = new_header
                end if
            end if
        end do

    end subroutine fix_duplicate_headers



end module mr_fortranMR
