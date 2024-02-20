program main
    use df_precision
    use mr_fortranMR
    implicit none

    type(mesa_history) :: history
    real(rk),dimension(:),allocatable :: mass
    integer :: i

    call history%new()
    call history%read_history_file("app/history_0M42_0Z02.data")
    mass = history%getr("star_mass")
    
    do i=1,10
        print*, mass(i)
    end do
    print*, size(mass,dim=1)
    print*, "last: ", mass(ubound(mass,dim=1))
    print*, "last model number: ", history%geti("model_number",history%nrows())
    

end program main
