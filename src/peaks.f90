! REF: http://billauer.co.il/peakdet.html
module peaks
    use iso_fortran_env
    implicit none
    private
    public :: peak_detect

    interface peak_detect
        module procedure :: peak_detect_r32
    end interface
    
    ! peaks_implementation.f90
    interface
        module subroutine peak_detect_r32(x, delta, maxind, maxvals, minind, minvals)
            real(real32), intent(in) :: x(:)
            real(real32), intent(in) :: delta
            integer(int32), intent(out), allocatable :: maxind(:), minind(:)
            real(real32), intent(out), allocatable :: maxvals(:), minvals(:)
        end subroutine
    end interface

end module