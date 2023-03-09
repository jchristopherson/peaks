!> @mainpage
!!
!! @section intro_sec Introduction
!! PEAKS is a peak detection library meant to locate peaks and valleys in a 
!! signal.  This library works on both smooth data and on noisy data where other
!! routines, especially those that rely upon derivatives, have difficulties.
!! This library contains routines that are a re-implementation of the peak
!! detection routine presented by Eli Billauer, which can be found 
!! [here](http://billauer.co.il/peakdet.html).
module peaks
    use iso_fortran_env
    implicit none
    private
    public :: peak_detect

    interface peak_detect
        module procedure :: peak_detect_r32
        module procedure :: peak_detect_r64
    end interface
    
    ! peaks_implementation.f90
    interface
        module subroutine peak_detect_r32(x, delta, maxind, maxvals, minind, minvals)
            real(real32), intent(in) :: x(:)
            real(real32), intent(in) :: delta
            integer(int32), intent(out), allocatable :: maxind(:), minind(:)
            real(real32), intent(out), allocatable :: maxvals(:), minvals(:)
        end subroutine

        module subroutine peak_detect_r64(x, delta, maxind, maxvals, minind, minvals)
            real(real64), intent(in) :: x(:)
            real(real64), intent(in) :: delta
            integer(int32), intent(out), allocatable :: maxind(:), minind(:)
            real(real64), intent(out), allocatable :: maxvals(:), minvals(:)
        end subroutine
    end interface

end module