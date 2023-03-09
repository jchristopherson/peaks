module peaks_tests_implementation
    use iso_fortran_env
    use peaks
    use fortran_test_helper
    implicit none
contains
! ------------------------------------------------------------------------------
function test_peak_detect_r32() result(rst)
    ! Arguments
    logical :: rst

    ! Local Variables
    real(real32), parameter :: tol = 1.0e-3
    real(real32), parameter :: delta = 0.05
    real(real32), parameter :: dt = 1.0e-3
    real(real32), parameter :: freq_hz = 10.0
    real(real32), parameter :: pi = 2.0 * acos(0.0)
    integer(int32), parameter :: npts = 1000

    integer(int32) :: i, nmax, nmin
    integer(int32), allocatable, dimension(:) :: imx, imn
    real(real32) :: t, x(npts)
    real(real32), allocatable, dimension(:) :: mx, mn, mxans, mnans

    ! Initialization
    rst = .true.

    ! Build a harmonic waveform
    t = 0.0
    do i = 1, npts
        x(i) = sin(2.0 * pi * freq_hz * t)
        t = t + dt
    end do

    ! Find the peaks
    call peak_detect(x, delta, imx, mx, imn, mn)

    ! Check the lengths of each array
    nmax = floor(freq_hz / t)
    nmin = nmax
    if (size(imx) /= nmax) then
        rst = .false.
        print '(A)', "TEST FAILED: test_peak_detect_r32 1-1"
    end if
    if (size(mx) /= nmax) then
        rst = .false.
        print '(A)', "TEST FAILED: test_peak_detect_r32 1-2"
    end if

    if (size(imn) /= nmin) then
        rst = .false.
        print '(A)', "TEST FAILED: test_peak_detect_r32 1-3"
    end if
    if (size(mn) /= nmin) then
        rst = .false.
        print '(A)', "TEST FAILED: test_peak_detect_r32 1-4"
    end if

    ! Ensure the values make sense
    allocate(mxans(nmax), source = 1.0)
    allocate(mnans(nmin), source = -1.0)
    if (.not.assert(mx, mxans, tol)) then
        rst = .false.
        print '(A)', "TEST FAILED: test_peak_detect_r32 1-5"
    end if
    if (.not.assert(mn, mnans, tol)) then
        rst = .false.
        print '(A)', "TEST FAILED: test_peak_detect_r32 1-6"
    end if
end function

! ------------------------------------------------------------------------------
end module