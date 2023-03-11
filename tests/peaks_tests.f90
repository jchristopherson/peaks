program main
    use peaks_tests_implementation
    implicit none

    ! Local Variables
    logical :: local, overall
    integer(int32) :: flag

    ! Initialization
    overall = .true.

    ! Tests
    local = test_peak_detect_r32(flag)
    if (.not.local) overall = .false.

    local = test_peak_detect_r64(flag)
    if (.not.local) overall = .false.
    flag = 10 * flag

    ! End
    if (.not.overall) stop flag
end program