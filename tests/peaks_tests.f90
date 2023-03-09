program main
    use peaks_tests_implementation
    implicit none

    ! Local Variables
    logical :: local, overall

    ! Initialization
    overall = .true.

    ! Tests
    local = test_peak_detect_r32()
    if (.not.local) overall = .false.

    ! End
    if (.not.overall) stop 1
end program