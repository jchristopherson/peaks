! Copyright (c) 2023 Jason Christopherson
! SPDX-License-Identifier: MIT
!
! PEAKS is a peak detection library meant to locate peaks and valleys in a signal.
! This file is part of PEAKS.
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the "Software"), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in all
! copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
! SOFTWARE.

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
