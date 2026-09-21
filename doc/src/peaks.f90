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

module peaks
    use iso_fortran_env
    implicit none
    private
    public :: peak_detect
    interface peak_detect
        !! Attempts to locate local maxima (peaks), and local minima 
        !! (valleys) within a signal.
        !!
        !! The code is a re-implementation of the peak detection routine 
        !! presented by Eli Billauer, which can be found at 
        !! https://billauer.co.il/blog/2009/01/peakdet-matlab-octave/.
        module procedure :: peak_detect_r32
        module procedure :: peak_detect_r64
    end interface
    
contains
! ------------------------------------------------------------------------------
subroutine peak_detect_r32(x, delta, maxind, maxvals, minind, minvals)
    !! The real32 implementation.
    real(real32), intent(in) :: x(:)
        !! An N-element array containing the signal to analyze.
    real(real32), intent(in) :: delta
        !! A threshold value that determines the sensitivity the algorithm has 
        !! to discerning local maxima or minima from noise.
    integer(int32), intent(out), allocatable :: maxind(:)
        !! An allocatable array that will be allocated by this routine and 
        !! filled with the indices of the local maxima.
    real(real32), intent(out), allocatable :: maxvals(:)
        !! An allocatable array that will be allocated by this routine and 
        !! filled with the local maxima.
    integer(int32), intent(out), allocatable :: minind(:)
        !! An allocatable array that will be allocated by this routine and 
        !! filled with the indices of the local minima.
    real(real32), intent(out), allocatable :: minvals(:)
        !! An allocatable array that will be allocated by this routine and 
        !! filled with the local minima.

    ! Local Variables
    logical :: lookformax
    integer(int32) :: i, n, nmax, nmin, mxpos, mnpos
    integer(int32), allocatable :: maxibuffer(:), minibuffer(:)
    real(real32), allocatable :: maxbuffer(:), minbuffer(:)
    real(real32) :: this, mn, mx

    ! Initialization
    n = size(x)
    nmax = 0
    nmin = 0
    lookformax = .true.
    mn = huge(delta)
    mx = -huge(delta)
    allocate(maxibuffer(n))
    allocate(maxbuffer(n))
    allocate(minibuffer(n))
    allocate(minbuffer(n))

    ! Process
    do i = 1, n
        this = x(i)
        if (this > mx) then
            mx = this
            mxpos = i
        end if
        if (this < mn) then
            mn = this
            mnpos = i
        end if

        if (lookformax) then
            if (this < mx - delta) then
                nmax = nmax + 1
                maxibuffer(nmax) = mxpos
                maxbuffer(nmax) = mx
                mn = this
                mnpos = i
                lookformax = .false.
            end if
        else
            if (this > mn + delta) then
                nmin = nmin + 1
                minibuffer(nmin) = mnpos
                minbuffer(nmin) = mn
                mx = this
                mxpos = i
                lookformax = .true.
            end if
        end if
    end do

    ! Handle the output
    allocate(maxind(nmax), source = maxibuffer(1:nmax))
    allocate(maxvals(nmax), source = maxbuffer(1:nmax))
    allocate(minind(nmin), source = minibuffer(1:nmin))
    allocate(minvals(nmin), source = minbuffer(1:nmin))
end subroutine

! ------------------------------------------------------------------------------
subroutine peak_detect_r64(x, delta, maxind, maxvals, minind, minvals)
    !! The real64 implementation.
    real(real64), intent(in) :: x(:)
        !! An N-element array containing the signal to analyze.
    real(real64), intent(in) :: delta
        !! A threshold value that determines the sensitivity the algorithm has 
        !! to discerning local maxima or minima from noise.
    integer(int32), intent(out), allocatable :: maxind(:)
        !! An allocatable array that will be allocated by this routine and 
        !! filled with the indices of the local maxima.
    real(real64), intent(out), allocatable :: maxvals(:)
        !! An allocatable array that will be allocated by this routine and 
        !! filled with the local maxima.
    integer(int32), intent(out), allocatable :: minind(:)
        !! An allocatable array that will be allocated by this routine and 
        !! filled with the indices of the local minima.
    real(real64), intent(out), allocatable :: minvals(:)
        !! An allocatable array that will be allocated by this routine and 
        !! filled with the local minima.

    ! Local Variables
    logical :: lookformax
    integer(int32) :: i, n, nmax, nmin, mxpos, mnpos
    integer(int32), allocatable :: maxibuffer(:), minibuffer(:)
    real(real64), allocatable :: maxbuffer(:), minbuffer(:)
    real(real64) :: this, mn, mx

    ! Initialization
    n = size(x)
    nmax = 0
    nmin = 0
    lookformax = .true.
    mn = huge(delta)
    mx = -huge(delta)
    allocate(maxibuffer(n))
    allocate(maxbuffer(n))
    allocate(minibuffer(n))
    allocate(minbuffer(n))

    ! Process
    do i = 1, n
        this = x(i)
        if (this > mx) then
            mx = this
            mxpos = i
        end if
        if (this < mn) then
            mn = this
            mnpos = i
        end if

        if (lookformax) then
            if (this < mx - delta) then
                nmax = nmax + 1
                maxibuffer(nmax) = mxpos
                maxbuffer(nmax) = mx
                mn = this
                mnpos = i
                lookformax = .false.
            end if
        else
            if (this > mn + delta) then
                nmin = nmin + 1
                minibuffer(nmin) = mnpos
                minbuffer(nmin) = mn
                mx = this
                mxpos = i
                lookformax = .true.
            end if
        end if
    end do

    ! Handle the output
    allocate(maxind(nmax), source = maxibuffer(1:nmax))
    allocate(maxvals(nmax), source = maxbuffer(1:nmax))
    allocate(minind(nmin), source = minibuffer(1:nmin))
    allocate(minvals(nmin), source = minbuffer(1:nmin))
end subroutine

! ------------------------------------------------------------------------------
end module
