module primes
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    interface primesUpTo
        module procedure primesUpTo_int8, primesUpTo_int32
    end interface

    interface isPrime
        module procedure isPrime_int8, isPrime_int32
    end interface

    interface randomPrime
        module procedure randomPrime_int8, randomPrime_int32
    end interface

    public :: primesUpTo, isPrime, randomPrime

    contains
    function primesUpTo_int8(n) result(primes)
        integer(int8), intent(in) :: n
        integer(int8), dimension(:), allocatable :: primes
        integer(int8), dimension(:), allocatable :: temp
        logical, dimension(:), allocatable :: fullset
        integer(int8) :: i, j

        allocate(primes(0))
        if (n > 1_int8) then ! basic sieve
            allocate(fullset(n))
            fullset = .true.
            do i = 2,n
                if (fullset(i)) then
                    allocate(temp(size(primes) + 1))
                    temp(:size(primes)) = primes
                    temp(size(primes) + 1) = i
                    deallocate(primes)
                    allocate(primes(size(temp)), source=temp)
                    deallocate(temp)
                    do j = 2_int8 * i, n, i
                        fullset(j) = .false.
                    end do
                end if
            end do
            deallocate(fullset)
        end if
    end function

    function primesUpTo_int32(n) result(primes)
        integer(int32), intent(in) :: n
        integer(int32), dimension(:), allocatable :: primes
        integer(int32), dimension(:), allocatable :: temp
        logical, dimension(:), allocatable :: fullset
        integer(int32) :: i, j

        allocate(primes(0))
        if (n > 1_int32) then ! basic sieve
            allocate(fullset(n))
            fullset = .true.
            do i = 2,n
                if (fullset(i)) then
                    allocate(temp(size(primes) + 1))
                    temp(:size(primes)) = primes
                    temp(size(primes) + 1) = i
                    deallocate(primes)
                    allocate(primes(size(temp)), source=temp)
                    deallocate(temp)
                    do j = 2_int32 * i, n, i
                        fullset(j) = .false.
                    end do
                end if
            end do
            deallocate(fullset)
        end if
    end function

    function isPrime_int8(n) result(t)
        integer(int8), intent(in) :: n
        logical :: t
        integer(int8), dimension(:), allocatable :: primes
        integer :: i

        t = .true.
        primes = primesUpTo_int8(floor(sqrt(real(n)), int8))
        do i = 1,size(primes)
            if (mod(n, primes(i)) == 0_int8) then
                t = .false.
                exit
            end if
        end do
        deallocate(primes)
    end function

    function isPrime_int32(n) result(t)
        integer(int32), intent(in) :: n
        logical :: t
        integer(int32), dimension(:), allocatable :: primes
        integer :: i

        t = .true.
        primes = primesUpTo_int32(floor(sqrt(real(n)), int32))
        do i = 1,size(primes)
            if (mod(n, primes(i)) == 0_int32) then
                t = .false.
                exit
            end if
        end do
        deallocate(primes)
    end function

    recursive function randomPrime_int8(min, max) result(p)
        integer(int8), intent(in) :: min, max
        integer(int8) :: p
        logical :: first = .true.
        real(real32) :: r

        if (first) then
            first = .false.
            call RANDOM_SEED()
        end if
        call RANDOM_NUMBER(r)
        p = floor(r * ((max - 1) - min + 1), int8) + min
        if (.not. isPrime_int8(p)) then
            p = randomPrime_int8(min, max)
        end if
    end function

    recursive function randomPrime_int32(min, max) result(p)
        integer(int32), intent(in) :: min, max
        integer(int32) :: p
        logical :: first = .true.
        real(real32) :: r

        if (first) then
            first = .false.
            call RANDOM_SEED()
        end if
        call RANDOM_NUMBER(r)
        p = floor(r * ((max - 1) - min + 1), int32) + min
        if (.not. isPrime_int32(p)) then
            p = randomPrime_int32(min, max)
        end if
    end function
end module
