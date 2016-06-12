module rsa
    use, intrinsic :: iso_fortran_env
    use primes
    implicit none
    private

    public :: rsaGenerateKeys, rsaEncrypt

    contains
    function modularMultiplicativeInverse(a0, n0) result(inv)
        integer(int32), intent(in) :: a0, n0
        integer(int32) :: inv
        integer(int32) :: a, n, t, nt, r, nr, quot, tmp

        a = a0
        n = n0
        t = 0_int8
        nt = 1_int8
        r = n
        nr = mod(a, n)
        if (n < 0_int32) then
            n = -n
        end if
        if (a < 0_int32) then
            a = n - mod(-a, n)
        end if
        do while (nr /= 0_int32)
            quot = r / nr
            tmp = nt; nt = t - quot * nt; t = tmp
            tmp = nr; nr = r - quot * nr; r = tmp
        end do
        if (r > 1_int32) then
            inv = -1
        else if (t < 0_int32) then
            inv = t + n
        else
            inv = t
        end if
    end function modularMultiplicativeInverse

    function rsaGenerateKeys() result(keys)
        integer(int32), dimension(3) :: keys
        integer(int32) :: p, q, n, t, e, d

        p = randomPrime(1_int32, 255_int32)
        q = randomPrime(1_int32, 255_int32)
        n = p * q
        t = (p - 1) * (q - 1)
        e = randomPrime(1_int32, t)
        d = modularMultiplicativeInverse(e, t)
        keys = [ n, e, d ]
    end function rsaGenerateKeys

    function rsaEncrypt(message, n, e)
        character(len=*), intent(in) :: message
        integer(int32), intent(in) :: n, e
        character(len=len(message)) :: rsaEncrypt
        integer(int8), dimension(:), allocatable :: bytes

        rsaEncrypt = message
    end function
end module
