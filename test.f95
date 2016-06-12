program fortran-crypto
    use, instrinsic :: iso_fortran_end
    use primes
    use sha1
    use rsa
    implicit none

    print *, "isPrime(567)"
    print *, isPrime(567_int8)

    print *, "primesUpTo(1024)"
    print *, primesUpTo(1024_int32)

    print *, "test"
    print *, SHA1("test")

    print *, ""
    print *, SHA1("")

    print *, 20_int64
    print *, SHA1(20_int64)

    print *, 2300_int64
    print *, SHA1(2300_int64)

    print *, rsaGenerateKeys()
end program fortran-crypto
