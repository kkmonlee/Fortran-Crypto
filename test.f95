program fortran_crypto
    use, intrinsic :: iso_fortran_env
    use primes_module
    use sha1_module
    use rsa_module
    use hashtable_module
    implicit none

    print *, "isPrime(127)"
    print *, isPrime(127_int8) ! max for int8

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
end program fortran_crypto
