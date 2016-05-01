module crypto_mod
    implicit none

    contains
        function SHA1(string)
            character(len=*) :: string
            character*20 :: SHA1
            integer*4 :: h0, h1, h2, h3, h4, w(80), a, b, c, d, e, f, k, temp
            integer*8 :: s1
            integer*1, dimension(:), allocatable :: ipadded
            integer :: i, j, n, l

            s1 = len_trim(string)

            allocate(ipadded(((sl+8)/64 + 1)*64))
            ipadded(:sl) = transfer(string(:sl), ipadded(:sl))
            ipadded(sl+1) = ibset(0_1, 7)
            ipadded(s1+2:) = 0
            ipadded(size(ipadded)-7:) = bytearray_reverse(transfer(sl*8, ipadded(size(ipadded)-7:)))

            h0 = 1732584193 ! z'67452301'
            h1 = -271733879 ! z'EFCDAB89'
            h2 = -1732584194 ! z'98BADCFE'
            h3 = 271733878 ! z'10325476'
            h4 = -1009589776 ! z'C3D2E1F0'

            do i = 1,size(ipadded)/64
                do j = 1,16
                    ! takes a 512-bit chunk of string
                    w(j) = transfer(ipadded((i-1)*64 + j*4:(i-1)*64 + (j-1)*4 + 1:-1), w(j))
                end do

                do j = 17,80
                    ! Extend 16 32-bit words into 80 32-bit words
                    w(j) = ishftc(ieor(ieor(ieor(w(j-3), w(j-8)), w(j-14)), w(j-16)), 1)
                end do

                a = h0; b = h1; c = h2; d = h3; e = h4

                do j = 1,80
                    select case (j)
                        case (1:20)
                            f = ior(iand(b, c), iand(not(b), d))
                            k = 1518500249 ! k = z'5A827999'
                        case (21:40)
                            f = ieor(ieor(b, c), d)
                            k = 1859775393 ! k = z'6ED9EBA1'
                        case (41:60)
                            f = ior(ior(iand(b, c), iand(b, d)), iand(c, d))
                            k = -1894007588 ! k = z'8F1BBCDC'
                        case (61:80)
                            f = ieor(ieor(b, c), d)
                            k = -899497514 ! k = z'CA62C1D6'
                    end select

                    temp = ishftc(a, 5) + f + e + w(j) + k
                    e = d
                    d = c
                    c = ishftc(b, 30)
                    b = a
                    a = temp
                end do

                h0 = h0 + a
                h1 = h1 + b
                h2 = h2 + c
                h3 = h3 + d
                h4 = h4 + e
            end do

            deallocate(ipadded)

            SHA1(1:4) = transfer(int_reversebytes(h0), SHA1(1:4))
            SHA1(5:8) = transfer(int_reversebytes(h1), SHA1(5:8))
            SHA1(9:12) = transfer(int_reversebytes(h2), SHA1(9:12))
            SHA1(13:16) = transfer(int_reversebytes(h3), SHA1(13:16))
            SHA1(17:20) = transfer(int_reversebytes(h4), SHA1(17:20))

    end function SHA1

    function int_reversebytes(a)
        integer*4 :: a, int_reversebytes, i
        integer*1, dimension(4) :: bytes, newbytes

        bytes = transfer(a, bytes)
        do i = 1,4
            newbytes(i) = bytes(4-i+1)
        end do
        int_reversebytes = transfer(newbytes, int_reversebytes)
    end function int_reversebytes

    function bytearray_reverse(a)
        integer*1, dimension(:) :: a
        integer*1, dimension(size(a)) :: bytearray_reverse

        bytearray_reverse = a(size(a):1:-1)
    end function bytearray_reverse

end module crypto_mod
