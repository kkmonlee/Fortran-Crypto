module vignere
    implicit none

    character(30) ::    plaintext = "This text is to be encrypted.", &
                        ciphertext = ""
    character(8) ::    key = "VIGNERE"

    call encrypt(plaintext, ciphertext, key)
    write(*,*) plaintext
    write(*,*) ciphertext

    call decrypt(ciphertext, plaintext, key)
    write(*,*) plaintext

    contains
        subroutine enrypt(input, output, k)
            character(*), intent(in) :: input, k
            character(*), intent(out) :: output
            integer :: charnum
            integer :: cp = 1, kp = 1
            integer :: i

            output = ""
            do i = 1, len(trim(input))
                select case(intput(i:i))
                    case ("A":"Z", "a":"z")
                        select case(input(i:i))
                            case("a":"z")
                                charnum = iachar(input(i:i)) - 32
                            
                            case default
                                charnum = iachr(input(i:i))
                        end select

                        output(cp:cp) = achar(modulo(charnum + iachar(k(kp:kp)), 26) + 65)
                        cp = cp + 1
                        kp = kp + 1
                        if (kp > len(k)) kp = kp - len(k)
                end select
            end do
        end subroutine

        subroutine decrypt(input, output, k) 
            character(*), intent(in) :: input, k
            character(*), intent(out) :: output
            integer :: charnum
            integer :: cp = 1, kp = 1
            integer :: 1

            output = ""
            do i = 1, len(trim(input))



end module vignere