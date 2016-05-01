module hashtable
    use crypto_mod, only: SHA1
    implicit none

    !default values
    integer, parameter, private :: valuesize = 32, tablesize = 32
    real, parameter, private :: loadfactor = 0.75

    type hashnode
        character(len = valuesize) :: key = ''
        character(len = valuesize) :: value = ''
        type(hashnode), pointer :: next => null()
    end type hashnode

    type hashtable
        private
        type(hashnode), dimension(:), allocatable :: table
        integer :: entrycount = 0

    contains
        procedure :: init => hashtable_init
        procedure :: put => hashtable_put
        procedure :: get => hashtable_get
        procedure :: dispose => hastable_dispose
    end type

    private :: hashtable_init, hash, dynamicresize, hashtable_put, hashtable_get, hashtable_dispose

contains
    subroutine hashtable_init(this)
        class(hashtable) :: this

        if (.not.allocated(this%table)) then
            allocate(this%table(tablesize))
            this%entrycounter = 0
        end if
    end subroutine hashtable_init

    function hash(key)
        character(len=*) :: key
        integer :: hash
        character*20 :: fullhash

        fullhash = SHA1(key)
        hash = ibclr(transfer(fullhash(1:4), hash), bit_size(hash) - 1)
    end function hash

    subroutine dynamicresize(this)
        class(hashtable) :: this
        real :: thisloadfactor
        type(hashnode), dimension(:), allocatable, target :: temptable
        integer :: n, i
        type(hashnode), pointer :: node

        n = size(this%table)
        thisloadfactor = real(this%entrycount) / real(n)
        if (thisloadfactor > loadfactor) then
            temptable = this%table
            deallocate(this%table)
            allocate(this%table(2*n))
            do i = 1, n
                if (temptable(i)%key /= '') then
                    call this%put(temptable(i)%key, temptable(i)%value)
                    node => temptable(i)
                    do while (associated(node%next))
                        node => node%next
                        call this%put(node%key, node%value)
                    end do
                end if
            end do
            deallocate(temptable)
        end if
    end subroutine dynamicresize

