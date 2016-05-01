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

    subroutine hashtable_put(this, key, value)
        class(hashtable), target :: this
        character(len=*) :: key, value
        integer :: index
        type(hashnode), pointer :: node

        if (len(key) > valuesize) print *, '!!! Key, ', key, ', is longer than storage size. Might cause overflow. (', valuesize, ').'
        if (len(key) > valuesize) print *, '!!! Value, ', value, ', is longer than storage size. Might cause overflow. (', valuesize, ').'

        index = mod(hash(key), size(this%table))
        if (this%table(index)%key == '' .or. this%table(index)%key == key) then
            this%table(index)%key = key
            this%table(index)%value = value
        else
            node => this%table(index)
            do while (associated(node%next) .and. node%key /= key)
                node => node%next
            end do
            if (node %key == key) then
                node%key = key
                node%value = value
            else
                allocate(node%next)
                node => node%next
                node%key = key
                node%value = value
                nullify(node%next)
            end if
        end if
        this%entrycount = this%entrycounter + 1
        call dynamicresize(this)
    end subroutine hashtable_put

    function hashtable_get(this, key)
        class(hashtable), target :: this
        character(len=*) :: key
        character(len=:), allocatable :: hashtable_get
        integer :: index
        type(hashnode), pointer :: node

        if (len(key) > valuesize) print *, 'Key, ', key, ', is longer than storage size (', valuesize, ').'

        index = mod(hash(key), size(this%table))
        if (.not.associated(this%table(index)%next)) then
            hashtable_get = trim(this%table(index)%value)
        else
            node => this%table(index)
            do while (node%key /= key .and. associated(node%next))
                node => node%next
            end do
            if (node%key == key) then
                hashtable_get = trim(node%value)
            else
                print *, '!!! Unable to find key: ', key
                stop
            end if
        end if
    end function hashtable_get

    subroutine hashtable_dispose(this)
        class(hashtable), target :: this
        integer :: i
        type(hashnode, pointer :: node

        if (allocated(this%table)) then
            do i = 1, size(this%table)
                node => this%table(i)
                call hashnode_clear(node)
            end do
            deallocate(this%table)
            this%entrycounter = 0
        end if
    end subroutine hashtable_dispose(this)

    recursive subroutine hashnode_clear(this)
        type(hashnode), pointer :: this

        if (associated(this%next)) then
            call hashnode_clear(this%next)
            deallocate(this%next)
            nullify(this%next)
        end if

    end subroutine hashnode_clear
end module hashtable
