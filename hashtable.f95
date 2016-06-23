module hashtable_module
!     use crypto_mod, only: SHA1
    use sha1_module
    implicit none
    private

    ! default values
    integer, parameter, private :: keysize = 32, tablesize = 32, resize_factor = 2
    real, parameter, private :: loadfactor = 0.75

    type hashnode
        character(len=keysize) :: key = ''
        class(*), allocatable :: value
        type(hashnode), pointer :: next => null()
    end type hashnode

    type hashtable
        private
        type(hashnode), dimension(:), allocatable :: table
        integer :: count = 0

    contains
        private
        procedure, public :: put => hashtable_put
        procedure :: getc => hashtable_getc
        procedure :: getr => hashtable_getr
        procedure :: geti => hashtable_geti
        generic, public :: get => getc, getr, geti
        procedure, public :: dispose => hashtable_dispose
    end type hashtable

    public :: hashtable

contains
    function hash(key)
        character(len=*) :: key
        integer :: hash
        character(len=40) :: fullhash

        fullhash = SHA1(key)
        hash = ibclr(transfer(fullhash(1:4), hash), bit_size(hash)-1)
    end function hash

    subroutine dynamicresize(this)
        class(hashtable), target :: this
        real :: thisloadfactor
        type(hashnode), dimension(:), allocatable :: temptable
        integer :: n, i, count, j
        type(hashnode), pointer :: node

        n = size(this%table)
        count = this%count
        thisloadfactor = real(count, kind(thisloadfactor)) / real(n, kind(thisloadfactor))
        if (thisloadfactor > loadfactor) then
            allocate(temptable(count))
            j = 1
            do i = 1,n
                if (allocated(this%table(i)%value)) then
                    call hashnode_write(temptable(j), this%table(i)%key, this%table(i)%value)
                    node => this%table(i)
                    do while (associated(node%next))
                        j = j + 1
                        call hashnode_write(temptable(j), node%next%key, node%next%value)
                        node => node%next
                    end do
                    j = j + 1
                end if
            end do
            call this%dispose()
            allocate(this%table(resize_factor*n))
            do i = 1,count
                call this%put(temptable(i)%key, temptable(i)%value)
            end do
            deallocate(temptable)
        end if
    end subroutine dynamicresize

    subroutine hashtable_put(this, key, value)
        class(hashtable), target :: this
        character(len=*) :: key
        class(*) :: value
        integer :: index
        type(hashnode), pointer :: node

        if (len(key) > keysize) print *, 'Warning: Key, ', key, ', is longer than storage size (', keysize, ').'

        if (.not.allocated(this%table)) then
            allocate(this%table(tablesize))
            this%count = 0
        end if

        index = mod(hash(key), size(this%table))
        if (this%table(index)%key == '' .or. this%table(index)%key == key) then
            if (allocated(this%table(index)%value)) deallocate(this%table(index)%value)
            call hashnode_write(this%table(index), key, value)
        else
            node => this%table(index)
            do while (associated(node%next) .and. node%key /= key)
                node => node%next
            end do
            if (node%key == key) then
                if (allocated(node%value)) deallocate(node%value)
                call hashnode_write(node, key, value)
            else
                allocate(node%next)
                node => node%next
                call hashnode_write(node, key, value)
                nullify(node%next)
            end if
        end if
        this%count = this%count + 1
        call dynamicresize(this)
    end subroutine hashtable_put

    subroutine hashnode_write(this, key, value)
        class(hashnode) :: this
        character(len=*) :: key
        class(*) :: value
        real :: valuer
        integer :: valuei

        this%key = key
        select type(value)
            type is (character(*))
                call hashnode_value_setc(value, this%value)
            type is (real)
                valuer = value
                allocate(this%value, source=valuer)
            type is (integer)
                valuei = value
                allocate(this%value, source=valuei)
            class default
                print *, 'Error: passed object type is not supported.'
                stop
        end select
    end subroutine hashnode_write

    subroutine hashnode_value_setc(source, dest)
        character(*) :: source
        class(*), allocatable, intent(out) :: dest

        allocate(dest, source=source)
    end subroutine hashnode_value_setc

    subroutine hashtable_getc(this, key, value)
        class(hashtable), target :: this
        character(*) :: key, value
        integer :: index
        type(hashnode), pointer :: node

        if (len(key) > keysize) print *, 'Warning: Key, ', key, ', is longer than storage size (', keysize, ').'

        index = mod(hash(key), size(this%table))
        if (.not.associated(this%table(index)%next)) then
            if (allocated(this%table(index)%value)) then
                select type(source => this%table(index)%value)
                    type is (character(*))
                        value = source
                    class default
                        print *, 'Error: returned object type is not the result type, character(*).'
                        stop
                end select
            else
                print *, 'Error: key does not exist: ', key
                stop
            end if
        else
            node => this%table(index)
            do while (node%key /= key .and. associated(node%next))
                node => node%next
            end do
            if (node%key == key .and. allocated(node%value)) then
                select type(source => this%table(index)%value)
                    type is (character(*))
                        value = source
                    class default
                        print *, 'Error: returned object type is not the result type, character(*).'
                        stop
                end select
            else
                print *, 'Error: key does not exist: ', key
                stop
            end if
        end if
    end subroutine hashtable_getc

    subroutine hashtable_getr(this, key, value)
        class(hashtable), target :: this
        character(*) :: key
        real :: value
        integer :: index
        type(hashnode), pointer :: node

        if (len(key) > keysize) print *, 'Warning: Key, ', key, ', is longer than storage size (', keysize, ').'

        index = mod(hash(key), size(this%table))
        if (.not.associated(this%table(index)%next)) then
            if (allocated(this%table(index)%value)) then
                select type(source => this%table(index)%value)
                    type is (real)
                        value = source
                    class default
                        print *, 'Error: returned object type is not the result type, real.'
                        stop
                end select
            else
                print *, 'Error: key does not exist: ', key
                stop
            end if
        else
            node => this%table(index)
            do while (node%key /= key .and. associated(node%next))
                node => node%next
            end do
            if (node%key == key .and. allocated(node%value)) then
                select type(source => this%table(index)%value)
                    type is (real)
                        value = source
                    class default
                        print *, 'Error: returned object type is not the result type, real.'
                        stop
                end select
            else
                print *, 'Error: key does not exist: ', key
                stop
            end if
        end if
    end subroutine hashtable_getr

    subroutine hashtable_geti(this, key, value)
        class(hashtable), target :: this
        character(*) :: key
        integer :: value
        integer :: index
        type(hashnode), pointer :: node

        if (len(key) > keysize) print *, 'Warning: Key, ', key, ', is longer than storage size (', keysize, ').'

        index = mod(hash(key), size(this%table))
        if (.not.associated(this%table(index)%next)) then
            if (allocated(this%table(index)%value)) then
                select type(source => this%table(index)%value)
                    type is (integer)
                        value = source
                    class default
                        print *, 'Error: returned object type is not the result type, integer.'
                        stop
                end select
            else
                print *, 'Error: key does not exist: ', key
                stop
            end if
        else
            node => this%table(index)
            do while (node%key /= key .and. associated(node%next))
                node => node%next
            end do
            if (node%key == key .and. allocated(node%value)) then
                select type(source => this%table(index)%value)
                    type is (integer)
                        value = source
                    class default
                        print *, 'Error: returned object type is not the result type, integer.'
                        stop
                end select
            else
                print *, 'Error: key does not exist: ', key
                stop
            end if
        end if
    end subroutine hashtable_geti

    subroutine hashtable_dispose(this)
        class(hashtable), target :: this
        integer :: i
        type(hashnode), pointer :: node

        if (allocated(this%table)) then
            do i = 1,size(this%table)
                node => this%table(i)
                call hashnode_clear(node)
            end do
            deallocate(this%table)
            this%count = 0
        end if
    end subroutine hashtable_dispose

    recursive subroutine hashnode_clear(this)
        type(hashnode), pointer :: this

        if (associated(this%next)) then
            call hashnode_clear(this%next)
            deallocate(this%next)
            nullify(this%next)
        end if
        if (allocated(this%value)) deallocate(this%value)
    end subroutine hashnode_clear
end module hashtable_module
