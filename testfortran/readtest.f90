module readfiles
    implicit none

    interface readarg
        module procedure readarg_1
        module procedure readarg_2
    end interface readarg

    contains 

    subroutine readarg_1(arg1)
        implicit none
        integer::i,length,status
        character(:), allocatable,intent(out) :: arg1
        intrinsic :: command_argument_count, get_command_argument
        if (command_argument_count() .ne. 1) then
            write(*,*) "error! num. of arguments should be 1"
            stop
        end if
        i = 1
        call get_command_argument(i, length = length, status = status)
        if (status == 0) then
            allocate (character(length) :: arg1)
            call get_command_argument(i, arg1, status = status)
            write(*,*) arg1
        end if

    end subroutine

    subroutine readarg_2(arg1,arg2)
        implicit none
        integer::i,length,status
        character(:), allocatable,intent(out) :: arg1,arg2
        intrinsic :: command_argument_count, get_command_argument
        if (command_argument_count() .ne. 2) then
            write(*,*) "error! num. of arguments should be 1"
            stop
        end if
        i = 1
        call get_command_argument(i, length = length, status = status)
        if (status == 0) then
            allocate (character(length) :: arg1)
            call get_command_argument(i, arg1, status = status)
            write(*,*) arg1
        end if

        i = 2
        call get_command_argument(i, length = length, status = status)
        if (status == 0) then
            allocate (character(length) :: arg2)
            call get_command_argument(i, arg2, status = status)
            write(*,*) arg2
        end if        

    end subroutine    

    subroutine readfromfiles(filename)
        implicit none
        character(len=*),intent(in)::filename
        integer::io
        integer,parameter :: max_line_len = 4000
        character(max_line_len) linebuf
        integer::equalposition
        integer::ivalue,length
        real(8)::dvalue
        character(:), allocatable::cvalue,ckeyword
        

        open(101,file=filename)
        do 
            read(101,'(a)',iostat = io) linebuf
            if (io < 0) exit
            write(*,*) "Original string: ",trim(linebuf)
            equalposition = index(trim(linebuf),"=")
            if (equalposition.ne. 0) then
                length = len(trim(linebuf(:equalposition-1)))
                allocate(character(length) :: ckeyword)
                !write(*,*) "keyword ",ckeyword

                length = len(trim(linebuf(equalposition+1:)))
                allocate(character(length) :: cvalue)
                !write(*,*) "value ",cvalue
                write(*,*) "We read from data: "
                if (ckeyword == "model") then
                    write(*,*) ckeyword, cvalue
                else if (ckeyword == "method") then
                    write(*,*) ckeyword, cvalue
                else if (ckeyword == "lattice") then
                    write(*,*) ckeyword, cvalue
                else if (ckeyword == "W") then
                    read(cvalue,*) ivalue
                    write(*,*) ckeyword, ivalue
                else if (ckeyword == "L") then
                    read(cvalue,*) ivalue
                    write(*,*) ckeyword, ivalue
                else if (ckeyword == "t") then
                    read(cvalue,*) dvalue
                    write(*,*) ckeyword, dvalue
                else if (ckeyword == "U") then
                    read(cvalue,*) dvalue
                    write(*,*) ckeyword, dvalue
                else if (ckeyword == "nelec") then
                    read(cvalue,*) ivalue
                    write(*,*) ckeyword, ivalue
                else if (ckeyword == "2Sz") then
                    read(cvalue,*) dvalue
                    write(*,*) ckeyword, dvalue
                else
                    write(*,*) "unsupported input key!"
                    stop
                end if    

                deallocate(cvalue)
                deallocate(ckeyword)

            end if
            
        end do

        close(101)

        return
    end subroutine


end module readfiles

program main
    use readfiles
    implicit none
    character(:), allocatable::arg1
    character(:), allocatable::arg2
    call readarg(arg1)
    call readfromfiles(arg1)
end program

