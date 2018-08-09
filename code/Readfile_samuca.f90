subroutine readrea(search,col,varrea,dime,singlelv,sameline,iou,filen,msg)   
    ! Reads a real variable within an input file (unit = iou) based on a search string and columm
    ! MSV - 13-jan-2017
    
    implicit none
    
    
    integer iou                                 !i/o unit
    integer ios                                 !i/o status
    integer lnumber                             !line counter
    integer dimnumber                           !Variable dimension index
    integer col                                 !Column number
    integer dime                                !Variable dimension    
    
    character (len=*)       filen               !File name
    character (len=1000)    line                !Line of file (RECORD)
    character (len=200)    msg                  !Ordinary msg for warnings
    character (len=10)      skipcol             !Ignored variable
    character (len=100)     fmtslv              !Format for a single value and line
    character (len=*)       search              !Search string
    
    real*8 varrea(dime)                           !Host variable
    
    character (len=1)    :: skip = '!'          !Skip character indicator
    character (len=1)    :: stch = '*'          !Search character indicator
    
    logical singlelv                            !Flag for single value at line
    logical sameline                            !Flag to start the reading on same search line
    
    lnumber     = 1
    dimnumber   = 1
    
    do
        read(iou,'(A1000)',iostat=ios) line
        if(ios .gt. 0) then
            write(msg,'(A,A,A,I,A)') 'Problem in reading, please check file',trim(filen),'. Line ',lnumber,'.'
            !call wmsg(5)
        
        else if(ios .lt. 0) then
            write(msg,'(A,A,A,A,A)') 'No ',trim(search),' information found in ',trim(filen),' file.'
            !call wmsg(5)
            exit
        endif      
    
        if(line(1:1) == skip .or. line(1:1)==char(9) .or. trim(line)=='') cycle
        
        if(line(1:(len_trim(search)+1)) == trim('*'//search)) then            
                        
            do while(dimnumber .le. dime)
                
            if(.not. sameline) read(iou,'(A1000)',iostat=ios) line            
            
            if(ios .gt. 0) then
            write(msg,'(A,A,A,A,A,I,A)') 'Problem in ',search,' reading, please check file',trim(filen),'. Line ',lnumber,'.'
            !call wmsg(5)
            else if(ios .lt. 0)then
                exit
            endif   
            
            lnumber = lnumber + 1
            
            if(line(1:1) == skip .or. line(1:1)==char(9) .or. trim(line)=='') cycle            
            if(line(1:1) == stch) exit
            
            if(singlelv) then                
                
                
                read(line,'(I5)',iostat=ios) varrea(dimnumber)
                exit
                
            else
                
            select case (col)       
            case (1)                
                read(line,*) varrea(dimnumber)          

            case (2)
                read(line,*) skipcol,varrea(dimnumber)
       
            case (3)
                read(line,*) skipcol,skipcol,varrea(dimnumber)
       
            case (4)
                read(line,*) skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (5)
                read(line,*) skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (6)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (7)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (8)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (9)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (10)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (11)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (12)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (13)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (14)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (15)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (16)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (17)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (18)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (19)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)

            case (20)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (21)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (22)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (23)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (24)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (25)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (26)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (27)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (28)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (29)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (30)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            case (31)
                read(line,*) skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,skipcol,varrea(dimnumber)
       
            end select            
            
            dimnumber   =   dimnumber + 1
            
            endif
            
            enddo
            
            exit      
        endif
        
        
    enddo
    
    rewind(iou)
    
    
    end subroutine readrea