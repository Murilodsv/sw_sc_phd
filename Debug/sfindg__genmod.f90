        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:22 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SFINDG__genmod
          INTERFACE 
            SUBROUTINE SFINDG(NAMLIS,ILDEC,IST,IEND,NAME,ISTYPE,IFINDG, &
     &IMATCH)
              INTEGER(KIND=4) :: ILDEC
              CHARACTER(*) :: NAMLIS(ILDEC)
              INTEGER(KIND=4) :: IST
              INTEGER(KIND=4) :: IEND
              CHARACTER(*) :: NAME
              INTEGER(KIND=4) :: ISTYPE
              INTEGER(KIND=4) :: IFINDG
              INTEGER(KIND=4) :: IMATCH
            END SUBROUTINE SFINDG
          END INTERFACE 
        END MODULE SFINDG__genmod
