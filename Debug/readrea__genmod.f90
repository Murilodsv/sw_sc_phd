        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:04 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE READREA__genmod
          INTERFACE 
            SUBROUTINE READREA(SEARCH,COL,VARREA,DIME,SINGLELV,SAMELINE,&
     &IOU,FILEN,MSG)
              INTEGER(KIND=4) :: DIME
              CHARACTER(*) :: SEARCH
              INTEGER(KIND=4) :: COL
              REAL(KIND=8) :: VARREA(DIME)
              LOGICAL(KIND=4) :: SINGLELV
              LOGICAL(KIND=4) :: SAMELINE
              INTEGER(KIND=4) :: IOU
              CHARACTER(*) :: FILEN
              CHARACTER(LEN=200) :: MSG
            END SUBROUTINE READREA
          END INTERFACE 
        END MODULE READREA__genmod
