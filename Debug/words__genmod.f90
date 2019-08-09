        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:45 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE WORDS__genmod
          INTERFACE 
            SUBROUTINE WORDS(RECORD,ILW,SEPARS,IWBEG,IWEND,IFND)
              INTEGER(KIND=4) :: ILW
              CHARACTER(*) :: RECORD
              CHARACTER(*) :: SEPARS
              INTEGER(KIND=4) :: IWBEG(ILW)
              INTEGER(KIND=4) :: IWEND(ILW)
              INTEGER(KIND=4) :: IFND
            END SUBROUTINE WORDS
          END INTERFACE 
        END MODULE WORDS__genmod
