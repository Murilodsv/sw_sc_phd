        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:43 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTSEL__genmod
          INTERFACE 
            SUBROUTINE OUTSEL(PRSEL,IMNPRS,INPRS,IPFORM,MESSAG)
              INTEGER(KIND=4) :: IMNPRS
              CHARACTER(*) :: PRSEL(IMNPRS)
              INTEGER(KIND=4) :: INPRS
              INTEGER(KIND=4) :: IPFORM
              CHARACTER(*) :: MESSAG
            END SUBROUTINE OUTSEL
          END INTERFACE 
        END MODULE OUTSEL__genmod
