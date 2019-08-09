        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:33 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE STEPNR__genmod
          INTERFACE 
            FUNCTION STEPNR(ARRAY,LENGTH,X)
              INTEGER(KIND=4) :: LENGTH
              REAL(KIND=8) :: ARRAY(LENGTH)
              REAL(KIND=8) :: X
              INTEGER(KIND=4) :: STEPNR
            END FUNCTION STEPNR
          END INTERFACE 
        END MODULE STEPNR__genmod
