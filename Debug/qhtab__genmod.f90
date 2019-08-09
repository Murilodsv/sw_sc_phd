        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:33 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE QHTAB__genmod
          INTERFACE 
            FUNCTION QHTAB(WLEV,IMPER,HQHTAB,QQHTAB)
              REAL(KIND=8) :: WLEV
              INTEGER(KIND=4) :: IMPER
              REAL(KIND=8) :: HQHTAB(3660,25)
              REAL(KIND=8) :: QQHTAB(3660,25)
              REAL(KIND=8) :: QHTAB
            END FUNCTION QHTAB
          END INTERFACE 
        END MODULE QHTAB__genmod
