        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:39 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RDFDOR__genmod
          INTERFACE 
            SUBROUTINE RDFDOR(XNAME,XMIN,XMAX,X,ILDEC,IVALS)
              INTEGER(KIND=4) :: ILDEC
              CHARACTER(*) :: XNAME
              REAL(KIND=8) :: XMIN
              REAL(KIND=8) :: XMAX
              REAL(KIND=8) :: X(ILDEC)
              INTEGER(KIND=4) :: IVALS
            END SUBROUTINE RDFDOR
          END INTERFACE 
        END MODULE RDFDOR__genmod
