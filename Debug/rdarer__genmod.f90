        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:18 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RDARER__genmod
          INTERFACE 
            SUBROUTINE RDARER(XNAME,XMIN,XMAX,X,ILDEC,IFND)
              INTEGER(KIND=4) :: ILDEC
              CHARACTER(*) :: XNAME
              REAL(KIND=4) :: XMIN
              REAL(KIND=4) :: XMAX
              REAL(KIND=4) :: X(ILDEC)
              INTEGER(KIND=4) :: IFND
            END SUBROUTINE RDARER
          END INTERFACE 
        END MODULE RDARER__genmod
