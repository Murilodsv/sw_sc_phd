        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:46 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RDADOR__genmod
          INTERFACE 
            SUBROUTINE RDADOR(XNAME,XMIN,XMAX,X,ILDEC,IFND)
              INTEGER(KIND=4) :: ILDEC
              CHARACTER(*) :: XNAME
              REAL(KIND=8) :: XMIN
              REAL(KIND=8) :: XMAX
              REAL(KIND=8) :: X(ILDEC)
              INTEGER(KIND=4) :: IFND
            END SUBROUTINE RDADOR
          END INTERFACE 
        END MODULE RDADOR__genmod
