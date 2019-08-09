        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:06 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RDFINR__genmod
          INTERFACE 
            SUBROUTINE RDFINR(XNAME,XMIN,XMAX,X,ILDEC,IVALS)
              INTEGER(KIND=4) :: ILDEC
              CHARACTER(*) :: XNAME
              INTEGER(KIND=4) :: XMIN
              INTEGER(KIND=4) :: XMAX
              INTEGER(KIND=4) :: X(ILDEC)
              INTEGER(KIND=4) :: IVALS
            END SUBROUTINE RDFINR
          END INTERFACE 
        END MODULE RDFINR__genmod
