        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:45 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DPCHCS__genmod
          INTERFACE 
            SUBROUTINE DPCHCS(SWITCH,N,H,SLOPE,D,INCFD,IERR)
              INTEGER(KIND=4) :: INCFD
              REAL(KIND=8) :: SWITCH
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: H(*)
              REAL(KIND=8) :: SLOPE(*)
              REAL(KIND=8) :: D(INCFD,*)
              INTEGER(KIND=4) :: IERR
            END SUBROUTINE DPCHCS
          END INTERFACE 
        END MODULE DPCHCS__genmod
