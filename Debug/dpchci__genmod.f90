        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:45 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DPCHCI__genmod
          INTERFACE 
            SUBROUTINE DPCHCI(N,H,SLOPE,D,INCFD)
              INTEGER(KIND=4) :: INCFD
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: H(*)
              REAL(KIND=8) :: SLOPE(*)
              REAL(KIND=8) :: D(INCFD,*)
            END SUBROUTINE DPCHCI
          END INTERFACE 
        END MODULE DPCHCI__genmod
