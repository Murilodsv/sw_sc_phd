        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:45 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DPCHCE__genmod
          INTERFACE 
            SUBROUTINE DPCHCE(IC,VC,N,X,H,SLOPE,D,INCFD,IERR)
              INTEGER(KIND=4) :: INCFD
              INTEGER(KIND=4) :: IC(2)
              REAL(KIND=8) :: VC(2)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: X(*)
              REAL(KIND=8) :: H(*)
              REAL(KIND=8) :: SLOPE(*)
              REAL(KIND=8) :: D(INCFD,*)
              INTEGER(KIND=4) :: IERR
            END SUBROUTINE DPCHCE
          END INTERFACE 
        END MODULE DPCHCE__genmod
