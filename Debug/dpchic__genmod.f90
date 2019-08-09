        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:45 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DPCHIC__genmod
          INTERFACE 
            SUBROUTINE DPCHIC(IC,VC,SWITCH,N,X,F,D,INCFD,WK,NWK,IERR)
              INTEGER(KIND=4) :: NWK
              INTEGER(KIND=4) :: INCFD
              INTEGER(KIND=4) :: IC(2)
              REAL(KIND=8) :: VC(2)
              REAL(KIND=8) :: SWITCH
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: X(*)
              REAL(KIND=8) :: F(INCFD,*)
              REAL(KIND=8) :: D(INCFD,*)
              REAL(KIND=8) :: WK(NWK)
              INTEGER(KIND=4) :: IERR
            END SUBROUTINE DPCHIC
          END INTERFACE 
        END MODULE DPCHIC__genmod
