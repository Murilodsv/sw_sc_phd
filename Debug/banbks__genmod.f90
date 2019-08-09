        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:41 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE BANBKS__genmod
          INTERFACE 
            SUBROUTINE BANBKS(A,N,M1,M2,NP,MP,AL,MPL,INDX,B)
              INTEGER(KIND=4) :: MPL
              INTEGER(KIND=4) :: MP
              INTEGER(KIND=4) :: NP
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: A(NP,MP)
              INTEGER(KIND=4) :: M1
              INTEGER(KIND=4) :: M2
              REAL(KIND=8) :: AL(NP,MPL)
              INTEGER(KIND=4) :: INDX(N)
              REAL(KIND=8) :: B(N)
            END SUBROUTINE BANBKS
          END INTERFACE 
        END MODULE BANBKS__genmod
