        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:41 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE TRIDAG__genmod
          INTERFACE 
            SUBROUTINE TRIDAG(N,A,B,C,R,U,IERROR)
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: A(N)
              REAL(KIND=8) :: B(N)
              REAL(KIND=8) :: C(N)
              REAL(KIND=8) :: R(N)
              REAL(KIND=8) :: U(N)
              INTEGER(KIND=4) :: IERROR
            END SUBROUTINE TRIDAG
          END INTERFACE 
        END MODULE TRIDAG__genmod
