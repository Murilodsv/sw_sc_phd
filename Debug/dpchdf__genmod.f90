        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:45 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DPCHDF__genmod
          INTERFACE 
            FUNCTION DPCHDF(K,X,S,IERR)
              INTEGER(KIND=4) :: K
              REAL(KIND=8) :: X(K)
              REAL(KIND=8) :: S(K)
              INTEGER(KIND=4) :: IERR
              REAL(KIND=8) :: DPCHDF
            END FUNCTION DPCHDF
          END INTERFACE 
        END MODULE DPCHDF__genmod
