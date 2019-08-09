        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:45 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE EVALTABULATEDFUNCTION__genmod
          INTERFACE 
            SUBROUTINE EVALTABULATEDFUNCTION(INVERSE,N,X,Y,DYDX,XE,YE,  &
     &DYEDXE)
              INTEGER(KIND=4) :: INVERSE
              INTEGER(KIND=4) :: N
              REAL(KIND=8) :: X(1000)
              REAL(KIND=8) :: Y(1000)
              REAL(KIND=8) :: DYDX(1000)
              REAL(KIND=8) :: XE
              REAL(KIND=8) :: YE
              REAL(KIND=8) :: DYEDXE
            END SUBROUTINE EVALTABULATEDFUNCTION
          END INTERFACE 
        END MODULE EVALTABULATEDFUNCTION__genmod
