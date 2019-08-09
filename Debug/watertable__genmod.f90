        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:01 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE WATERTABLE__genmod
          INTERFACE 
            SUBROUTINE WATERTABLE(NODE,NODLEV,NODHLP,FLSAT,CRITUNDSATVOL&
     &,DZ,H,THETA,THETAS,Z,WATERLEVEL)
              INTEGER(KIND=4) :: NODE
              INTEGER(KIND=4) :: NODLEV
              INTEGER(KIND=4) :: NODHLP
              LOGICAL(KIND=4) :: FLSAT
              REAL(KIND=8) :: CRITUNDSATVOL
              REAL(KIND=8) :: DZ(5000)
              REAL(KIND=8) :: H(5000)
              REAL(KIND=8) :: THETA(5000)
              REAL(KIND=8) :: THETAS(5000)
              REAL(KIND=8) :: Z(5000)
              REAL(KIND=8) :: WATERLEVEL
            END SUBROUTINE WATERTABLE
          END INTERFACE 
        END MODULE WATERTABLE__genmod
