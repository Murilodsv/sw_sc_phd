        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:08 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE SHRINK__genmod
          INTERFACE 
            FUNCTION SHRINK(SWSOILSHR,SWSHRINP,SHRPARA,SHRPARB,SHRPARC, &
     &SHRPARD,SHRPARE,THETA,THETAS)
              INTEGER(KIND=4) :: SWSOILSHR
              INTEGER(KIND=4) :: SWSHRINP
              REAL(KIND=8) :: SHRPARA
              REAL(KIND=8) :: SHRPARB
              REAL(KIND=8) :: SHRPARC
              REAL(KIND=8) :: SHRPARD
              REAL(KIND=8) :: SHRPARE
              REAL(KIND=8) :: THETA
              REAL(KIND=8) :: THETAS
              REAL(KIND=8) :: SHRINK
            END FUNCTION SHRINK
          END INTERFACE 
        END MODULE SHRINK__genmod
