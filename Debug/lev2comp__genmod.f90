        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:04 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LEV2COMP__genmod
          INTERFACE 
            SUBROUTINE LEV2COMP(LEVEL,THICKCOMP,NUMCOM2LEV,THICKCUM,    &
     &THICKCOMPABVLEV,THICKCOMPBLWLEV)
              REAL(KIND=8) :: LEVEL
              REAL(KIND=8) :: THICKCOMP(5000)
              INTEGER(KIND=4) :: NUMCOM2LEV
              REAL(KIND=8) :: THICKCUM
              REAL(KIND=8) :: THICKCOMPABVLEV
              REAL(KIND=8) :: THICKCOMPBLWLEV
            END SUBROUTINE LEV2COMP
          END INTERFACE 
        END MODULE LEV2COMP__genmod
