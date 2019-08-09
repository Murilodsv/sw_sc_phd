        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:55 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE ASTRO__genmod
          INTERFACE 
            SUBROUTINE ASTRO(LOGF,SWSCRE,DAYNR,LAT,DAYL,DAYLP,SINLD,    &
     &COSLD,DSINB,DSINBE,DSO)
              INTEGER(KIND=4) :: LOGF
              INTEGER(KIND=4) :: SWSCRE
              INTEGER(KIND=4) :: DAYNR
              REAL(KIND=8) :: LAT
              REAL(KIND=8) :: DAYL
              REAL(KIND=8) :: DAYLP
              REAL(KIND=8) :: SINLD
              REAL(KIND=8) :: COSLD
              REAL(KIND=8) :: DSINB
              REAL(KIND=8) :: DSINBE
              REAL(KIND=8) :: DSO
            END SUBROUTINE ASTRO
          END INTERFACE 
        END MODULE ASTRO__genmod
