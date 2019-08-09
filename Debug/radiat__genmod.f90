        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:55 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RADIAT__genmod
          INTERFACE 
            SUBROUTINE RADIAT(DAYNR,HOUR,DAYL,SINLD,COSLD,AVRAD,SINB,   &
     &PARDIR,PARDIF)
              INTEGER(KIND=4) :: DAYNR
              REAL(KIND=8) :: HOUR
              REAL(KIND=8) :: DAYL
              REAL(KIND=8) :: SINLD
              REAL(KIND=8) :: COSLD
              REAL(KIND=8) :: AVRAD
              REAL(KIND=8) :: SINB
              REAL(KIND=8) :: PARDIR
              REAL(KIND=8) :: PARDIF
            END SUBROUTINE RADIAT
          END INTERFACE 
        END MODULE RADIAT__genmod
