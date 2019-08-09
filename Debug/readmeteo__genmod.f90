        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:10 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE READMETEO__genmod
          INTERFACE 
            SUBROUTINE READMETEO(LOGF,SWETR,SWMETEO,SWCALT,SWRAIN,SWSNOW&
     &,SWFROST,YEARMETEO,PATHATM,METFIL,DAYNRFIRST,DAYNRLAST,ARAD,ATMN, &
     &ATMX,AHUM,AWIN,ARAI,AETR,WET,RAINTAB)
              INTEGER(KIND=4) :: LOGF
              INTEGER(KIND=4) :: SWETR
              INTEGER(KIND=4) :: SWMETEO
              INTEGER(KIND=4) :: SWCALT
              INTEGER(KIND=4) :: SWRAIN
              INTEGER(KIND=4) :: SWSNOW
              INTEGER(KIND=4) :: SWFROST
              INTEGER(KIND=4) :: YEARMETEO
              CHARACTER(*) :: PATHATM
              CHARACTER(*) :: METFIL
              INTEGER(KIND=4) :: DAYNRFIRST
              INTEGER(KIND=4) :: DAYNRLAST
              REAL(KIND=8) :: ARAD(366)
              REAL(KIND=8) :: ATMN(366)
              REAL(KIND=8) :: ATMX(366)
              REAL(KIND=8) :: AHUM(366)
              REAL(KIND=8) :: AWIN(366)
              REAL(KIND=8) :: ARAI(366)
              REAL(KIND=8) :: AETR(366)
              REAL(KIND=8) :: WET(366)
              REAL(KIND=8) :: RAINTAB(60)
            END SUBROUTINE READMETEO
          END INTERFACE 
        END MODULE READMETEO__genmod
