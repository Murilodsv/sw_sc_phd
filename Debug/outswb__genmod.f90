        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:14 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTSWB__genmod
          INTERFACE 
            SUBROUTINE OUTSWB(TASK,OUTFIL,VTAIR,PATHWORK,DAYNR,DAYCUM,  &
     &HBWEIR,OVERFL,GWL,POND,WLSTAR,WLS,SWSTINI,SWST,CQDRD,CRUNOFF,     &
     &CQMPOUTDRRAP,CWSUPP,SWB,NUMADJ,HWLMAN,CWOUT,SWSEC,SWMAN,NMPER,    &
     &IMPEND,IMPER,PROJECT,LOGF,SWSCRE,DATE,T1900,T,OUTPER,IYEAR)
              INTEGER(KIND=4) :: NMPER
              INTEGER(KIND=4) :: TASK
              CHARACTER(LEN=16) :: OUTFIL
              REAL(KIND=8) :: VTAIR
              CHARACTER(LEN=80) :: PATHWORK
              INTEGER(KIND=4) :: DAYNR
              INTEGER(KIND=4) :: DAYCUM
              REAL(KIND=8) :: HBWEIR(NMPER)
              LOGICAL(KIND=4) :: OVERFL
              REAL(KIND=8) :: GWL
              REAL(KIND=8) :: POND
              REAL(KIND=8) :: WLSTAR
              REAL(KIND=8) :: WLS
              REAL(KIND=8) :: SWSTINI
              REAL(KIND=8) :: SWST
              REAL(KIND=8) :: CQDRD
              REAL(KIND=8) :: CRUNOFF
              REAL(KIND=8) :: CQMPOUTDRRAP
              REAL(KIND=8) :: CWSUPP
              INTEGER(KIND=4) :: SWB
              INTEGER(KIND=4) :: NUMADJ
              REAL(KIND=8) :: HWLMAN
              REAL(KIND=8) :: CWOUT
              INTEGER(KIND=4) :: SWSEC
              INTEGER(KIND=4) :: SWMAN(3660)
              REAL(KIND=8) :: IMPEND(3660)
              INTEGER(KIND=4) :: IMPER
              CHARACTER(LEN=80) :: PROJECT
              INTEGER(KIND=4) :: LOGF
              INTEGER(KIND=4) :: SWSCRE
              CHARACTER(LEN=11) :: DATE
              REAL(KIND=8) :: T1900
              REAL(KIND=8) :: T
              REAL(KIND=8) :: OUTPER
              INTEGER(KIND=4) :: IYEAR
            END SUBROUTINE OUTSWB
          END INTERFACE 
        END MODULE OUTSWB__genmod
