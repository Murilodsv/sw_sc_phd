        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:14 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTWBA__genmod
          INTERFACE 
            SUBROUTINE OUTWBA(TASK,WBA,DAYNR,DAYCUM,SWSCRE,CEVAP,CGIRD, &
     &CGRAI,CSNRAI,CNIRD,CNRAI,CPEVA,CPTRA,CQBOT,CQDRA,CQROT,CRUNON,    &
     &CRUNOFF,GWL,CQMPOUTDRRAP,POND,T1900,DATE,VOLACT,VOLINI,WBALANCE,  &
     &OUTFIL,PATHWORK,PROJECT,FLPRINTSHORT,FLOUTPUT,SWSNOW,CQPRAI,SSNOW,&
     &SNOWINCO,PONDINI,FLHEADER)
              INTEGER(KIND=4) :: TASK
              INTEGER(KIND=4) :: WBA
              INTEGER(KIND=4) :: DAYNR
              INTEGER(KIND=4) :: DAYCUM
              INTEGER(KIND=4) :: SWSCRE
              REAL(KIND=8) :: CEVAP
              REAL(KIND=8) :: CGIRD
              REAL(KIND=8) :: CGRAI
              REAL(KIND=8) :: CSNRAI
              REAL(KIND=8) :: CNIRD
              REAL(KIND=8) :: CNRAI
              REAL(KIND=8) :: CPEVA
              REAL(KIND=8) :: CPTRA
              REAL(KIND=8) :: CQBOT
              REAL(KIND=8) :: CQDRA
              REAL(KIND=8) :: CQROT
              REAL(KIND=8) :: CRUNON
              REAL(KIND=8) :: CRUNOFF
              REAL(KIND=8) :: GWL
              REAL(KIND=8) :: CQMPOUTDRRAP
              REAL(KIND=8) :: POND
              REAL(KIND=8) :: T1900
              CHARACTER(LEN=11) :: DATE
              REAL(KIND=8) :: VOLACT
              REAL(KIND=8) :: VOLINI
              REAL(KIND=8) :: WBALANCE
              CHARACTER(LEN=16) :: OUTFIL
              CHARACTER(*) :: PATHWORK
              CHARACTER(LEN=80) :: PROJECT
              LOGICAL(KIND=4) :: FLPRINTSHORT
              LOGICAL(KIND=4) :: FLOUTPUT
              INTEGER(KIND=4) :: SWSNOW
              REAL(KIND=8) :: CQPRAI
              REAL(KIND=8) :: SSNOW
              REAL(KIND=8) :: SNOWINCO
              REAL(KIND=8) :: PONDINI
              LOGICAL(KIND=4) :: FLHEADER
            END SUBROUTINE OUTWBA
          END INTERFACE 
        END MODULE OUTWBA__genmod
