        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:14 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTINC__genmod
          INTERFACE 
            SUBROUTINE OUTINC(TASK,INC,DAYNR,DAYCUM,IGRAI,ISNRAI,IGSNOW,&
     &IGIRD,IINTC,IRUNON,IRUNO,IPTRA,IQROT,IPEVA,IEVAP,IQDRA,           &
     &IQMPOUTDRRAP,IQBOT,T1900,DATE,OUTFIL,PATHWORK,PROJECT,FLHEADER,GWL&
     &,FLPRINTSHORT)
              INTEGER(KIND=4) :: TASK
              INTEGER(KIND=4) :: INC
              INTEGER(KIND=4) :: DAYNR
              INTEGER(KIND=4) :: DAYCUM
              REAL(KIND=8) :: IGRAI
              REAL(KIND=8) :: ISNRAI
              REAL(KIND=8) :: IGSNOW
              REAL(KIND=8) :: IGIRD
              REAL(KIND=8) :: IINTC
              REAL(KIND=8) :: IRUNON
              REAL(KIND=8) :: IRUNO
              REAL(KIND=8) :: IPTRA
              REAL(KIND=8) :: IQROT
              REAL(KIND=8) :: IPEVA
              REAL(KIND=8) :: IEVAP
              REAL(KIND=8) :: IQDRA
              REAL(KIND=8) :: IQMPOUTDRRAP
              REAL(KIND=8) :: IQBOT
              REAL(KIND=8) :: T1900
              CHARACTER(LEN=11) :: DATE
              CHARACTER(LEN=16) :: OUTFIL
              CHARACTER(*) :: PATHWORK
              CHARACTER(LEN=80) :: PROJECT
              LOGICAL(KIND=4) :: FLHEADER
              REAL(KIND=8) :: GWL
              LOGICAL(KIND=4) :: FLPRINTSHORT
            END SUBROUTINE OUTINC
          END INTERFACE 
        END MODULE OUTINC__genmod
