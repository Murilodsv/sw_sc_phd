        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:14 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTBLC__genmod
          INTERFACE 
            SUBROUTINE OUTBLC(TASK,BLC,OUTFIL,PATHWORK,CGRAI,CNRAI,CGIRD&
     &,CNIRD,CQROT,CEVAP,VOLACT,VOLINI,Z,DZ,SWMACRO,NRLEVS,SWIRFIX,     &
     &SCHEDULE,NUMNOD,SNOWINCO,SSNOW,CGSNO,CMELT,CAINTC,CSNRAI,CQPRAI,  &
     &IOUTDAT,T1900,OUTDAT,TSTART,PROJECT,POND,PONDINI,CQDRAININ,       &
     &CQDRAINOUT,CINUND,CRUNOFF,CQTDO,CQTUP,CQBOTDO,CQBOTUP,CSUBL,CRUNON&
     &,CQMPEXFMTX,CQMPINFMTX,CQMPINTOPPREDM1,CQMPINTOPPREDM2,           &
     &CQMPINTOPLATDM1,CQMPINTOPLATDM2)
              INTEGER(KIND=4) :: NRLEVS
              INTEGER(KIND=4) :: TASK
              INTEGER(KIND=4) :: BLC
              CHARACTER(LEN=16) :: OUTFIL
              CHARACTER(LEN=80) :: PATHWORK
              REAL(KIND=8) :: CGRAI
              REAL(KIND=8) :: CNRAI
              REAL(KIND=8) :: CGIRD
              REAL(KIND=8) :: CNIRD
              REAL(KIND=8) :: CQROT
              REAL(KIND=8) :: CEVAP
              REAL(KIND=8) :: VOLACT
              REAL(KIND=8) :: VOLINI
              REAL(KIND=8) :: Z(5000)
              REAL(KIND=8) :: DZ(5000)
              INTEGER(KIND=4) :: SWMACRO
              INTEGER(KIND=4) :: SWIRFIX
              INTEGER(KIND=4) :: SCHEDULE
              INTEGER(KIND=4) :: NUMNOD
              REAL(KIND=8) :: SNOWINCO
              REAL(KIND=8) :: SSNOW
              REAL(KIND=8) :: CGSNO
              REAL(KIND=8) :: CMELT
              REAL(KIND=8) :: CAINTC
              REAL(KIND=8) :: CSNRAI
              REAL(KIND=8) :: CQPRAI
              INTEGER(KIND=4) :: IOUTDAT
              REAL(KIND=8) :: T1900
              REAL(KIND=8) :: OUTDAT(3000)
              REAL(KIND=8) :: TSTART
              CHARACTER(LEN=80) :: PROJECT
              REAL(KIND=8) :: POND
              REAL(KIND=8) :: PONDINI
              REAL(KIND=8) :: CQDRAININ(NRLEVS)
              REAL(KIND=8) :: CQDRAINOUT(NRLEVS)
              REAL(KIND=8) :: CINUND
              REAL(KIND=8) :: CRUNOFF
              REAL(KIND=8) :: CQTDO
              REAL(KIND=8) :: CQTUP
              REAL(KIND=8) :: CQBOTDO
              REAL(KIND=8) :: CQBOTUP
              REAL(KIND=8) :: CSUBL
              REAL(KIND=8) :: CRUNON
              REAL(KIND=8) :: CQMPEXFMTX
              REAL(KIND=8) :: CQMPINFMTX
              REAL(KIND=8) :: CQMPINTOPPREDM1
              REAL(KIND=8) :: CQMPINTOPPREDM2
              REAL(KIND=8) :: CQMPINTOPLATDM1
              REAL(KIND=8) :: CQMPINTOPLATDM2
            END SUBROUTINE OUTBLC
          END INTERFACE 
        END MODULE OUTBLC__genmod
