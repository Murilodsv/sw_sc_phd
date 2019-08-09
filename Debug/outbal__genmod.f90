        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:14 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTBAL__genmod
          INTERFACE 
            SUBROUTINE OUTBAL(TASK,BAL,LOGF,SWSCRE,SWDRA,NUMNOD,NRLEVS, &
     &SWSOLU,IOUTDAT,CEVAP,CGIRD,CGRAI,CQBOT,TSTART,CQROT,CRUNOFF,CRUNON&
     &,CQMPOUTDRRAP,CQDRA,CQDRAIN,DZ,Z,SAMINI,SAMPRO,SAMCRA,SQPREC,     &
     &SQIRRIG,SQBOT,DECTOT,ROTTOT,SQRAP,SQDRA,POND,VOLACT,VOLINI,T1900, &
     &OUTDAT,OUTFIL,PATHWORK,PROJECT,CAINTC,CSUBL,PONDINI,WASRDM1,      &
     &WASRDM2,WASRDM1INI,WASRDM2INI,SWSNOW,CGSNOW,CSNRAI,SNOWINCO,SSNOW)
              INTEGER(KIND=4) :: TASK
              INTEGER(KIND=4) :: BAL
              INTEGER(KIND=4) :: LOGF
              INTEGER(KIND=4) :: SWSCRE
              INTEGER(KIND=4) :: SWDRA
              INTEGER(KIND=4) :: NUMNOD
              INTEGER(KIND=4) :: NRLEVS
              INTEGER(KIND=4) :: SWSOLU
              INTEGER(KIND=4) :: IOUTDAT
              REAL(KIND=8) :: CEVAP
              REAL(KIND=8) :: CGIRD
              REAL(KIND=8) :: CGRAI
              REAL(KIND=8) :: CQBOT
              REAL(KIND=8) :: TSTART
              REAL(KIND=8) :: CQROT
              REAL(KIND=8) :: CRUNOFF
              REAL(KIND=8) :: CRUNON
              REAL(KIND=8) :: CQMPOUTDRRAP
              REAL(KIND=8) :: CQDRA
              REAL(KIND=8) :: CQDRAIN(5)
              REAL(KIND=8) :: DZ(5000)
              REAL(KIND=8) :: Z(5000)
              REAL(KIND=8) :: SAMINI
              REAL(KIND=8) :: SAMPRO
              REAL(KIND=8) :: SAMCRA
              REAL(KIND=8) :: SQPREC
              REAL(KIND=8) :: SQIRRIG
              REAL(KIND=8) :: SQBOT
              REAL(KIND=8) :: DECTOT
              REAL(KIND=8) :: ROTTOT
              REAL(KIND=8) :: SQRAP
              REAL(KIND=8) :: SQDRA
              REAL(KIND=8) :: POND
              REAL(KIND=8) :: VOLACT
              REAL(KIND=8) :: VOLINI
              REAL(KIND=8) :: T1900
              REAL(KIND=8) :: OUTDAT(3000)
              CHARACTER(LEN=16) :: OUTFIL
              CHARACTER(*) :: PATHWORK
              CHARACTER(LEN=80) :: PROJECT
              REAL(KIND=8) :: CAINTC
              REAL(KIND=8) :: CSUBL
              REAL(KIND=8) :: PONDINI
              REAL(KIND=8) :: WASRDM1
              REAL(KIND=8) :: WASRDM2
              REAL(KIND=8) :: WASRDM1INI
              REAL(KIND=8) :: WASRDM2INI
              INTEGER(KIND=4) :: SWSNOW
              REAL(KIND=8) :: CGSNOW
              REAL(KIND=8) :: CSNRAI
              REAL(KIND=8) :: SNOWINCO
              REAL(KIND=8) :: SSNOW
            END SUBROUTINE OUTBAL
          END INTERFACE 
        END MODULE OUTBAL__genmod
