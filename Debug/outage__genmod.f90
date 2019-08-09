        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:14 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTAGE__genmod
          INTERFACE 
            SUBROUTINE OUTAGE(TASK,AGEP,AGEE,AGEQ,DAYNR,DAYCUM,DATE,    &
     &OUTPER,PROJECT,NRLEVS,OUTFIL,PATHWORK,NUMNOD,Z,CML,AGEGWL1M,      &
     &ICAGEBOT,ICAGEDRA,ICAGEROT,ICAGESUR,INQDRA)
              INTEGER(KIND=4) :: TASK
              INTEGER(KIND=4) :: AGEP
              INTEGER(KIND=4) :: AGEE
              INTEGER(KIND=4) :: AGEQ
              INTEGER(KIND=4) :: DAYNR
              INTEGER(KIND=4) :: DAYCUM
              CHARACTER(LEN=11) :: DATE
              REAL(KIND=8) :: OUTPER
              CHARACTER(LEN=80) :: PROJECT
              INTEGER(KIND=4) :: NRLEVS
              CHARACTER(LEN=16) :: OUTFIL
              CHARACTER(LEN=80) :: PATHWORK
              INTEGER(KIND=4) :: NUMNOD
              REAL(KIND=8) :: Z(5000)
              REAL(KIND=8) :: CML(5000)
              REAL(KIND=8) :: AGEGWL1M
              REAL(KIND=8) :: ICAGEBOT
              REAL(KIND=8) :: ICAGEDRA(5)
              REAL(KIND=8) :: ICAGEROT
              REAL(KIND=8) :: ICAGESUR
              REAL(KIND=8) :: INQDRA(5,5000)
            END SUBROUTINE OUTAGE
          END INTERFACE 
        END MODULE OUTAGE__genmod
