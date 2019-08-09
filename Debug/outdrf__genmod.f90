        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:14 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTDRF__genmod
          INTERFACE 
            SUBROUTINE OUTDRF(TASK,OUTFIL,DRF,PATHWORK,DAYNR,DATE,NRPRI,&
     &NRLEVS,CQDRAIN,CQDRD,CRUNOFF,CQMPOUTDRRAP,FLHEADER)
              INTEGER(KIND=4) :: TASK
              CHARACTER(LEN=16) :: OUTFIL
              INTEGER(KIND=4) :: DRF
              CHARACTER(LEN=80) :: PATHWORK
              INTEGER(KIND=4) :: DAYNR
              CHARACTER(LEN=11) :: DATE
              INTEGER(KIND=4) :: NRPRI
              INTEGER(KIND=4) :: NRLEVS
              REAL(KIND=8) :: CQDRAIN(5)
              REAL(KIND=8) :: CQDRD
              REAL(KIND=8) :: CRUNOFF
              REAL(KIND=8) :: CQMPOUTDRRAP
              LOGICAL(KIND=4) :: FLHEADER
            END SUBROUTINE OUTDRF
          END INTERFACE 
        END MODULE OUTDRF__genmod
