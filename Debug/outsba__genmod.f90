        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:14 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTSBA__genmod
          INTERFACE 
            SUBROUTINE OUTSBA(TASK,SBA,DAYNR,DAYCUM,SAMPRO,SQBOT,PROJECT&
     &,SQDRA,SOLBAL,DECTOT,ROTTOT,SQPREC,DATE,SQIRRIG,OUTFIL,PATHWORK,  &
     &FLHEADER)
              INTEGER(KIND=4) :: TASK
              INTEGER(KIND=4) :: SBA
              INTEGER(KIND=4) :: DAYNR
              INTEGER(KIND=4) :: DAYCUM
              REAL(KIND=8) :: SAMPRO
              REAL(KIND=8) :: SQBOT
              CHARACTER(LEN=80) :: PROJECT
              REAL(KIND=8) :: SQDRA
              REAL(KIND=8) :: SOLBAL
              REAL(KIND=8) :: DECTOT
              REAL(KIND=8) :: ROTTOT
              REAL(KIND=8) :: SQPREC
              CHARACTER(LEN=11) :: DATE
              REAL(KIND=8) :: SQIRRIG
              CHARACTER(LEN=16) :: OUTFIL
              CHARACTER(LEN=80) :: PATHWORK
              LOGICAL(KIND=4) :: FLHEADER
            END SUBROUTINE OUTSBA
          END INTERFACE 
        END MODULE OUTSBA__genmod
