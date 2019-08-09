        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:12 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE CHECKDATE__genmod
          INTERFACE 
            SUBROUTINE CHECKDATE(IFND,DATES,TEND,TSTART,NAMEDAT,TOPIC)
              INTEGER(KIND=4) :: IFND
              REAL(KIND=8) :: DATES(IFND)
              REAL(KIND=8) :: TEND
              REAL(KIND=8) :: TSTART
              CHARACTER(LEN=5) :: NAMEDAT
              CHARACTER(*) :: TOPIC
            END SUBROUTINE CHECKDATE
          END INTERFACE 
        END MODULE CHECKDATE__genmod
