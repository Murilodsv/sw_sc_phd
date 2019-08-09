        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:10 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE READRAIN__genmod
          INTERFACE 
            SUBROUTINE READRAIN(CALLROUTINE,LOGF,PATHATM,RAINFIL,       &
     &YEARMETEO,TCUM,TEND,TSTART,RAINAMOUNT,RAINFLUXARRAY,RAINTIMEARRAY)
              CHARACTER(*) :: CALLROUTINE
              INTEGER(KIND=4) :: LOGF
              CHARACTER(*) :: PATHATM
              CHARACTER(*) :: RAINFIL
              INTEGER(KIND=4) :: YEARMETEO
              REAL(KIND=8) :: TCUM
              REAL(KIND=8) :: TEND
              REAL(KIND=8) :: TSTART
              REAL(KIND=8) :: RAINAMOUNT(40000)
              REAL(KIND=8) :: RAINFLUXARRAY(40000)
              REAL(KIND=8) :: RAINTIMEARRAY(40000)
            END SUBROUTINE READRAIN
          END INTERFACE 
        END MODULE READRAIN__genmod
