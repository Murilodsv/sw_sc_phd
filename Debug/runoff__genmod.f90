        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:33 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE RUNOFF__genmod
          INTERFACE 
            FUNCTION RUNOFF(SWDRA,POND,PONDMX,RSRO,RSROEXP,WLS,SWST,    &
     &STTAB,DT)
              INTEGER(KIND=4) :: SWDRA
              REAL(KIND=8) :: POND
              REAL(KIND=8) :: PONDMX
              REAL(KIND=8) :: RSRO
              REAL(KIND=8) :: RSROEXP
              REAL(KIND=8) :: WLS
              REAL(KIND=8) :: SWST
              REAL(KIND=8) :: STTAB(22,2)
              REAL(KIND=8) :: DT
              REAL(KIND=8) :: RUNOFF
            END FUNCTION RUNOFF
          END INTERFACE 
        END MODULE RUNOFF__genmod
