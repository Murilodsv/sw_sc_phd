        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:14 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTTEM__genmod
          INTERFACE 
            SUBROUTINE OUTTEM(TASK,NUMNOD,DATE,DAYNR,TEM,DAYCUM,TAV,    &
     &TEBOT,TSOIL,TETOP,OUTFIL,PATHWORK,FLHEADER,PROJECT)
              INTEGER(KIND=4) :: TASK
              INTEGER(KIND=4) :: NUMNOD
              CHARACTER(LEN=11) :: DATE
              INTEGER(KIND=4) :: DAYNR
              INTEGER(KIND=4) :: TEM
              INTEGER(KIND=4) :: DAYCUM
              REAL(KIND=8) :: TAV
              REAL(KIND=8) :: TEBOT
              REAL(KIND=8) :: TSOIL(5000)
              REAL(KIND=8) :: TETOP
              CHARACTER(LEN=16) :: OUTFIL
              CHARACTER(LEN=80) :: PATHWORK
              LOGICAL(KIND=4) :: FLHEADER
              CHARACTER(LEN=80) :: PROJECT
            END SUBROUTINE OUTTEM
          END INTERFACE 
        END MODULE OUTTEM__genmod
