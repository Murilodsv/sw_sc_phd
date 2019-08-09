        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:14 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTSTR__genmod
          INTERFACE 
            SUBROUTINE OUTSTR(TASK,STR,DAYNR,DAYCUM,IES0,IET0,IEW0,IPEVA&
     &,IPTRA,IQROT,IQREDWET,IQREDDRY,IQREDWSOL,IQREDFRS,T1900,DATE,     &
     &OUTFIL,PATHWORK,PROJECT,FLHEADER,FLPRINTSHORT)
              INTEGER(KIND=4) :: TASK
              INTEGER(KIND=4) :: STR
              INTEGER(KIND=4) :: DAYNR
              INTEGER(KIND=4) :: DAYCUM
              REAL(KIND=8) :: IES0
              REAL(KIND=8) :: IET0
              REAL(KIND=8) :: IEW0
              REAL(KIND=8) :: IPEVA
              REAL(KIND=8) :: IPTRA
              REAL(KIND=8) :: IQROT
              REAL(KIND=8) :: IQREDWET
              REAL(KIND=8) :: IQREDDRY
              REAL(KIND=8) :: IQREDWSOL
              REAL(KIND=8) :: IQREDFRS
              REAL(KIND=8) :: T1900
              CHARACTER(LEN=11) :: DATE
              CHARACTER(LEN=16) :: OUTFIL
              CHARACTER(*) :: PATHWORK
              CHARACTER(LEN=80) :: PROJECT
              LOGICAL(KIND=4) :: FLHEADER
              LOGICAL(KIND=4) :: FLPRINTSHORT
            END SUBROUTINE OUTSTR
          END INTERFACE 
        END MODULE OUTSTR__genmod
