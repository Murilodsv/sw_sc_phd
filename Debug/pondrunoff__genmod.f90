        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:07 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PONDRUNOFF__genmod
          INTERFACE 
            SUBROUTINE PONDRUNOFF(SWDRA,SWMACRO,FLRUNOFF,DISNOD,DT,H,   &
     &H0MAX,K1MAX,PONDM1,PONDMX,Q0,RSRO,RSROEXP,STTAB,WLS,SWST,QMPLATSS,&
     &HSURF,POND,RUNOTS,SWPONDMX,PONDMXTAB,T1900)
              INTEGER(KIND=4) :: SWDRA
              INTEGER(KIND=4) :: SWMACRO
              LOGICAL(KIND=4) :: FLRUNOFF
              REAL(KIND=8) :: DISNOD(5001)
              REAL(KIND=8) :: DT
              REAL(KIND=8) :: H(5000)
              REAL(KIND=8) :: H0MAX
              REAL(KIND=8) :: K1MAX
              REAL(KIND=8) :: PONDM1
              REAL(KIND=8) :: PONDMX
              REAL(KIND=8) :: Q0
              REAL(KIND=8) :: RSRO
              REAL(KIND=8) :: RSROEXP
              REAL(KIND=8) :: STTAB(22,2)
              REAL(KIND=8) :: WLS
              REAL(KIND=8) :: SWST
              REAL(KIND=8) :: QMPLATSS
              REAL(KIND=8) :: HSURF
              REAL(KIND=8) :: POND
              REAL(KIND=8) :: RUNOTS
              INTEGER(KIND=4) :: SWPONDMX
              REAL(KIND=8) :: PONDMXTAB(20000)
              REAL(KIND=8) :: T1900
            END SUBROUTINE PONDRUNOFF
          END INTERFACE 
        END MODULE PONDRUNOFF__genmod
