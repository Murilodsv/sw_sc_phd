        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:13 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE HYSTERESIS__genmod
          INTERFACE 
            SUBROUTINE HYSTERESIS(NUMNOD,LAYER,H,HM1,INDEKS,TAU,PARAMVG,&
     &COFGEN,DIMOCA,THETA,DISNOD,DT,SWSOPHY,NUMTAB,SPTAB)
              INTEGER(KIND=4) :: NUMNOD
              INTEGER(KIND=4) :: LAYER(5000)
              REAL(KIND=8) :: H(5000)
              REAL(KIND=8) :: HM1(5000)
              INTEGER(KIND=4) :: INDEKS(5000)
              REAL(KIND=8) :: TAU
              REAL(KIND=8) :: PARAMVG(10,200)
              REAL(KIND=8) :: COFGEN(12,5000)
              REAL(KIND=8) :: DIMOCA(5000)
              REAL(KIND=8) :: THETA(5000)
              REAL(KIND=8) :: DISNOD(5001)
              REAL(KIND=8) :: DT
              INTEGER(KIND=4) :: SWSOPHY
              INTEGER(KIND=4) :: NUMTAB(5000)
              REAL(KIND=8) :: SPTAB(5,5000,1000)
            END SUBROUTINE HYSTERESIS
          END INTERFACE 
        END MODULE HYSTERESIS__genmod
