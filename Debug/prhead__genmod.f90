        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:33 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE PRHEAD__genmod
          INTERFACE 
            FUNCTION PRHEAD(NODE,DISNOD,COFGEN,WCON,H,SWSOPHY,NUMTAB,   &
     &SPTAB)
              INTEGER(KIND=4) :: NODE
              REAL(KIND=8) :: DISNOD
              REAL(KIND=8) :: COFGEN(12,5000)
              REAL(KIND=8) :: WCON
              REAL(KIND=8) :: H(5000)
              INTEGER(KIND=4) :: SWSOPHY
              INTEGER(KIND=4) :: NUMTAB(5000)
              REAL(KIND=8) :: SPTAB(5,5000,1000)
              REAL(KIND=8) :: PRHEAD
            END FUNCTION PRHEAD
          END INTERFACE 
        END MODULE PRHEAD__genmod
