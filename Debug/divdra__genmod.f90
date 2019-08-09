        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:38:05 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DIVDRA__genmod
          INTERFACE 
            SUBROUTINE DIVDRA(NUMCOMP,NUMDRAIN,THICKCOMP,KSATFIT,LAYER, &
     &COFANI,GWLEV,DISTDRAIN,FLUXDR,FLUXDRCOMP,SWDIVDINF,SWNRSRF,       &
     &SWTOPNRSRF,ZBOTDR,DT,FACDPTHINF,OWLTAB,T1900)
              INTEGER(KIND=4) :: NUMCOMP
              INTEGER(KIND=4) :: NUMDRAIN
              REAL(KIND=8) :: THICKCOMP(5000)
              REAL(KIND=8) :: KSATFIT(200)
              INTEGER(KIND=4) :: LAYER(5000)
              REAL(KIND=8) :: COFANI(200)
              REAL(KIND=8) :: GWLEV
              REAL(KIND=8) :: DISTDRAIN(5)
              REAL(KIND=8) :: FLUXDR(5)
              REAL(KIND=8) :: FLUXDRCOMP(5,5000)
              INTEGER(KIND=4) :: SWDIVDINF
              INTEGER(KIND=4) :: SWNRSRF
              INTEGER(KIND=4) :: SWTOPNRSRF
              REAL(KIND=8) :: ZBOTDR(5)
              REAL(KIND=8) :: DT
              REAL(KIND=8) :: FACDPTHINF
              REAL(KIND=8) :: OWLTAB(5,7320)
              REAL(KIND=8) :: T1900
            END SUBROUTINE DIVDRA
          END INTERFACE 
        END MODULE DIVDRA__genmod
