        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:55 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE LAIS__genmod
          INTERFACE 
            SUBROUTINE LAIS(DIAC,DI,PHYLOC,NSTK,SWFACE,STK_AGEFAC,LN,   &
     &MAXGL,CUMLA,DDEALLA,MLA,SLA,CUMLW,DDEALLW,DNLEAF,DLA,DLEAFDM,DW,  &
     &LGPF,DNETBIOMASS,DNODEN,DDEADLN,DEVGL,STK_AGE,LFAGE,LFAREA,       &
     &LFWEIGHT,DNSTK,STK_DNETBIOMASS,INIT_NLF,LFSHP,MAXDEVGL)
              REAL(KIND=4) :: DIAC
              REAL(KIND=4) :: DI
              REAL(KIND=4) :: PHYLOC
              REAL(KIND=4) :: NSTK
              REAL(KIND=4) :: SWFACE
              REAL(KIND=4) :: STK_AGEFAC
              REAL(KIND=4) :: LN
              REAL(KIND=4) :: MAXGL
              REAL(KIND=4) :: CUMLA(150)
              REAL(KIND=4) :: DDEALLA
              REAL(KIND=4) :: MLA
              REAL(KIND=4) :: SLA
              REAL(KIND=4) :: CUMLW(150)
              REAL(KIND=4) :: DDEALLW
              REAL(KIND=4) :: DNLEAF
              REAL(KIND=4) :: DLA
              REAL(KIND=4) :: DLEAFDM
              REAL(KIND=4) :: DW
              REAL(KIND=4) :: LGPF
              REAL(KIND=4) :: DNETBIOMASS
              REAL(KIND=4) :: DNODEN
              REAL(KIND=4) :: DDEADLN
              REAL(KIND=4) :: DEVGL
              REAL(KIND=4) :: STK_AGE(60,4)
              REAL(KIND=4) :: LFAGE(60,20)
              REAL(KIND=4) :: LFAREA(60,20)
              REAL(KIND=4) :: LFWEIGHT(60,20)
              REAL(KIND=4) :: DNSTK
              REAL(KIND=4) :: STK_DNETBIOMASS(60)
              INTEGER(KIND=4) :: INIT_NLF
              REAL(KIND=4) :: LFSHP
              INTEGER(KIND=4) :: MAXDEVGL
            END SUBROUTINE LAIS
          END INTERFACE 
        END MODULE LAIS__genmod
