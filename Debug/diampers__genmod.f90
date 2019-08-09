        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:40:55 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE DIAMPERS__genmod
          INTERFACE 
            SUBROUTINE DIAMPERS(THOUR,TBI,TOPT,DPERCOEFF,SWFACE,        &
     &AGEFACTOR,NSTK,PLENG,NODEN,SUCMAX,SUCACCFAC,DWS,DI,DIAC,PHYLOC,DW,&
     &SGPF,AGE_STKEMER,DPER,DSUC,DFIB,SUCEXCEEDED,INTERNODE,            &
     &STK_DNETBIOMASS,DNETBIOMASS,DSTKDM,STK_ITN,STK_AGE,ITAGE,ITPF,    &
     &ITLEN,ITSUC,ITFIB,ITTDW,NGRNODES,ITSHP,SUCSHP,MAXITDW,FLSINK_FBRES&
     &,DRGP_PG)
              REAL(KIND=4) :: THOUR(24)
              REAL(KIND=4) :: TBI
              REAL(KIND=4) :: TOPT
              REAL(KIND=4) :: DPERCOEFF
              REAL(KIND=4) :: SWFACE
              REAL(KIND=4) :: AGEFACTOR
              REAL(KIND=4) :: NSTK
              REAL(KIND=4) :: PLENG
              INTEGER(KIND=4) :: NODEN
              REAL(KIND=4) :: SUCMAX
              INTEGER(KIND=4) :: SUCACCFAC
              REAL(KIND=4) :: DWS
              REAL(KIND=4) :: DI
              REAL(KIND=4) :: DIAC
              REAL(KIND=4) :: PHYLOC
              REAL(KIND=4) :: DW
              REAL(KIND=4) :: SGPF
              REAL(KIND=4) :: AGE_STKEMER
              REAL(KIND=4) :: DPER
              REAL(KIND=4) :: DSUC
              REAL(KIND=4) :: DFIB
              REAL(KIND=4) :: SUCEXCEEDED
              REAL(KIND=4) :: INTERNODE(100,6)
              REAL(KIND=4) :: STK_DNETBIOMASS(60)
              REAL(KIND=4) :: DNETBIOMASS
              REAL(KIND=4) :: DSTKDM
              REAL(KIND=4) :: STK_ITN(60)
              REAL(KIND=4) :: STK_AGE(60,4)
              REAL(KIND=4) :: ITAGE(60,60)
              REAL(KIND=4) :: ITPF(60,60)
              REAL(KIND=4) :: ITLEN(60,60)
              REAL(KIND=4) :: ITSUC(60,60)
              REAL(KIND=4) :: ITFIB(60,60)
              REAL(KIND=4) :: ITTDW(60,60)
              INTEGER(KIND=4) :: NGRNODES
              REAL(KIND=4) :: ITSHP
              REAL(KIND=4) :: SUCSHP
              REAL(KIND=4) :: MAXITDW
              LOGICAL(KIND=4) :: FLSINK_FBRES
              REAL(KIND=4) :: DRGP_PG
            END SUBROUTINE DIAMPERS
          END INTERFACE 
        END MODULE DIAMPERS__genmod
