        !COMPILER-GENERATED INTERFACE MODULE: Fri Aug 09 18:39:14 2019
        ! This source file is for reference only and may not completely
        ! represent the generated interface used by the compiler.
        MODULE OUTAFO__genmod
          INTERFACE 
            SUBROUTINE OUTAFO(TASK,AFO,OUTFIL,PATHWORK,NUMNOD,OUTPER,   &
     &PERIOD,POND,GWL,THETA,H,INQ,INQROT,NRLEVS,INQDRA,IEVAP,IPEVA,IPTRA&
     &,IRUNO,NUMLAY,BOTCOM,DZ,THETAS,COFGEN,SWAFO,IGRAI,IGIRD,INRAI,    &
     &INIRD,IINTC,GC,LAI,TAV,TSOIL,DAYCUM,RD,CF,WBALANCE,PROJECT,TSTART,&
     &TEND,SWDISCRVERT,NUMNODNEW,DZNEW,SWMACRO,SSNOW,IGSNOW,ISNRAI,ISUBL&
     &,IRUNON,FRARMTRX,IPONDBEG,ISSNOWBEG,ITHETABEG,VLMPSTDM1,VLMPSTDM2,&
     &DIPOCP,WALEVDM1,VLMPDM1,WASRDM1,VLMPDM2,WASRDM2,IQINTOPPREDM1,    &
     &IQINTOPLATDM1,INQEXCMTXDM1CP,INQOUTDRRAPCP,IAVFRMPWLWTDM1,        &
     &IQINTOPPREDM2,IQINTOPLATDM2,INQEXCMTXDM2CP,IAVFRMPWLWTDM2,        &
     &IWASRDM1BEG,IWASRDM2BEG,CRITDEVMASBAL,TCUM,NOD1LAY,SWSOPHY,NUMTAB,&
     &SPTAB)
              INTEGER(KIND=4) :: TASK
              INTEGER(KIND=4) :: AFO
              CHARACTER(*) :: OUTFIL
              CHARACTER(*) :: PATHWORK
              INTEGER(KIND=4) :: NUMNOD
              REAL(KIND=8) :: OUTPER
              INTEGER(KIND=4) :: PERIOD
              REAL(KIND=8) :: POND
              REAL(KIND=8) :: GWL
              REAL(KIND=8) :: THETA(5000)
              REAL(KIND=8) :: H(5000)
              REAL(KIND=8) :: INQ(5001)
              REAL(KIND=8) :: INQROT(5000)
              INTEGER(KIND=4) :: NRLEVS
              REAL(KIND=8) :: INQDRA(5,5000)
              REAL(KIND=8) :: IEVAP
              REAL(KIND=8) :: IPEVA
              REAL(KIND=8) :: IPTRA
              REAL(KIND=8) :: IRUNO
              INTEGER(KIND=4) :: NUMLAY
              INTEGER(KIND=4) :: BOTCOM(200)
              REAL(KIND=8) :: DZ(5000)
              REAL(KIND=8) :: THETAS(5000)
              REAL(KIND=8) :: COFGEN(12,5000)
              INTEGER(KIND=4) :: SWAFO
              REAL(KIND=8) :: IGRAI
              REAL(KIND=8) :: IGIRD
              REAL(KIND=8) :: INRAI
              REAL(KIND=8) :: INIRD
              REAL(KIND=8) :: IINTC
              REAL(KIND=8) :: GC
              REAL(KIND=8) :: LAI
              REAL(KIND=8) :: TAV
              REAL(KIND=8) :: TSOIL(5000)
              INTEGER(KIND=4) :: DAYCUM
              REAL(KIND=8) :: RD
              REAL(KIND=8) :: CF
              REAL(KIND=8) :: WBALANCE
              CHARACTER(LEN=80) :: PROJECT
              REAL(KIND=8) :: TSTART
              REAL(KIND=8) :: TEND
              INTEGER(KIND=4) :: SWDISCRVERT
              INTEGER(KIND=4) :: NUMNODNEW
              REAL(KIND=8) :: DZNEW(5000)
              INTEGER(KIND=4) :: SWMACRO
              REAL(KIND=8) :: SSNOW
              REAL(KIND=8) :: IGSNOW
              REAL(KIND=8) :: ISNRAI
              REAL(KIND=8) :: ISUBL
              REAL(KIND=8) :: IRUNON
              REAL(KIND=8) :: FRARMTRX(5000)
              REAL(KIND=8) :: IPONDBEG
              REAL(KIND=8) :: ISSNOWBEG
              REAL(KIND=8) :: ITHETABEG(5000)
              REAL(KIND=8) :: VLMPSTDM1(5000)
              REAL(KIND=8) :: VLMPSTDM2(5000)
              REAL(KIND=8) :: DIPOCP(5000)
              REAL(KIND=8) :: WALEVDM1
              REAL(KIND=8) :: VLMPDM1
              REAL(KIND=8) :: WASRDM1
              REAL(KIND=8) :: VLMPDM2
              REAL(KIND=8) :: WASRDM2
              REAL(KIND=8) :: IQINTOPPREDM1
              REAL(KIND=8) :: IQINTOPLATDM1
              REAL(KIND=8) :: INQEXCMTXDM1CP(5000)
              REAL(KIND=8) :: INQOUTDRRAPCP(5000)
              REAL(KIND=8) :: IAVFRMPWLWTDM1(5000)
              REAL(KIND=8) :: IQINTOPPREDM2
              REAL(KIND=8) :: IQINTOPLATDM2
              REAL(KIND=8) :: INQEXCMTXDM2CP(5000)
              REAL(KIND=8) :: IAVFRMPWLWTDM2(5000)
              REAL(KIND=8) :: IWASRDM1BEG
              REAL(KIND=8) :: IWASRDM2BEG
              REAL(KIND=8) :: CRITDEVMASBAL
              REAL(KIND=8) :: TCUM
              INTEGER(KIND=4) :: NOD1LAY(200)
              INTEGER(KIND=4) :: SWSOPHY
              INTEGER(KIND=4) :: NUMTAB(5000)
              REAL(KIND=8) :: SPTAB(5,5000,1000)
            END SUBROUTINE OUTAFO
          END INTERFACE 
        END MODULE OUTAFO__genmod
